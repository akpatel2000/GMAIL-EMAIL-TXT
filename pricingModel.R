set.seed(2018)

library(XML)
library(lubridate)
library(timeDate)
library(tis)
library(RMySQL)
library(dbConnect)
library(httr)
library(ggplot2)
library(dplyr)

Sys.setenv(TZ='EST')


## read dealerOffer csv file
setwd("~/Desktop/test")
dealerOffers <- read.csv("dealerOffers.csv", stringsAsFactors = FALSE)

## read database file
# open database connection
db1 <- dbConnect(MySQL(), user= "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
historicalRates <- dbReadTable(db1, name = "rates")
numberRows <- nrow(historicalRates)
dbDisconnect(db1)

historicalRates$date <- as.Date(historicalRates$date)


# need to process date fields
dealerOffers$mty <- as.Date(dealerOffers$mty)
dealerOffers$nxtCall <- as.Date(dealerOffers$nxtCall)
dealerOffers$date <- as.Date(dealerOffers$date)

# add years to maturity and next call based of date 
dealerOffers <- dealerOffers %>% mutate(yearsToMat = as.numeric(mty - date)/365)
dealerOffers <- dealerOffers %>% mutate(yearsToNxtCall = as.numeric(nxtCall - date)/365)

# This model is not going to look at bond that are less than 2 years maturity
dealerOffers <- dealerOffers %>% filter(yearsToMat >= 2)

# for simplicity the model excludes calls less than 7 years 
# future enhancements can included better test to remove kicker bonds.
dealerOffers <- dealerOffers %>% filter(!yearsToNxtCall < 7 | yearsToNxtCall == 0)


# There is some data that has incorrect yield; filtering out all yield > 8%
dealerOffers <- dealerOffers %>% filter(askYTC < 8)

# process state 
dealerOffers$state <- as.factor(dealerOffers$state)
dealerOffers$state <- as.numeric(dealerOffers$state)

# change ratings to numeric value so Aaa = 1, Aa1 = 2, Aa2 = 3 etc
# Note there is no D rating for Moodys -- 
# So S&P rating scale needs to be adjusted to have 21 (C&D are same)
moodysDF <- data.frame(rating = c("Aaa", "Aa1", "Aa2", "Aa3",
                                  "A1", "A2", "A3",
                                  "Baa1", "Baa2", "Baa3",
                                  "Ba1", "Ba2", "Ba3",
                                  "B1", "B2", "B3",
                                  "Caa1", "Caa2", "Caa3",
                                  "Ca", "C"), 
                       ratingValue = c(1:21))

spDF <- data.frame(rating = c("AAA", "AA+", "AA", "AA-",
                                  "A+", "A", "A-",
                                  "BBB+", "BBB", "BBB-",
                                  "BB+", "BB", "BB-",
                                  "B+", "B", "B-",
                                  "CCC+", "CCC", "CCC-",
                                  "CC", "C","D"), 
                       ratingValue = c(1:21,21))

# Null ratings
nullRatings <- c("N.A.", "NA", "NR")

# Change nullRatings to NA 
x <- which(dealerOffers$moody %in% nullRatings)
dealerOffers$moody[x] <- NA
x <- which(dealerOffers$sp %in% nullRatings)
dealerOffers$sp[x] <- NA

# provide a number for each rating
dealerOffers <- dealerOffers %>% mutate(moodyNum = NA, spNum = NA, spreadToAAAi = NA)

for (i in 1:nrow(dealerOffers)) {
    if (dealerOffers$moody[i] %in% moodysDF$rating) {
        x <- which(moodysDF$rating == dealerOffers$moody[i])
        dealerOffers$moodyNum[i] <- moodysDF$ratingValue[x]
    }
    if (dealerOffers$sp[i] %in% spDF$rating) {
        y <- which(spDF$rating == dealerOffers$sp[i])
        dealerOffers$spNum[i] <- spDF$ratingValue[y]
    }
    # the rate is end of day -- the offer sheets are usually beginning of day
    # taking previous day AAA
    
    lookupDate <- as.Date(previousBusinessDay(dealerOffers$date[i], holidays = holidays(year(dealerOffers$date[1]))))
    if (lookupDate %in% historicalRates$date) {
        z <- which(historicalRates$date == lookupDate)
        # take first element because data has some duplicate entries
        z <- z[1]
        ## Calculate spline curve from current yield curve
        muniCurve <- spline(c(1,2,5,10,30), 
                            c(historicalRates$muniYield1Y[z], 
                              historicalRates$muniYield2Y[z], 
                              historicalRates$muniYield5Y[z], 
                              historicalRates$muniYield10Y[z], 
                              historicalRates$muniYield30Y[z]), 
                            n=30, method = "natural")
        muniCurve <- as.data.frame(muniCurve)
        names(muniCurve) <- c("Maturity", "AAA_Yield")
        muniCurve$AAA_Yield <- round(muniCurve$AAA_Yield, digits = 2)
        findAAA <- dealerOffers$yearsToMat[i]
        if (floor(findAAA) >= 30) {
            findAAA <- 30
        }
        floorAAA <- muniCurve$AAA_Yield[floor(findAAA)]
        if (findAAA %% 1 > 0) {
            ceilingAAA <- muniCurve$AAA_Yield[ceiling(findAAA)]
            interpolatedAAA <- ((findAAA %% 1) * (ceilingAAA - floorAAA)) + floorAAA
        } else {
            interpolatedAAA <- floorAAA
        }
        dealerOffers$spreadToAAAi[i] <- dealerOffers$askYTC[i] - interpolatedAAA
    }
}

# some cleanup
dealerOffers$spreadToAAAi <- round(dealerOffers$spreadToAAAi,2) * 100
dealerOffers$yearsToMat <- round(dealerOffers$yearsToMat, 2)
dealerOffers$yearsToNxtCall <- round(dealerOffers$yearsToNxtCall, 2)

# It is assumed that bonds with no next call are noncallable
dealerOffers$yearsToNxtCall[which(is.na(dealerOffers$yearsToNxtCall))] <- 0

# Need to drop NA
dealerOffers <- dealerOffers %>% filter(!is.na(spreadToAAAi))
dealerOffers <- dealerOffers %>% filter(!is.na(moodyNum))


##########################################################################
# Prediction section
##########################################################################
library(caret)
# inTrain <- createDataPartition(y=dealerOffers$CUSIP, p=0.75, list=FALSE)
# training <- dealerOffers[inTrain,]
# testing <- dealerOffers[-inTrain,]
training <- dealerOffers[1:1400,]
testing <- dealerOffers[1401:1538,]

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
modelFit <- train(spreadToAAAi ~ state + CPN + yearsToMat + yearsToNxtCall + moodyNum,
                  trControl = ctrl,
                  data = training, 
                  method = "knn")

predictions <- predict(modelFit, newdata = testing)

results <- data.frame(actual = testing$spreadToAAAi, estimate = predictions)
results <- results %>% mutate(diff = actual - estimate)
results$estimate <- round(results$estimate, 1)
results$diff <- round(results$diff, 1)
mean(results$diff)
median(results$diff)
max(results$diff)
min(results$diff)
plot(results$diff)
sum(results$diff < 20 & results$diff > -20)/nrow(results)
sum(results$diff < 10 & results$diff > -10)/nrow(results)
sum(results$diff < 5 & results$diff > -5)/nrow(results)
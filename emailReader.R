library(ggplot2)
library(lubridate)
library(XML)
library(lubridate)
library(RMySQL)
library(dbConnect)
library(tidyr)
library(dplyr)
library(stringr)
library(jrvFinance)
library(RSelenium)

setwd("~/Desktop/test")
source("extractFunctions.R")

dealerOffers <- data.frame(CUSIP = character(),
                           issuer = character(),
                           state = character(),
                           CPN = numeric(),
                           mty = as.Date(character()),
                           nxtCall = as.Date(character()),
                           moody = character(),
                           sp = character(),
                           askSize = numeric(),
                           askYTC = numeric(),
                           date = as.Date(character()),
                           fileNum = character())

fileList <- dir("newEmails")

for (i in seq(fileList)) {
    emailRead <- readLines(paste0("~/Desktop/test/newEmails/",fileList[i]), skipNul = TRUE)
    emailRead <- sub("[\"\\]", "", emailRead)
    emailRead <- sub("[\\\"]", "", emailRead)
    
    ## check to make sure file is of a certain type
    extractFlag <- extractCheck(emailRead)
    
    ## extract Data    
    if (extractFlag > 0) {
        emailStartRow <- extractFlag
        emailEndRow <- NROW(emailRead)
        # Contains the date email was sent
        emailDate <- emailRead[2]
        emailDate <- mdy(emailDate)
        ## Check header
        emailHeader <- emailRead[emailStartRow]
        ## Extract the data portion of the email
        emailRead <- emailRead[emailStartRow+1:emailEndRow]
        for (j in 1:emailEndRow) {
            emailExtracted <- extractData(emailRead[j])
            if (!is.null(emailExtracted)) {
                emailExtracted <- emailExtracted %>% mutate(date = emailDate, fileNum = fileList[i])
                dealerOffers <- rbind(dealerOffers, emailExtracted)
            }
        }
        
    }    
}


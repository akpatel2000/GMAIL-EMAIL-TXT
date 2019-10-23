extractData <- function(inputLine = character()) {
    emailLine <- inputLine
    emailCUSIP <- substr(emailLine,1,9)
    cusipFlag <- grepl("[0-9][0-9][0-9][0-9|a-z][0-9|a-z][0-9|a-z][0-9|a-z][0-9|a-z][0-9|a-z]", emailCUSIP, ignore.case = TRUE)
    # Check if valid CUSIP -- else return
    if (cusipFlag == FALSE) {
        return(NULL)    
    }
    cpnLoc <- regexpr("[0-9][.]", emailLine, ignore.case = TRUE)
    nextSpace <- regexpr("[ ]+", substr(emailLine, cpnLoc[1], 100L))
    emailCPN <- substr(emailLine, cpnLoc[1], cpnLoc[1]+nextSpace[1]-2)
    emailCPN <- as.numeric(emailCPN)
    emailState <- substr(emailLine, cpnLoc-3, cpnLoc-2)
    emailIssuer <- substr(emailLine, 11, cpnLoc-5 )
    cursorLoc <- cpnLoc[1]+nextSpace[1]
    emailMty <- substr(emailLine, cursorLoc[1],cursorLoc+7)
    emailMty <- mdy(emailMty)
    emailNxtCall <- substr(emailLine, cursorLoc+7+2, cursorLoc+7+2+7)
    emailNxtCall <- mdy(emailNxtCall)
    emailSplit <- substr(emailLine, cursorLoc+7+2+7+2, 100L)
    emailSplit <- strsplit(emailSplit, "[ ]+")
    emailMoody <- emailSplit[[1]][1]
    emailSP <- emailSplit[[1]][2]
    emailAskSize <- emailSplit[[1]][3]
    emailAskSize <- gsub(",","",emailAskSize)
    if (!is.na(emailAskSize)) {
        if (str_detect(emailAskSize, regex("[m][m]", ignore_case = TRUE))) {
            emailAskSize <- gsub("m", "", emailAskSize, ignore.case = TRUE)
            emailAskSize <- paste0(emailAskSize,"000")
        }
        emailAskSize <- gsub("m", "", emailAskSize, ignore.case = TRUE)
    }
    emailAskSize <- as.numeric(emailAskSize)
    emailAskYTC <- emailSplit[[1]][4]
    emailAskYTC <- as.numeric(emailAskYTC)
    emailLine <- data.frame(CUSIP = emailCUSIP,
                            issuer = emailIssuer,
                            state = emailState,
                            CPN = emailCPN,
                            mty = emailMty,
                            nxtCall = emailNxtCall,
                            moody = emailMoody,
                            sp = emailSP,
                            askSize = emailAskSize,
                            askYTC = emailAskYTC)
    return(emailLine)
}

extractCheck <- function(inputEmail = character()) {
    ## This function checks to see if the email is of a particular type
    ## it returns an integer.  A positive number represents email passes
    ## format check and the number that is return represents what line number
    ## the data starts.
    ## The return of 0 means the format check failed
    
    emailRead <- inputEmail
    
    ## Check the first 20 rows -- This format starts with word Security
    emailRows <- NROW(emailRead)
    emailStartRow <- 0
    for (i in 1:20) {
        flag1 <- grepl("^Security", emailRead[i], ignore.case = TRUE)
        if (flag1 == "TRUE" & emailStartRow == 0) {
            emailStartRow <- i
        } 
    }
    
    # Check to see if first test passed
    if (emailStartRow == 0) {
        return(0)
    }
    
    # Get rid of all previous rows and start with the header
    emailRead <- emailRead[emailStartRow:emailRows]
    
    # Checking format & order
    flag1 <- regexpr("iss|issuer", emailRead[1], ignore.case = TRUE)
    flag2 <- regexpr("st|state", emailRead[1], ignore.case = TRUE)
    flag3 <- regexpr("cpn|coupon", emailRead[1], ignore.case = TRUE)
    flag4 <- regexpr("mat|mty|maturity", emailRead[1], ignore.case = TRUE)
    flag5 <- regexpr("nxt call|next call", emailRead[1], ignore.case = TRUE)
    flag6 <- regexpr("moody|mdy", emailRead[1], ignore.case = TRUE)
    flag7 <- regexpr("s&p|sp", emailRead[1], ignore.case = TRUE)
    flag8 <- regexpr("sz|size", emailRead[1], ignore.case = TRUE)
    flag9 <- regexpr("ytw|ytc|yield|yld", emailRead[1], ignore.case = TRUE)
    
    if (flag1 < flag2 &  flag2 < flag3 & flag3 < flag4 & flag4 < flag5 & 
        flag5 < flag6 & flag6 < flag7 &
        flag7 < flag8 & flag8 < flag9) {
        return(emailStartRow)
    } else {
        return(0)
    }
}

extractNewIssue <- function(inputLine = character()) {
    asof <- mdy(emailRead[2])
    
    # Check if it is Merrill new issue
    newFlag <- sum(grepl("MSS Wire", emailRead[1:15], ignore.case = TRUE))
    if (newFlag < 1) {
        return(0)
    } 
    
    # Check type of scale (ie preliminary)
    lineRead <- grep(" ==$", emailRead[1:15], ignore.case = TRUE)
    lineRead <- lineRead[1]
    loc <- str_locate(emailRead[lineRead], " == ")
    partLine <- substr(emailRead[lineRead], loc, 10000L)
    partLine <- gsub("=", "", partLine)
    partLine <- str_trim(scaleType, "both")
    pricingFlag <- grepl("Preliminary Pricing", partLine, ignore.case = TRUE)
    if (pricingFlag == FALSE) {
        return(0)
    }
    
    lineRead <- which(grep("^RE:", emailRead[1:15], ignore.case = TRUE))
    dealPar <- as.numeric(gsub("[^0-9]", "",emailRead[lineRead]))
    lineRead <- lineRead + 1
    issuer <- emailRead[lineRead]
    
    lineRead <- grep(" +moody's: +([a-z0-9]+)? +s&p", emailRead[1:40], ignore.case = TRUE)
    lineRead <- lineRead[1]
    loc <- str_locate(emailRead[lineRead], "[Mm][Oo][Oo][Dd][Yy]'[Ss]:")
    moody <- substr(emailRead[lineRead], loc[2]+1,loc[2]+7)
    moody <- str_trim(moody, "both")
    loc <- str_locate(emailRead[lineRead], "[Ss][&][Pp]:")
    sp <- substr(emailRead[lineRead], loc[2]+1,loc[2]+7)
    sp <- str_trim(sp, "both")
    
}
library(tm.plugin.mail)
library(EML)
library(base64)

## Prior to running this program you must have run mboxToEML or related function
## to create readable files which will be processed to get at the plain text of 
## each email.
## this function takes the eml file 
## looks for the CONTENT-TYPE PLAIN/TEXT UTF-8 section which contains the encoded
## text of the email
## the encoded text is converted from the based64 and written to a csv file


 
readEmail <- function (fileName) {
    # read eml files
    mailText <- readLines(fileName)
    
    # look for Content-type: plain/text UTF-8
    locText1 <- grep("([Cc][Oo][Nn][Tt][Ee][Nn][Tt])+.([Tt][Yy][Pp][Ee])+(.*)([Tt][Ee][Xx][Tt]).([Pp][Ll][Aa][Ii][Nn])+(.*)([Uu][Tt][Ff]-8)", 
                     mailText, ignore.case = TRUE)
    
    ## Make sure content is base64 encoded; check only next 5 lines
    logicalTest <- grepl("base64", 
                         mailText[locText1:(locText1+5)], ignore.case = TRUE)
    if (sum(logicalTest)>0) {
        logicalTest <- TRUE
        
    } else
    {
        logicalTest <- FALSE
        return("Not Base 64 encoded")
    }
    
    ## Find start of encoded text as represented by empty string
    ## continue to search all thru document.  The second spot will represent the 
    ## end of the text.  Convert from base64.  Check to see how the page breaks
    ## put it in a dataframe.
    logicalTest <- mailText[locText1: length(mailText)] == ""
    locText2 <- which(logicalTest)
    beginLoc <- locText1 + locText2[1] - 1
    endLoc <- locText1 + locText2[2] - 1
    mailText <- base64enc::base64decode(mailText[beginLoc:endLoc])
    mailText <- rawToChar(mailText)
    if (grepl("\\r\\n", mailText)) {
        splitText <- "\r\n"
    } else {
        if (grepl("\\r", mailText)) { 
            splitText <- "\r"} else {
                if (grepl("\\n", mailText)) {
                    splitText <- "\n"
                }
            }
    }
    
    mailText <- strsplit(mailText, splitText)
    return(mailText)
}

# the eml files reside in:
fname <- dir("~/Desktop/test/ext")
# the place you want to write the outpu:
newdir <- "~/Desktop/test/newEmails"

# loop thru the number of eml files and make the function call to readEmail
for (i in seq(fname)) {
    x <- readEmail(fname[i])
    write.csv(x, file = paste0(newdir, "/", fname[i]), row.names = FALSE)
}
library(EML)

# This program needs to be changed for each run.  
# It is meant to be run once only.
# It may take sometime to complete
# It is only one function.
# The first contains the location and file of mbox (GMail download)
# The second contains the directory of where you want to email files to be created

## Took mbox and converted to eml -- returns true if succesful.
mail <- convert_mbox_eml("/Users/atulpatel/Downloads/Takeout/Mail/Muni Dealer.mbox",
                         "/Users/atulpatel/Desktop/test/ext")
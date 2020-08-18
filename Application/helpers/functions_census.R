### This file is designed to collect all special functions needed for the creation of the census variables

# define mode 
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

# Define function that turns the factor variables on disabilities into binary
  binarize <- function(x){

    # put strings into numbers
    x <- as.numeric(x)
    
    # binarize it
    if(is.na(x)){NA} else {if(x %in% 3:4){1} else {0}}
  
  }


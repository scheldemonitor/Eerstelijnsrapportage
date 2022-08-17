# Function to write a Latex table file to a table structure for Rmarkdown

# TO DO:
# - Tabel 3.24 heeft een NA, waarom?
# 

require(stringr)

tex2dt <- function(texfile) {
  # Get the latex table in a dataframe
  tex <- read.csv(texfile, header = FALSE, sep = "&", skip = 3)
  dt <- tex[1:(nrow(tex)-1),] # remove last line
  
  # check if values were made bold in Latex tables
  for (i in 1:ncol(dt)-1) { # ncol-1 to skip last column in this loop
    n <- 0
    m <- 0
    for (j in dt[, i]) {
      n <- n+1
      if(is.na(j)) {
        dt[, i][n] <- ""
      } else if(str_detect(j, "textbf")) {
        m = i # store i to get column to make numeric
        a <- str_trim(j, side = c("both")) # remove all spaces
        b <- strsplit(a, " ")[[1]] # split the string to extract the value
        dt[, i][n] <- b[2]
      }
    }
    if(m > 0) { # check if a value for m is stored
      dt[, m] <- as.numeric(dt[, m]) # store the values as a numeric in the dataframe
    }
  }
  
  # remove or modify last column to get rid of \
  if(str_starts(dt[, ncol(dt)][1], "  \\\\")) { # check if column is empty
    return(dt[1:ncol(dt)-1])
    
  } else { # loop to get correct values in last column if not done by previous loop
    n <- 0
    for (i in dt[, ncol(dt)]) {
      n <- n+1
      if(is.na(i)) {
        dt[, ncol(dt)][n] <- ""
      } else if(str_detect(i, "textbf")) { # process if the values were made bold in the Latex table
        a <- str_trim(i, side = c("both")) # remove all spaces
        b <- strsplit(a, " ")[[1]] # split the string to extract the value
        dt[, ncol(dt)][n] <- b[2]
      } else { # process if the values were made not bold
        a <- strsplit(i, " ")[[1]] 
        if(a[1] == "") { # check to get value instead of empty string
          dt[, ncol(dt)][n] <- a[2] 
        } else {
          dt[, ncol(dt)][n] <- a[1]
        }
      }
      dt[, ncol(dt)] <- as.numeric(dt[, ncol(dt)]) # store the values as a numeric in the dataframe
    }
    return(dt)
  }
}


# TEST
#dt <- tex2dt("Figuren/Hydro/Waterstanden/meanvalues.tex") # standard
#dt <- tex2dt("Figuren/Fysisch/Zuurstof_percentage/meanvalues.tex") # bold values
#dt <- tex2dt("Figuren/Fysisch/Metalen/Boor_fil_meanvalues.tex") # empty last row
#dt <- tex2dt("Figuren/Fysisch/Metalen/Boor_totaal_meanvalues.tex") # NA
#dt <- tex2dt("Figuren/Tabellen/fytoplankton.tex")

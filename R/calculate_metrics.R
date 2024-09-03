# More metrics can be added in the function (e.g. rolling means of X,Y,Z/DBAx,y,z; summary statistics of static
# acceleration if posture is to be taken into account; slopes; logs etc.) This can depend on the question to 
# be answered, the animal in question, the type of behaviour etc.
#
#
# To run function for multiple datasets the following code can be added in the beginning of function:
# require(stringr)
# s <- str_pad(n_sensor, 2, pad = "0")
# path <- paste0("C:\\...\\TLCRP0", s, ".csv") # example for dataset named TLCRP005
# tl <- read.csv(file = path, head = TRUE)
# str(tl)
#
# And then run it like this:
#
# for (i in c(1, 2, 3)){
#   s <- str_pad(i, 2, pad = "0")
#   tl <- clean_sensor(n_sensor = i)
#   tl_code <- paste0("TLCRP0", s)
#   assign(tl_code, tl)
# }
#
######################   RUN CODE  ######################
#
#
# @example
#
# setwd("./code")
# load("timon_acc.Rda")
# 
# source("calculate_metrics.R")
# tl <- calculate_metrics(k = 11)


calculate_metrics <- function(n_sensor = 5, k = 11){
  
  require(dplyr)
  
  # Calculate ODBA and VeDBA with a running mean of e.g., 1 second (k = 11). Should be tested with more interval lengths. 
  
  # Calculation of the static acceleration compenent in each axis by smoothing of the data with a rolling mean
  tl <- tl %>%
    dplyr::mutate(STx = zoo::rollmean(X, k = k, fill = NA),
                  STy = zoo::rollmean(Y, k = k, fill = NA),
                  STz = zoo::rollmean(Z, k = k, fill = NA))
  
  # Calculation of the dynamic body acceleration in each axis (raw acceleration data - static acceleration)
  tl <- tl %>% mutate(DBAx = X - STx,
                      DBAy = Y - STy,
                      DBAz = Z - STz)
  
  # Calculation of the vectorial (VeDBA) and overal (ODBA) dynamic body acceleration
  tl <- tl %>% mutate(VeDBA = sqrt(DBAx^2 + DBAx^2 + DBAy^2))
  tl <- tl %>% mutate(ODBA = abs(DBAx) + abs(DBAy) + abs(DBAz))
  
  
  ## Calculate standard deviation (and other useful metrics) in rolling intervals
  
  
  tl <- tl %>%
   dplyr::mutate(SDx = zoo::rollapply(X, width = k, FUN = sd, fill = NA),
                 SDy = zoo::rollapply(Y, width = k, FUN = sd, fill = NA),
                 SDz = zoo::rollapply(Z, width = k, FUN = sd, fill = NA))
  
  tl <- tl %>%
    dplyr::mutate(MAXx = zoo::rollapply(X, width = k, FUN = max, fill = NA),
                  MAXy = zoo::rollapply(Y, width = k, FUN = max, fill = NA),
                  MAXz = zoo::rollapply(Z, width = k, FUN = max, fill = NA))
  
  tl <- tl %>%
    dplyr::mutate(SDdbax = zoo::rollapply(DBAx, width = k, FUN = sd, fill = NA),
                  SDdbay = zoo::rollapply(DBAy, width = k, FUN = sd, fill = NA),
                  SDdbaz = zoo::rollapply(DBAz, width = k, FUN = sd, fill = NA))
  
  tl <- tl %>%
    dplyr::mutate(MAXdbax = zoo::rollapply(DBAx, width = k, FUN = max, fill = NA),
                  MAXdbay = zoo::rollapply(DBAy, width = k, FUN = max, fill = NA),
                  MAXdbaz = zoo::rollapply(DBAz, width = k, FUN = max, fill = NA))
  
  return(tl)
}

run_prediction <- function () {
  
  library(data.table)
  library(dplyr)
  library(plyr)
  
  # various paths and filenames
  dir <- "data"
  
  fileName_age_gender_buckets <- "age_gender_bkts.csv"
  fileName_countries <- "countries.csv"
  
  fileName_subjects_train <- "train_users.csv"
  fileName_subjects_test <- "test_users.csv"
  
  fileName_sessions <- "sessions.csv"
  
  # build the paths
  
  file_subjects_train<- paste(dir, "/", fileName_subjects_train, sep="")
  file_subjects_test <- paste(dir, "/", fileName_subjects_test, sep="")

  file_age_gender_buckets <- paste(dir, "/", fileName_age_gender_buckets, sep="")

  file_sessions <- paste(dir, "/", fileName_sessions, sep="")
  file_countries <- paste(dir, "/", fileName_countries, sep="")
    
  # read in data for test group
  dt_subjects_train <- read.csv(file_subjects_train)
  dt_subjects_test <- read.csv(file_subjects_test)

  dt_age_gender_buckets <- read.csv(file_age_gender_buckets)
  dt_sessions <- read.csv(file_sessions)
  dt_countries <- read.csv(file_countries)
  
  # test > train info for sessions information 
  
  # prepare countries info
  dt_countries$year <- NULL
  # sort countries by distance. farthest is index 10. US is index 1.
  dt_countries <- dt_countries[order(dt_countries$distance_km),]
  
  dt_age_gender_buckets$age_bucket <- gsub("100\\+", "100-150", dt_age_gender_buckets$age_bucket)
  dt_age_gender_buckets <- dt_age_gender_buckets %>% arrange(country_destination, gender, age_bucket)
  
  
}
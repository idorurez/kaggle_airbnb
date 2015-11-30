run_prediction <- function () {
  
  library(data.table)
  library(dplyr)
  library(plyr)
  library(randomForest)
  
  # various paths and filenames
  dir <- "data"

  string_na = c("", " ", "-unknown-", "NA")
    
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
  df_subjects_train <- read.csv(file_subjects_train, na.strings=string_na)
  df_subjects_test <- read.csv(file_subjects_test, na.strings=string_na)

  df_age_gender_buckets <- read.csv(file_age_gender_buckets, na.strings=string_na)
  df_sessions <- read.csv(file_sessions, na.strings=string_na)
  df_countries <- read.csv(file_countries, na.strings=string_na)
  
  # test > train info for sessions information 
  
  # prepare countries info
  df_countries$year <- NULL
  # sort countries by distance. farthest is index 10. US is index 1.
  df_countries <- df_countries[order(df_countries$distance_km),]
  
  # dt_age_gender_buckets$age_bucket <- gsub("100\\+", "100-150", dt_age_gender_buckets$age_bucket)
  df_age_gender_buckets <- df_age_gender_buckets %>% arrange(country_destination, gender, age_bucket)
  
  # prepping tidy data
  
  # clean test data
  df_subjects_test$date_account_created <- as.numeric(as.POSIXct(df_subjects_test$date_account_created))
  df_subjects_test$date_first_booking <- as.numeric(as.POSIXct(df_subjects_test$date_first_booking))
  #df_subjects_test$first_browser <- as.character(df_subjects_test$first_browser)
  colnames(df_subjects_test)[which(names(df_subjects_test) == "id")] <- "user_id"
  
  # clean train data
  df_subjects_train$date_account_created <- as.numeric(as.POSIXct(df_subjects_train$date_account_created))
  df_subjects_train$date_first_booking <- as.numeric(as.POSIXct(df_subjects_train$date_first_booking))
  #df_subjects_train$first_browser <- as.character(df_subjects_train$first_browser)
  colnames(df_subjects_train)[which(names(df_subjects_train) == "id")] <- "user_id"
  
  df_subjects_train$user_id <- as.character(df_subjects_train$user_id)
  df_subjects_test$user_id <- as.character(df_subjects_test$user_id)
  
  # equalize factors
  
  # clean all NA stuff
  df_subjects_test <- df_subjects_test[complete.cases(df_subjects_test),]
  df_subjects_train <- df_subjects_train[complete.cases(df_subjects_train),]
  
  df_sessions <- df_sessions[complete.cases(df_sessions),]
  df_sessions$action <-as.character(df_sessions$action)
  df_sessions$action_detail <- as.character(df_sessions$action_detail)
  
  # prepare data for merging
  dt_sessions <- data.table(df_sessions, key="user_id")
  dt_subjects_test <- data.table(df_subjects_test, key="user_id")
  dt_subjects_train <- data.table(df_subjects_train, key="user_id")
  dt_countries <- data.table(df_countries, key="country_destination")
  
  # merge sesssions information
  dt_subjects_test <- dt_subjects_test[dt_sessions, nomatch=0]
  dt_subjects_train <- dt_subjects_train[dt_sessions, nomatch=0]
  
  # merge by country destination
  dt_subjects_train <- merge(dt_subjects_train, dt_countries, by="country_destination")
  
  
  # drop levels that were removed due to subsetting
  # also remove uneeded user_id 
  #dt_subjects_train <- droplevels(dt_subjects_train)
  dt_subjects_train$user_id <- as.factor(dt_subjects_train$user_id)
  #dt_subjects_test <- droplevels(dt_subjects_test)
  
  # fix "new factor levels not present in training data" error
  #dt_subjects_test$isTest <- rep(1, nrow(dt_subjects_test))
  #dt_subjects_train$isTest <- rep(0, nrow(dt_subjects_train))
  #fullSet <- rbind(dt_subjects_test,dt_subjects_train, fill=TRUE)
  
  model <- randomForest(as.factor(country_destination) ~ age + gender + signup_method + language, data=train, importance=TRUE, ntree=200)
  
  # write.csv(submit, file = 'submission.csv', row.names = FALSE)
  # modelFit <- knn(x_train, y_train, k = 5)
  
  
}
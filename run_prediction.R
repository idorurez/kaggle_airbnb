run_prediction <- function () {
  
  library(data.table)
  library(dplyr)
  library(plyr)
  library(randomForest)
  library(party)
  
  set.seed(510)
  
  # various paths and filenames
  dir <- "data"

  string_na = c("", " ", "-unknown-", "NA")
    
  fileName_age_gender_buckets <- "age_gender_bkts.csv"
  fileName_countries <- "countries.csv"
  
  fileName_train <- "train_users.csv"
  fileName_test <- "test_users.csv"
  
  fileName_sessions <- "sessions.csv"
  
  # build the paths
  
  file_train<- paste(dir, "/", fileName_train, sep="")
  file_test <- paste(dir, "/", fileName_test, sep="")

  file_age_gender_buckets <- paste(dir, "/", fileName_age_gender_buckets, sep="")

  file_sessions <- paste(dir, "/", fileName_sessions, sep="")
  file_countries <- paste(dir, "/", fileName_countries, sep="")
    
  # read in data for test group
  df_train <- read.csv(file_train, na.strings=string_na)
  df_test <- read.csv(file_test, na.strings=string_na)

  df_age_gender_buckets <- read.csv(file_age_gender_buckets, na.strings=string_na)
  df_sessions <- read.csv(file_sessions, na.strings=string_na)
  df_countries <- read.csv(file_countries, na.strings=string_na)
  
  # test > train info for sessions information 
  
  # prepare countries info
  # df_countries$year <- NULL
  

  
  # sort countries by distance. farthest is index 10. US is index 1.
  df_countries <- df_countries[order(df_countries$distance_km),]
  
  # dt_age_gender_buckets$age_bucket <- gsub("100\\+", "100-150", dt_age_gender_buckets$age_bucket)
  df_age_gender_buckets <- df_age_gender_buckets %>% arrange(country_destination, gender, age_bucket)
  
  # prepping tidy data
  
  # clean test data
  df_test$date_account_created <- as.numeric(as.POSIXct(df_test$date_account_created))
  df_test$date_first_booking <- as.numeric(as.POSIXct(df_test$date_first_booking))
  # df_test$first_browser <- as.character(df_test$first_browser)
  colnames(df_test)[which(names(df_test) == "id")] <- "user_id"
  
  # clean train data
  df_train$date_account_created <- as.numeric(as.POSIXct(df_train$date_account_created))
  df_train$date_first_booking <- as.numeric(as.POSIXct(df_train$date_first_booking))
  # df_train$first_browser <- as.character(df_train$first_browser)
  colnames(df_train)[which(names(df_train) == "id")] <- "user_id"
  
  df_train$age <- as.numeric(df_train$age)
  df_test$age <- as.numeric(df_test$age)
  
  # variable "engineering"
  # -----------------------
  
  # fix age issues
  df_train$age[is.na(df_train$age) | df_train$age==2014] <- as.numeric("18")
  df_test$age[is.na(df_test$age) | df_test$age==2014] <- as.numeric("18")
  
  gender <- c("MALE", "FEMALE", "OTHER")
  # fix gender issues
  df_train$gender[is.na(df_train$gender)] <- as.factor(sample(gender, 1))
  df_test$gender[is.na(df_test$gender)] <- as.factor(sample(gender, 1))
  
  # fix missing data
  df_test$date_first_booking[is.na(df_test$date_first_booking)] <- max(df_test$date_account_created)
  df_train$date_first_booking[is.na(df_train$date_first_booking)] <- max(df_train$date_account_created)
  
  df_test$date_first_booking[is.na(df_test$secs_elapsed)] <- max(df_test$secs_elapsed)
  df_train$date_first_booking[is.na(df_train$secs_elapsed)] <- max(df_train$secs_elapsed)
  
  
  # prepare data for merging
  dt_sessions <- data.table(df_sessions, key="user_id")
  dt_test <- data.table(df_test, key="user_id")
  dt_train <- data.table(df_train, key="user_id")
  dt_countries <- data.table(df_countries, key="country_destination")
  
  # merge sesssions information
  dt_test <- dt_test[dt_sessions, nomatch=0]
  dt_train <- dt_train[dt_sessions, nomatch=0]
  
  # merge by country destination
  # dt_train <- merge(dt_train, dt_countries, by="country_destination")

  # clean all NA stuff
  df_test <- df_test[complete.cases(df_test),]
  dt_train <- dt_train[complete.cases(dt_train),]
  
  # trim
  dt_train$user_id <- NULL
  
  # common destinations ranked
  common_destination_ranked <- names(sort(rank(summary(dt_train$country_destination)), decreasing=TRUE))
  
  dt_train <- droplevels(dt_train)
  # dt_train$user_id <- as.factor(dt_train$user_id)
  dt_test <- droplevels(dt_test)
  
  # sorting out levels
  
  levels(dt_train$signup_method) <- levels(dt_test$signup_method)
  levels(dt_train$action_type) <- levels(dt_test$action_type)
  
  device_type_levels <- union(levels(dt_test$device_type), levels(dt_train$device_type))
  first_device_type_levels <- union(levels(dt_test$first_device_type), levels(dt_train$first_device_type))
  language_levels <-  union(levels(dt_test$language), levels(dt_train$language))
  affiliate_provider_levels <- union(levels(dt_test$affiliate_provider), levels(dt_train$affiliate_provider))
  first_browser_levels <-  union(levels(dt_test$first_browser), levels(dt_train$first_browser))
  
  
  levels(dt_test$device_type) <- device_type_levels
  levels(dt_train$device_type) <- device_type_levels
  
  levels(dt_test$first_device_type) <- first_device_type_levels
  levels(dt_train$first_device_type) <- first_device_type_levels

  levels(dt_test$language) <- language_levels
  levels(dt_train$language) <- language_levels
  
  levels(dt_test$affiliate_provider) <- affiliate_provider_levels
  levels(dt_train$affiliate_provider) <- affiliate_provider_levels
  
  levels(dt_test$first_browser) <- first_browser_levels
  levels(dt_train$first_browser) <- first_browser_levels

  
  
  model <- randomForest(as.factor(country_destination) ~ age + gender + signup_method + language + date_account_created
                        + date_first_booking + action_type + secs_elapsed + device_type, 
                        data=dt_train, 
                        importance=TRUE, ntree=50) 

  
  prediction <- predict(model, dt_test)
  
  postpred <- unique(data.frame(id = dt_test$user_id, country = prediction))
  postpred <- split(postpred, postpred$id)
  # common_destination_ranked <- names(sort(rank(summary(dt_train$country_destination)), decreasing=TRUE))
  
  submit <- NULL

  for (data in postpred) {
    tempRanked <- common_destination_ranked
    
    # get list of non NA countries that this person has listed
    countriesPredicted <- as.character(data$country[!is.na(data$country)])

    # remove list of non NA countries from tempRanked
    tempRanked <- tempRanked[! tempRanked %in% countriesPredicted]

    # get countries list
    countries <- data$country
    
    revised <- NULL
    for (country in countries) {
      if (is.na(country)) {
          country <- tempRanked[1]
          tempRanked <- tempRanked[-1]
      }
    revised <- c(revised, country)
    }
    
    # we've replaced NA with ranked list
    data$country <- revised
    
    if (length(data$country) < 5) {
    # build list of countries left in the ranked list
      user_idList <- rep(as.character(data$id[1]), 5-length(data$country))
      countriesLeft <- tempRanked[1:length(user_idList)]
    
    
      df <- data.frame(id=user_idList, country=countriesLeft)
      data <- rbind(data, df)
    }
    
    submit <- rbind(submit, data)
  }
    
  write.csv(submit, file = 'submission.csv', row.names = FALSE)

  
  
}
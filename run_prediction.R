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
  
  file_train <- paste(dir, "/", fileName_train, sep="")
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
  df_test$date_account_created <- as.POSIXct(df_test$date_account_created)
  df_test$date_first_booking <- as.POSIXct(df_test$date_first_booking)
  colnames(df_test)[which(names(df_test) == "id")] <- "user_id"
  
  # clean train data
  df_train$date_account_created <- as.POSIXct(df_train$date_account_created)
  df_train$date_first_booking <- as.POSIXct(df_train$date_first_booking)
  colnames(df_train)[which(names(df_train) == "id")] <- "user_id"
  
  df_train$age <- as.numeric(df_train$age)
  df_test$age <- as.numeric(df_test$age)
  
  # feature "engineering"
  # -----------------------
  
  # fix age issues
  # let's assume that the folks who don't enter in an age or has the current 2014 year, is the average age.
  # or do we want a randomized value with standard deviation?
  
  df_train$age[df_train$age > 100] <- NA
  df_test$age[df_test$age > 100] <- NA
  
  age_train_mean <- as.numeric(round(mean(df_train$age, na.rm = TRUE)))
  age_test_mean <- as.numeric(round(mean(df_test$age,na.rm = TRUE)))
  age_train_std <- as.numeric(round(sd(df_train$age, na.rm = TRUE)))
  age_test_std <- as.numeric(round(sd(df_test$age,na.rm = TRUE)))
    
  df_train$age[is.na(df_train$age) ] <- sample((age_train_mean - age_train_std) : (age_train_mean + age_train_std), 1)
  df_test$age[is.na(df_test$age) ] <- sample((age_test_mean - age_test_std) : (age_test_mean + age_test_std), 1)
  
  gender <- c("MALE", "FEMALE", "OTHER")
  
  # fix NA / NAN / missing values
  
  # get list of all females, males and other, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTestGenders <- prop.table(summary(df_test$gender[!is.na(df_test$gender)]))
  weightedTrainGenders <- prop.table(summary(df_train$gender[!is.na(df_train$gender)]))
  df_train$gender[is.na(df_train$gender)] <- as.factor(sample(gender, 1, prob=as.vector(weightedTrainGenders)))
  df_test$gender[is.na(df_test$gender)] <- as.factor(sample(gender, 1, prob=as.vector(weightedTestGenders)))
  
  # fix missing data date first booking
  df_test$date_first_booking[is.na(df_test$date_first_booking)] <- mean(df_test$date_account_created)
  df_train$date_first_booking[is.na(df_train$date_first_booking)] <- mean(df_train$date_account_created)
  
  # add month and year of booking as a new variable
  df_train$month <- as.factor(as.vector(months(df_train$date_first_booking)))
  df_test$month <- as.factor(as.vector(months(df_test$date_first_booking)))
  
  df_train$year <- as.factor(as.vector(year(df_train$date_first_booking)))
  df_test$year <- as.factor(as.vector(year(df_test$date_first_booking)))
  
  # get list of all browsers, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTestfBrowser <- prop.table(summary(df_test$first_browser[!is.na(df_test$first_browser)]))
  weightedTrainfBrowser <- prop.table(summary(df_train$first_browser[!is.na(df_train$first_browser)]))
  
  df_train$first_browser[is.na(df_train$first_browser)] <- as.factor(sample(names(weightedTrainfBrowser), 1, prob=as.vector(weightedTrainfBrowser)))
  df_test$first_browser[is.na(df_test$first_browser)] <- as.factor(sample(names(weightedTestfBrowser), 1, prob=as.vector(weightedTestfBrowser)))
  
  
  # get list of all browsers, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTestAffiliateTrack <- prop.table(summary(df_test$first_affiliate_tracked[!is.na(df_test$first_affiliate_tracked)]))
  weightedTrainAffiliateTrack <- prop.table(summary(df_train$first_affiliate_tracked[!is.na(df_train$first_affiliate_tracked)]))
  
  df_train$first_affiliate_tracked[is.na(df_train$first_affiliate_tracked)] <- as.factor(sample(names(weightedTrainAffiliateTrack), 1, prob=as.vector(weightedTrainAffiliateTrack)))
  df_test$first_affiliate_tracked[is.na(df_test$first_affiliate_tracked)] <- as.factor(sample(names(weightedTestAffiliateTrack), 1, prob=as.vector(weightedTestAffiliateTrack)))
  
  
  # prepare data for merging
  dt_sessions <- data.table(df_sessions, key="user_id")
  dt_test <- data.table(df_test, key="user_id")
  dt_train <- data.table(df_train, key="user_id")
  dt_countries <- data.table(df_countries, key="country_destination")
  
  # merge sesssions informationd
  dt_test <- dt_test[dt_sessions, nomatch=0]
  dt_train <- dt_train[dt_sessions, nomatch=0]
  
  # more feature engineering
  dt_test$secs_elapsed[is.na(dt_test$secs_elapsed)] <- mean(dt_test$secs_elapsed, na.rm = TRUE)
  dt_train$secs_elapsed[is.na(dt_train$secs_elapsed)] <- mean(dt_train$secs_elapsed, na.rm = TRUE)
  
  # get list of all browsers, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTrainAffiliateTrack <- prop.table(summary(dt_test$first_affiliate_tracked[!is.na(dt_test$first_affiliate_tracked)]))
  weightedTestAffiliateTrack <- prop.table(summary(dt_train$first_affiliate_tracked[!is.na(dt_train$first_affiliate_tracked)]))
  dt_train$first_affiliate_tracked[is.na(dt_train$first_affiliate_tracked)] <- as.factor(sample(names(weightedTrainAffiliateTrack), 1, prob=as.vector(weightedTrainAffiliateTrack)))
  dt_test$first_affiliate_tracked[is.na(dt_test$first_affiliate_tracked)] <- as.factor(sample(names(weightedTestAffiliateTrack), 1, prob=as.vector(weightedTestAffiliateTrack)))
  
  # get list of all action types, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTrainActionType <- prop.table(summary(dt_test$action_type[!is.na(dt_test$action_type)]))
  weightedTestActionType <- prop.table(summary(dt_train$action_type[!is.na(dt_train$action_type)]))
  dt_train$action_type[is.na(dt_train$action_type)] <- as.factor(sample(names(weightedTrainActionType), 1, prob=as.vector(weightedTrainActionType)))
  dt_test$action_type[is.na(dt_test$action_type)] <- as.factor(sample(names(weightedTestActionType), 1, prob=as.vector(weightedTestActionType)))
  
  # get list of all action types, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTrainActionDetail <- prop.table(summary(dt_test$action_detail[!is.na(dt_test$action_detail)]))
  weightedTestActionDetail <- prop.table(summary(dt_train$action_detail[!is.na(dt_train$action_detail)]))
  dt_train$action_detail[is.na(dt_train$action_detail)] <- as.factor(sample(names(weightedTrainActionDetail), 1, prob=as.vector(weightedTrainActionDetail)))
  dt_test$action_detail[is.na(dt_test$action_detail)] <- as.factor(sample(names(weightedTestActionDetail), 1, prob=as.vector(weightedTestActionDetail)))
  
  # get list of all action types, and randomized sample with weighted averages, NOT NA, that's what we're trying to guess
  weightedTrainDeviceType <- prop.table(summary(dt_test$device_type[!is.na(dt_test$device_type)]))
  weightedTestDeviceType <- prop.table(summary(dt_train$device_type[!is.na(dt_train$device_type)]))
  dt_train$device_type[is.na(dt_train$device_type)] <- as.factor(sample(names(weightedTrainDeviceType), 1, prob=as.vector(weightedTrainDeviceType)))
  dt_test$device_type[is.na(dt_test$device_type)] <- as.factor(sample(names(weightedTestDeviceType), 1, prob=as.vector(weightedTestDeviceType)))
  
  
  
  # merge by country destination
  #dt_train <- merge(dt_train, dt_countries, by="country_destination", all=TRUE)

  # clean all NA stuff
  # df_test <- df_test[complete.cases(df_test),]
  dt_train <- dt_train[complete.cases(dt_train),]
  
  # trim
  dt_train$user_id <- NULL
  
  # common destinations ranked
  common_destination_ranked <- names(sort(rank(summary(dt_train$country_destination)), decreasing=TRUE))
  
  # drop empty levels, so randomForest doesn't throw a fit
  dt_train <- droplevels(dt_train)
  # dt_train$user_id <- as.factor(dt_train$user_id)
  dt_test <- droplevels(dt_test)
  
  # sorting out levels, so randomForest doesn't throw a fit.
  # I COULD simply merge both FIRST, but alas, i have typed too much code already.
  
  device_type_levels <- union(levels(dt_test$device_type), levels(dt_train$device_type))
  first_device_type_levels <- union(levels(dt_test$first_device_type), levels(dt_train$first_device_type))
  language_levels <-  union(levels(dt_test$language), levels(dt_train$language))
  affiliate_provider_levels <- union(levels(dt_test$affiliate_provider), levels(dt_train$affiliate_provider))
  first_browser_levels <-  union(levels(dt_test$first_browser), levels(dt_train$first_browser))
  month_levels <-  union(levels(dt_test$month), levels(dt_train$month))
  year_levels <- union(levels(dt_train$month), levels(dt_test$month))

  levels(dt_train$signup_method) <- levels(dt_test$signup_method)
  levels(dt_train$action_type) <- levels(dt_test$action_type)
    
  levels(dt_test$month) <- month_levels
  levels(dt_train$month) <- month_levels
  
  levels(dt_test$year) <- year_levels
  levels(dt_train$year) <- year_levels
  
  
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

  # run fit
  
  model <- randomForest(as.factor(country_destination) ~ age + gender + signup_method + month + year + language + date_account_created
                        + date_first_booking + action_type + secs_elapsed + device_type, 
                        data=dt_train, 
                        nodesize = 8,
                        importance=TRUE, ntree=200) 

  
  prediction <- predict(model, dt_test)
  
  postpred <- unique(data.frame(id = dt_test$user_id, country = prediction))
  postpred <- split(postpred, postpred$id)

  submit <- NULL

  for (data in postpred) {
    tempRanked <- common_destination_ranked
    
    # get list of non NA countries that this person has listed
    countriesPredicted <- as.character(data$country[!is.na(data$country)])

    # remove list of non NA countries we've found from tempRanked
    tempRanked <- tempRanked[! tempRanked %in% countriesPredicted]

    # get countries list as it stands
    countries <- data$country
    
    revised <- NULL
    for (country in countries) {
      if (is.na(country)) {
          country <- tempRanked[1]
          tempRanked <- tempRanked[-1]
      }
    revised <- c(revised, country)
    }
    
    # we've replaced NAs with a ranked list
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

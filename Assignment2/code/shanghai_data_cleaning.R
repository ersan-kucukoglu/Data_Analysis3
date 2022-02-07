# CLEAR MEMORY
rm(list=ls())
#import libraries
library(data.table)
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)

########################################
#####
# Load data

raw <- fread("https://raw.githubusercontent.com/ersan-kucukoglu/Data_Analysis3/main/Assignment2/data/raw/listings.csv?token=GHSAT0AAAAAABQMT6P6D4S224U7FNT32ADYYQJFSEA")

shanghai <- read_csv("~/Downloads/shanghai.csv.gz")
# filter the data frame for apartments that can accommodate 2-6 guests --------

# check room_type variable
table(shanghai$room_type)

# keep if room_type is entire home/apartment
data <- shanghai %>% filter( room_type == 'Entire home/apt')

# drop room_type variable
data <- data %>% select( -room_type )

# keep apartments which can host 2-6 guests
data <- data %>%
  filter( accommodates %in% c(2:6) )

### filter variables which cannot be used for prediction in this task
data <- data %>% select( -c( listing_url, scrape_id,last_scraped, description, neighborhood_overview, 
                                   picture_url, host_url, host_name, host_location, host_about,
                                   host_thumbnail_url, host_picture_url, host_neighbourhood, 
                                   host_listings_count, neighbourhood_group_cleansed, bathrooms, 
                                  minimum_minimum_nights, minimum_maximum_nights, 
                                   minimum_nights_avg_ntm, maximum_minimum_nights, 
                                   maximum_maximum_nights, maximum_nights_avg_ntm, calendar_updated, 
                                   has_availability, availability_30, availability_60, availability_90, availability_365, 
                                   calendar_last_scraped, number_of_reviews_ltm, number_of_reviews_l30d, license ) )
glimpse(data)

### convert price variable to numeric
# remove $ sign and comma
data$price <- as.numeric(gsub("[\\$,]","",data$price))

names(data)[names(data) == "bathrooms_text"] <- "bathrooms"
# Remove text from bathrooms column
table(data$bathrooms)
data$bathrooms <- gsub("baths", "", data$bathrooms)
data$bathrooms <- gsub("bath", "", data$bathrooms)
data$bathrooms <- replace(data$bathrooms,data$bathrooms == 'Half-',0.5)
data$bathrooms <- as.numeric(data$bathrooms)

# replace 'NA' string with NA
data$bathrooms <- ifelse( data$bathrooms == 'NA', NA, as.numeric( data$bathrooms ) )
# drop bathrooms_text variable
data <- data %>% select( -bathrooms_text )

### convert host_response_rate variable to numeric
# replace 'N/A' string with NA
data$host_response_rate <- ifelse( data$host_response_rate == 'N/A', NA, data$host_response_rate )

# remove % sign and convert to numeric
data$host_response_rate <- as.numeric( gsub( "%","",data$host_response_rate ) )

### convert host_acceptance_rate variable to numeric
data$host_acceptance_rate <- ifelse( data$host_acceptance_rate == 'N/A', NA, data$host_acceptance_rate )

# remove % sign and convert to numeric
data$host_acceptance_rate <- as.numeric( gsub( "%","",data$host_acceptance_rate ) )

### extract amenities
# remove unnecessary signs and convert to list
data$amenities <- tolower( data$amenities )
data$amenities <- gsub("\\[","", data$amenities)
data$amenities <- gsub("\\]","", data$amenities)
data$amenities <- gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))

# define levels and dummies and append to data
levels <- levels(factor(unlist(data$amenities)))
data <- cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levels), table))))


### convert date columns to date
data$first_review <- as.Date( data$first_review, format="%Y-%m-%d" )

data$last_review <- as.Date( data$last_review, format="%Y-%m-%d" )

data$host_since <- as.Date( data$host_since, format="%Y-%m-%d" )

# function to aggregate several columns of same type/category into one generic binary column
aggregate_columns <- function(word){
  
  # subset columns which contain a specific word and save them to another dataframe, also select 'id' to use for merge later
  new_df <- data %>% select(contains(word),"id")
  
  # go row by row to see if any of the rows have a 1, if it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # save new column and id column to another dataframe, this new dataframe is used to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  # merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  # remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  data <<- data %>% select(-colnames(new_df))
}

# aggregate columns for a few amenities that could be important for predicting price
aggregate_columns("refrigerator")
data <- data %>% rename("refrigerator" = col_name)

aggregate_columns("tv")
data <- data %>% rename("tv" = col_name)

aggregate_columns("wifi")
data <- data %>% rename("wifi" = col_name)

aggregate_columns("baby")
data <- data %>% rename("baby" = col_name)

aggregate_columns("stove")
data <- data %>% rename("stove" = col_name)

aggregate_columns("coffee maker")
data <- data %>% rename("coffee_maker" = col_name)

aggregate_columns("elevator")
data <- data %>% rename("elevator" = col_name)

aggregate_columns("air conditioning")
data <- data %>% rename("air_conditioning" = col_name)

aggregate_columns("iron")
data <- data %>% rename("iron" = col_name)

aggregate_columns("pool")
data <- data %>% rename("pool" = col_name)

aggregate_columns("private gym")
data <- data %>% rename("private_gym" = col_name)

aggregate_columns("dishwasher")
data <- data %>% rename("dishwasher" = col_name)

aggregate_columns("microwave")
data <- data %>% rename("microwave" = col_name)

aggregate_columns("sound")
data <- data %>% rename("sound" = col_name)

aggregate_columns("free parking")
data <- data %>% rename("free_parking" = col_name)

aggregate_columns("paid parking")
data <- data %>% rename("paid_parking" = col_name)

# drop the amenities column because a csv cannot store it since it is a list
data <- data %>% select( -amenities )

# drop amenities that were not used
data <- data[ -c( 41:250 )]



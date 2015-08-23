# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)


#if not already downloaded, download file
if (!file.exists("getdata_dataset.zip")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, "getdata_dataset.zip", method="curl")
}  

#if not already unzipped, unzip file 
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#read and define features variable
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# select columns containing means and standard deviations
features.filtered <- grep(".*mean.*|.*std.*", features[,2])
features.names <- features[features.filtered,2]
features.names <- gsub('[-()]', '', features.names)


#load the train dataset parts and combine
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")[features.filtered]
ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
subtrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(subtrain, ytrain, xtrain)

# Load the test dataset parts and combine
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")[features.filtered]
ytest <- read.table("UCI HAR Dataset/test/Y_test.txt")
subtest <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(subtest,ytest,xtest)

# combine test and train dataset; define column names
dataset<- rbind(train, test)
colnames(dataset) <- c("subject", "activity", features.names)

#read activity labels;  
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")
activity.labels[,2] <- as.character(activity.labels[,2])
dataset$activity <- factor(dataset$activity, levels = activity.labels[,1], labels = activity.labels[,2])
dataset$subject <- as.factor(dataset$subject)

#tidy the data: get mean values per subject and activity 
tidydata = dataset %>% gather(variable,value,-activity,-subject) %>% 
					   group_by(variable,subject,activity) %>%
					   summarize(value = mean(value)) %>%
					   spread(variable,value)

write.table(tidydata, "tidydata.txt", row.names = FALSE, quote = FALSE)

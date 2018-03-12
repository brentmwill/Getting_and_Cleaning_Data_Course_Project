################################################################################
#Step 1: Merge the training and test data sets to create one data set          #
################################################################################

## Clear, Prepare Environment
rm(list = ls())
library(data.table)
folder <- "C:/Users/B/Desktop/Data Science Master Folder/Data Cleaning Course/data"
setwd(folder)

## Download, Unzip Data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./wearable.zip")
unzip("./wearable.zip")
setwd("./UCI HAR Dataset")

## Read files into R
# Make a vector defining the files to read into R
list_files_vec1 <- list.files("./train/", full.names = TRUE, pattern = "\\.txt$")
list_files_vec1 <- c(list_files_vec1, list.files("./test/", full.names = TRUE, pattern = "\\.txt$"))
list_files_vec1 <- c(list_files_vec1, "features.txt", "activity_labels.txt")

# Make a vector which will be used to define the object names in the environment
list_files_vec2 <- gsub(x = list_files_vec1, "./train/", "")
list_files_vec2 <- gsub(x = list_files_vec2, "./test/", "")
list_files_vec2 <- gsub(x = list_files_vec2, ".txt", "")
list_files_vec2

# Create a for-loop that reads in the files and names the objects accordingly
for (i in 1:length(list_files_vec1)) {
  temp.file.name <- read.table(list_files_vec1[i])
  assign(x = list_files_vec2[i], value = temp.file.name)
  temp.file.name <- NULL
}

## Rename Columns in data frames
names(y_test) <- "activity.number"
names(y_train) <- "activity.number"
names(X_test) <- features[[2]] # use the second column in features to rename X_test
names(X_train) <- features[[2]] # use the second column in features to rename X_train
names(subject_test) <- "subject"
names(subject_train) <- "subject"

## Combine data frames
test.combined <- cbind(subject_test, y_test, X_test) # create a single test data frame
train.combined <- cbind(subject_train, y_train, X_train) # create a single training data frame
combined <- rbind(test.combined, train.combined) # create a single data frame

################################################################################
#Step 2: Extract Only Columns with Mean or Standard Deviations                 #
################################################################################

## Based on my reading of the question, it appears that only those columns 
## which are means or standard deviations should be included in the analysis. 
## Therefore, I will subset the combined data frame by the vector of columns 
## that contain either "mean" or "std".

vec_means_stds <- grep("mean|std", names(combined))
combined <- combined[, c(1, 2, vec_means_stds)]

################################################################################
#Step 3: Use descriptive activity names to name the activities in the data     #
################################################################################

## For this step, I am using subsetting to replace column denoting activity 
## number with the descriptive activity label provided by the activity_labels
## file. 

combined$activity.names <- activity_labels[combined$activity.number, 2]
combined$activity.number <- NULL

################################################################################
#Step 4: Appropriately label the data set with descriptive variable names      #
################################################################################

## For this step, I am cleaning up the variable names with known values: 
## a leading f stands for frequency, a leading t stands for time (per the 
## codebook). I further eliminated troublesome characters like underscore and 
## parentheses from the names. 

names.cleaned.up <- names(combined)
names.cleaned.up <- gsub("^f", "freq.", names.cleaned.up)
names.cleaned.up <- gsub("^t", "time.", names.cleaned.up)
names.cleaned.up <- gsub("-", ".", names.cleaned.up)
names.cleaned.up <- gsub("\\(", "", names.cleaned.up)
names.cleaned.up <- gsub(")", "", names.cleaned.up)
names(combined) <- names.cleaned.up

################################################################################
#Step 5: Create a second, independent tidy data set with the average of each   #
#        activity and subject.                                                 #
################################################################################

## The following code makes the combined dataset consistent with tidy 
## principles. The "select" step just reorganizes the data, bringing activity 
## names to the second column. The group_by function uses activity.name and 
## subject to group the data for the next step, which creates a new data frame. 
## The new data frame is reduced to each grouping (i.e., unique observations of 
## user-activities) and provides the mean across observations for that grouping.

## Make the Data Consistent with the Tidy Format
tidy.data <- combined %>% 
             select(subject, activity.names, time.BodyAcc.mean.X:freq.BodyBodyGyroJerkMag.meanFreq) %>%
             group_by(activity.names, subject) %>% 
             summarize_all(funs(mean))
             

write.table(tidy.data, "wearable_data_tidy.txt", row.names = FALSE)



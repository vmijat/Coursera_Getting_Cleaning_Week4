library(tidyverse)
library(dplyr)
#download data

file <- file_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file_zip <- "UCI HAR Dataset.zip"
data_dir <- "./_data/"


if (!file.exists(paste0(data_dir, file_zip))) {
        download.file(file_URL, paste0(data_dir, file_zip), mode = "wb")
}



# unzip zip file   
if (file.exists(paste0(data_dir, file_zip))) {
        unzip(paste0(data_dir, file_zip), 
              exdir = "./_data/UCI HAR Dataset")
}


#get training data and test data

files_dir <- paste0(data_dir, "UCI HAR Dataset/UCI HAR Dataset/")

#training data
training_x <- read.table(paste0(files_dir, "train/X_train.txt"))
training_y <- read.table(paste0(files_dir, "train/Y_train.txt"))
training_subject <- read.table(paste0(files_dir, "train/subject_train.txt"))

#test data
test_x <- read.table(paste0(files_dir, "test/X_test.txt"))
test_y <- read.table(paste0(files_dir, "test/Y_test.txt"))
test_subject <- read.table(paste0(files_dir, "test/subject_test.txt"))

features <- read.table(paste0(files_dir, "features.txt"))
activity_labels <- read.table(paste0(files_dir, "activity_labels.txt"))

#features to labels

colnames(training_x) <- features[, 2]
colnames(test_x) <- features[, 2]

colnames(training_subject) <- "Subject_ID"
colnames(test_subject) <- "Subject_ID"

colnames(training_y) <- "Activity_ID"
colnames(test_y) <- "Activity_ID"

colnames(activity_labels) <- c("Activity_ID", "Activity_name")



#merge training set and test set

full_x <- rbind(training_x, test_x)
full_y <- rbind(training_y, test_y)
full_subject <- rbind(training_subject, test_subject)

full_set <- cbind(full_subject, full_y, full_x)

#replace numbers with activity names
full_set$Activity_ID <- factor(full_set$Activity_ID, 
                               levels = activity_labels[, 1],
                               labels = activity_labels[, 2])

# now the datasets are merged, and data is tidy!



#let's extract columns that have 'mean' or 'std' in them

col_names <- colnames(full_set)
selected_columns <- grepl("std", colnames(full_set)) | 
                                  grepl("mean", colnames(full_set)) 


subset_mean_and_std_measurements <- full_set[, selected_columns]

#we also add first two columns (Subject_ID and Activity_id) 
#to create final dataset
subset_mean_and_std <- cbind(full_subject, full_y, subset_mean_and_std_measurements)


# 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average
# of each variable for each activity and each subject.

average_full_set <- subset_mean_and_std %>% 
        group_by(Activity_ID, Subject_ID) %>% 
        summarise_all(list(mean = mean))

write.table(average_full_set, file = "Tidy Data.txt", row.names = FALSE)

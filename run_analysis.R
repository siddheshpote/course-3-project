library(dplyr)

filename <- "course3project"
#to check whether the file already exist
if(!file.exists(filename)){
  fileURL <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
  download.file(fileURL, filename, method = "curl")
}

#to check whether the folder exists
if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

#assigning all data frames

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_train <- read.table("UCI HAR DATASET/train/subject_train.txt", col.names = c("subject"))
x_train <- read.table("UCI HAR DATASET/train/X_train.txt", col.names = c(features$functions))
y_train <- read.table("UCI HAR DATASET/train/y_train.txt", col.names = c("code"))
subject_test <- read.table("UCI HAR DATASET/test/subject_test.txt", col.names = c("subject"))
x_test <- read.table("UCI HAR DATASET/test/X_test.txt", col.names = c(features$functions))
y_test <- read.table("UCI HAR DATASET/test/y_test.txt", col.names = c("code"))



#merging the training and test datasets
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
merged <- cbind(subject, y, x)

#extracting measurement on the means and the standard deviations for each measurements
tidydata <- merged %>% select(subject, code, contains("mean"), contains("std"))

#Using descriptive activity names to name the activities in the data set
tidydata$code <- activities[tidydata$code, 2]

#Appropriately labeling the data set with descriptive variable names
names(tidydata)[2] = "activity"
names(tidydata) <- gsub("Acc", "accelercometer", names(tidydata))
names(tidydata) <- gsub("Mag", "magnitude", names(tidydata))
names(tidydata) <- gsub("Gryo", "gyroscope", names(tidydata))
names(tidydata) <- gsub("BodyBody", "body", names(tidydata))
names(tidydata) <- gsub("^t", "time", names(tidydata))
names(tidydata) <- gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata) <- gsub("^f", "frequency", names(tidydata))
names(tidydata) <- gsub("tBody", "timebody", names(tidydata))
names(tidydata) <- gsub("-mean()", "mean", names(tidydata), ignore.case = TRUE)
names(tidydata) <- gsub("-freq()", "frequency", names(tidydata), ignore.case = TRUE)

#From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject

finaldata <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(finaldata, "finaldata.txt", row.names = FALSE)
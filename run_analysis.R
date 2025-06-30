# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the UCI HAR Samsung Galaxy S 
#   smartphone dataset, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#
# Before any steps, a dylpr library has to be installed to run this code:
install.packages("dplyr")
#
#
#
# STEP 1: GETTING THE DATA
# download zip file containing data
zUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zFile <- "UCI HAR Dataset.zip"

if (!file.exists(zFile)) {
  download.file(zUrl, zFile, mode = "wb")
}

# unzip zip file containing data
dataUCIPath <- "UCI HAR Dataset"
if (!file.exists(dataUCIPath)) {
  unzip(zFile)
}
# STEP 2: READING THE DATA
# read training data
trainSubjects <- read.table(file.path(dataUCIPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataUCIPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataUCIPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataUCIPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataUCIPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataUCIPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataUCIPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataUCIPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# STEP 3: COMBINING DATA TABLES

# combining individual data tables to make single data table
humActive <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# assign column names
colnames(humActive) <- c("subject", features[, 2], "activity")

# STEP 4: KEEPING STD AND MEAN DATA

# keep columns based on column names subject, activity, mean, std
colToKeep <- grepl("subject|activity|mean|std", colnames(humActive))

# keep the data from columns that are being kept
humActive <- humActive[, colToKeep]

# replace activity values with named factor levels
humActive$activity <- factor(humActive$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# get column names
humActCols <- colnames(humActive)

# STEP 5: LABELING DATA SET WITH DESCRIPTIVE NAMES

# remove special characters
humActCols <- gsub("[\\(\\)-]", "", humActCols)

# expand abbreviations and clean up names
humActCols <- gsub("^f", "frequencyDomain", humActCols)
humActCols <- gsub("^t", "timeDomain", humActCols)
humActCols <- gsub("Acc", "Accelerometer", humActCols)
humActCols <- gsub("Gyro", "Gyroscope", humActCols)
humActCols <- gsub("Mag", "Magnitude", humActCols)
humActCols <- gsub("Freq", "Frequency", humActCols)
humActCols <- gsub("mean", "Mean", humActCols)
humActCols <- gsub("std", "StandardDeviation", humActCols)

# use new labels as column names
colnames(humActive) <- humActCols

# group by subject and activity and summarise using mean
humActMeans <- humActive %>%  
  group_by(subject, activity) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# STEP 6: PRINT

# output to file "tidy_data.txt"
write.table(humActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

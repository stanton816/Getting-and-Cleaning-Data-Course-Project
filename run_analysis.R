if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataset to  directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Reading trainings tables:
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## Read features -- List of all features.  These will be used to label x data sets

features <- read.table('./data/UCI HAR Dataset/features.txt')

## Read the activity labels -- Links the class labels with their activity name.  These 
##  will be used to label the y datasets

activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

## Assign the column names to the data sets -- features second column for 
##  x_train, x_test and fixed variable names for other datasets

colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

#######################################################################################
######Requirement 1 - Merges the training and the test sets to create one data set.####
#######################################################################################

#Merge training and testing datasets then combine both
merged_train <- cbind(y_train, subject_train, x_train)
merged_test <- cbind(y_test, subject_test, x_test)
train_and_test <- rbind(merged_train, merged_test)

#Read column names into a vector
column_names <- colnames(train_and_test)

###############################################################################################################
#####Requirement 2 - Extracts only the measurements on the mean and standard deviation for each measurement.###
###############################################################################################################

## Extract the mean and standard deviation measurements - identifying the variables that contain mean and std
mean_and_standard <- (grepl("activityId" , column_names) | grepl("subjectId" , column_names) | 
                 grepl("mean.." , column_names) | grepl("std.." , column_names))

##Create a dataset with only mean and standard deviation measurements
Mean_standard_vars <- train_and_test[ , mean_and_standard == TRUE]


###############################################################################################################
#####Requirement 3 - Uses descriptive activity names to name the activities in the data set ###################
###############################################################################################################

##Merge in the activity names from activitylabels
addActivityNames <- merge(Mean_standard_vars, activityLabels, all.x = TRUE)


###############################################################################################################
#####   Requirement 4 - Appropriately labels the data set with descriptive variable names.  ###################
###############################################################################################################
names(addActivityNames) <- gsub("Acc", "Accelerator", names(addActivityNames))
names(addActivityNames) <- gsub("Mag", "Magnitude", names(addActivityNames))
names(addActivityNames) <- gsub("Gyro", "Gyroscope", names(addActivityNames))
names(addActivityNames) <- gsub("^t", "Time", names(addActivityNames))
names(addActivityNames) <- gsub("std", "StandardDeviation", names(addActivityNames))
names(addActivityNames) <- gsub("^f", "Frequency", names(addActivityNames))

###############################################################################################################
#####  Requirement 5 - creates a second, independent tidy data set with the average of      ###################
#####    each variable for each activity and each subject.                                  ###################
###############################################################################################################

##Create second tidy dataset that shows mean for each subjectID and activityId
tidydata2 <- aggregate(. ~subjectId + activityId, addActivityNames, mean)
##Order the dataset by subjectid and activityid
tidydata2 <- tidydata2[order(tidydata2$subjectId, tidydata2$activityId),]
## Create a text output dataset
write.table(tidydata2, "tidydata2.txt", row.name=FALSE)
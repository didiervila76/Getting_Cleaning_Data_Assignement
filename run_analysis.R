##
# The five essential tasks to complete the Course Project are as follows.
# Loading Activities
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each
#    variable for each activity and each subject.
#
#
#--------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------
# 1. -- Merge training and test sets to create one data set.


#Set working directory on my computer
setwd('C:/HardDisk/Coursera/explotary Analysis/Exploratory-Data-Analysis/Assignment/VersionA/Data');

#features  Loading and extraction for Column Naming
features =read.csv('./features.txt',header = FALSE, sep = ' ')
features <- as.character(features[,2])


# Read in the data, label set, and subject codes for the test data
xtestdata    <- read.table("./test/x_test.txt")     
ytestdata    <- read.table("./test/y_test.txt")      
testsubjects <- read.table("./test/subject_test.txt")

# Test Construction and renaming of the Column
Test<- data.frame(testsubjects ,ytestdata,xtestdata)
names(Test) <- c(c('subject', 'activity'), features)


# Read in the data,label set, and subject codes for the train data
xtraindata    <- read.table("./train/x_train.txt")
ytraindata    <- read.table("./train/y_train.txt")
trainsubjects <- read.table("./train/subject_train.txt")


#Train Construction and renaming of the Column
Train<- data.frame(trainsubjects , ytraindata, xtraindata)
names(Train) <- c(c('subject', 'activity'), features)



# merge  Test and Train  it is the objective of the step 1
all <- rbind(Test, Train)

#--------------------------------------------------------------------------------------------
# 2. -- Extracts only the measurements on the mean and standard deviation for each measurement.


# grab the mean or std 
colselect<-grep("mean|sted",features)


# remove columns that are not means or std. deviation features
filter <- all[,c(1,2,colselect + 2)]


#--------------------------------------------------------------------------------------------
# 3. -- Uses descriptive activity names to name the activities in the data set.

# Read the set of activity labels from the txt file
Labels  <- read.table("./activity_labels.txt")

# Adjustment for character
Labels <- as.character(Labels[,2])

# New name
filter$activity <- Labels[filter$activity]


#--------------------------------------------------------------------------------------------
# 4. -- Appropriately label the data set with descriptive variable names. 

# Manual Correction using the gsub

temporary <- names(filter)
temporary<- gsub("[(][)]", "", temporary)
temporary <- gsub("^t", "TimeDomain_", temporary)
temporary <- gsub("^f", "FrequencyDomain_", temporary)
temporary <- gsub("Acc", "Accelerometer", temporary)
temporary <- gsub("Gyro", "Gyroscope", temporary)
temporary <- gsub("Mag", "Magnitude", temporary)
temporary <- gsub("-mean-", "_Mean_", temporary)
temporary <- gsub("-std-", "_StandardDeviation_", temporary)
temporary <- gsub("-", "_", temporary)
names(filter) <- temporary

#--------------------------------------------------------------------------------------------
# 5.-- Creates a second, independent tidy data set with the average of each
#      variable for each activity and each subject. 

# use the function aggregate 
tidy <- aggregate(filter[,3:48], by = list(activity = filter$activity, subject = filter$subject),FUN = mean)

#write the table
write.table(x = tidy, file = "data_very_tidy.txt", row.names = FALSE)




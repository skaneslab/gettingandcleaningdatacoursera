library(data.table)
library(dplyr)

# Download dataset on the laptop

filename <- "data.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCIHARDataset")) { 
  unzip(filename) 
}


setwd("C:/Users/User/3. GETTING and CLEANING DATA/datacleaningproject/UCIHARDataset/train")


# Read train data

features        <- read.table("./features.txt",header=FALSE)

#activityLabels   <- read.table("./activity_labels.txt",header=FALSE)

subjectTrain    <-read.table("./subject_train.txt", header=FALSE)

xTrain          <- read.table("./X_train.txt", header=FALSE)

yTrain          <- read.table("./y_train.txt", header=FALSE)


colnames(subjectTrain) <- "subject_Id"

colnames(xTrain) <- features[,2]

colnames(yTrain) <- "activity_Id"

trainData <- cbind(subjectTrain,yTrain,xTrain)

# Remove duplicated columns

#trainData <-trainData[!duplicated(as.list(trainData))]

# Read test data

setwd("C:/Users/User/3. GETTING and CLEANING DATA/datacleaningproject/UCIHARDataset/test")


#features        <- read.table("./features.txt",header=FALSE)

#activityLabels   <- read.table("./activity_labels.txt",header=FALSE)

subjectTest   <-read.table("./subject_test.txt", header=FALSE)

xTest          <- read.table("./X_test.txt", header=FALSE)

yTest          <- read.table("./y_test.txt", header=FALSE)


colnames(subjectTest) <- "subject_Id"

colnames(xTest) <- features[,2]

colnames(yTest) <- "activity_Id"

testData <- cbind(subjectTest, yTest ,xTest)

# Remove duplicated columns

#testData <-testData[!duplicated(as.list(testData))]


# Question 1/ Merges the training and the test sets to create one data set.
mergedData<-rbind(trainData,testData)

# Question 2/ Extracts only the measurements on the mean and standard deviation for each measurement.



x <-mergedData[!duplicated(as.list(mergedData))]
mergedData_with_mean_sd <- select(x,"subject_Id","activity_Id", contains("mean"), contains("std"))

# Question 3/ Uses descriptive activity names to name the activities in the data set

#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING

mergedData$activity_Id <- factor(mergedData$activity_Id, labels=c("WALKING","WALKING_UPSTAIRS", "Walking Downstairs", "SITTING", "STANDING", "LAYING"))

# Question 4/ Appropriately labels the data set with descriptive variable names

names(mergedData_with_mean_sd)<-gsub("Acc", "Accelerometer", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("Gyro", "Gyroscope", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("BodyBody", "Body", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("Mag", "Magnitude", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("^t", "Time", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("^f", "Frequency", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("tBody", "TimeBody", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("-mean()", "Mean", names(mergedData_with_mean_sd), ignore.case = TRUE)
names(mergedData_with_mean_sd)<-gsub("-std()", "STD", names(mergedData_with_mean_sd), ignore.case = TRUE)
names(mergedData_with_mean_sd)<-gsub("-freq()", "Frequency", names(mergedData_with_mean_sd), ignore.case = TRUE)
names(mergedData_with_mean_sd)<-gsub("angle", "Angle", names(mergedData_with_mean_sd))
names(mergedData_with_mean_sd)<-gsub("gravity", "Gravity", names(mergedData_with_mean_sd))

# Question 5/From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mergedData_with_mean_sd %>% group_by(subject_Id, activity_Id) %>% summarise_all(mean)->tidy_data_set
write.table (tidy_data_set, file = "C:/Users/User/Documents/0. KUALA LUMPUR/20. THRIVE/Maths for DataSciences/DATA SCIENCES SPECIALIZATION/3. GETTING and CLEANING DATA/datacleaningproject/UCIHARDataset/tidydataset.txt",row.name=FALSE)

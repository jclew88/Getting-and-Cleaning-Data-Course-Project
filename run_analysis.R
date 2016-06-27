## Define file & folder path
zurl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zpath<- paste(getwd(),"UCI HAR Dataset",sep="/")
zdestfile<- paste(getwd(),"data.zip",sep="/")

##Load required packages
library(dplyr)
library(data.table)
library(tidyr)

## Download & unzip
download.file(zurl,zdestfile)
unzip(zdestfile)

##*******************************************************************************************

## Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(zpath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(zpath, "test" , "subject_test.txt" )))

## Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(zpath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(zpath, "test" , "Y_test.txt" )))

## Read data files.
dataTrain <- tbl_df(read.table(file.path(zpath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(zpath, "test" , "X_test.txt" )))

##*******************************************************************************************
## Merge data set
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

## combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)

## name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(zpath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

## column names for activity labels
activityLabels<- tbl_df(read.table(file.path(zpath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

## Merge columns
alldataSubject<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubject, dataTable)


##*******************************************************************************************
## Extract data

dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) 



dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 


##*******************************************************************************************
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

##*******************************************************************************************
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

##*******************************************************************************************

##Create Text file

write.table(dataTable,"tidy.txt", row.names = FALSE)

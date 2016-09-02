library(dplyr)
library(stringr)
rm(list=ls())
#download and unzip the files; read in field titles for the other datasets
setwd("~/R/Coursera/Getting and Cleaning Data/project")
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, "UCI HAR Dataset.zip")
dateDownloaded<-date()
unzip("UCI HAR Dataset.zip")
setwd("UCI HAR Dataset")
activity_labels<-read.table("activity_labels.txt", header=FALSE)
features<-read.table("features.txt", colClasses=c("NULL","character"),header=FALSE)

#read in the training data
setwd("train")
subjecttrain<-read.table("subject_train.txt",header=FALSE)
xtrain<-read.table("X_train.txt", header=FALSE)
ytrain<-read.table("y_train.txt", header=FALSE)

# read in the test data
setwd("..")
setwd("test")
subjecttest<-read.table("subject_test.txt",header=FALSE)
xtest<-read.table("X_test.txt", header=FALSE)
ytest<-read.table("y_test.txt", header=FALSE)

# 1. Merges the training and the test sets to create one data set. AND
# 3. Uses descriptive activity names to name the activities in the data set.
alldata<-rbind(cbind(subjecttrain, ytrain, xtrain),cbind(subjecttest, ytest, xtest))
colnames(alldata)<-c("subject","activity",features[,1])

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
featuresmeanstd<-grep("[M,m]ean|std", features[,], value=FALSE)
reduceddata<-cbind(subject=alldata$subject, activity=alldata$activity, alldata[,2+featuresmeanstd])

# 4. Appropriately labels the data set with descriptive variable names.
changed<-colnames(reduceddata)
changed<-str_replace_all(changed,pattern="[[:punct:]]","")
changed<-str_replace_all(changed,pattern="^t","total")
changed<-str_replace_all(changed,pattern="^f","force")
changed<-str_replace_all(changed,pattern="Acc","acceleration")
changed<-str_replace_all(changed,pattern="BodyBody", "body")
changed<-tolower(changed)
colnames(reduceddata)<-changed

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
means<-data.frame()
tidydata<-data.frame()
for (i in 1:30) {
  for (j in 1:6) {
    subset<-subset(reduceddata, subject==i & activity==j)
    means<-colMeans(subset)
    tidydata<-rbind(tidydata, means)
  }
}
colnames(tidydata)<-changed
tidydata<-merge(tidydata, activity_labels, by.x="activity", by.y="V1")
tidydata <- subset(tidydata, select=c(subject, V2, 3:88))
tidydata<-rename(tidydata, activity=V2)
tidydata<-tidydata[order(tidydata$subject),]
setwd("~/R/Coursera/Getting and Cleaning Data/project")

write.table(tidydata, file="tidydata.txt", row.names=FALSE, col.names=TRUE)


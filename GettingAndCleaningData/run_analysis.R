# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers 
# on a series of yes/no questions related to the project. You will be required to submit: 
#
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work that you performed 
#    to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
#
# This repo explains how all of the scripts work and how they are connected. 
#  
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
# The data linked to from the course website represent data collected from the accelerometers from 
# the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
# 
# 		http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
# 
# 		https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive activity names. 
# 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


# ===========================================
#   R commands from here - Downloading data
# ===========================================

# Libraries needed
library(data.table)
library(reshape2)

# Set working directory
setwd("/media/ADORNES/Profissional/Mestrado/MOOC/Coursera Specialization Data Science JHU/repo/GettingAndCleaningData")

# Free memory space
rm(list=ls())

# Download and unzip dataset files
if(!file.exists("dataset") && !file.exists("dataset.zip")){
	download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
				  destfile="dataset.zip", method="wget") # use "wget" for Linux, or "curl" for Mac"
	unzip(zipfile="dataset.zip")
}


# ============================
#   Reading and merging data
# ============================

# read features and activities
features   <- read.table("./dataset/features.txt", as.is=TRUE, col.names=c("id", "name"))
activities <- read.table("./dataset/activity_labels.txt", as.is=TRUE, col.names=c("id", "activity"))
activities$activity <- as.factor(activities$activity)

# read subject column into a vector
mergedData <- rbind(read.table("./dataset/train/subject_train.txt", col.names=c("subject")), 
				    read.table("./dataset/test/subject_test.txt",   col.names=c("subject")))

# read and merge activity codes
mergedData <- cbind(mergedData, rbind(read.table("./dataset/train/y_train.txt", col.names=c("activity_id")), 
				  					  read.table("./dataset/test/y_test.txt",   col.names=c("activity_id"))))

# read and merge feature data
mergedData <- cbind(mergedData, rbind(read.table("./dataset/train/X_train.txt", col.names=features$name), 
									  read.table("./dataset/test/X_test.txt",   col.names=features$name)))

# merge activity labels and discards activity id
mergedData <- merge(activities, mergedData, by.x="id", by.y="activity_id", all.y=TRUE, sort=TRUE)
mergedData <- mergedData[,setdiff(names(mergedData),c("activity_id","id"))]


# =======================================
#   Selecting mean and std columns only
# =======================================

# Apply grep to filter subject, activity, mean and std columns
relevant_cols <- grep("subject|activity|mean|std", names(mergedData))

# select relevant columns
mergedData <- mergedData[,relevant_cols]


# =========================================================================
#   Calculating average for each feature, groupin by subject and activity
# =========================================================================

# feature and group cols
feature_cols <- grep("mean|std", names(mergedData))
group_cols   <- grep("subject|activity", names(mergedData))

# performing average calculation for each group
melted_data <- melt(mergedData, id=group_cols, measure.vars=feature_cols)
tidy_data   <- dcast(melted_data, activity + subject ~ variable, mean)

# write table file with tidy data
write.table(tidy_data, file= "tidy_data.txt", sep=" ", quote=FALSE, row.names=FALSE)

# Libraries needed
library(data.table)
library(reshape2)

print("Cleaning data in workspace...")

# Free memory space
rm(list=ls())

print("Cleaning data in workspace [done]")

print("Checking whether dataset is already downloaded...")

# Download and unzip dataset files
if(!file.exists("dataset") && !file.exists("dataset.zip")){
	print("Downloading and uncompressing dataset...")
	
	download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
				  destfile="dataset.zip", method="wget") # use "wget" for Linux, or "curl" for Mac"
	unzip(zipfile="dataset.zip")
}

print("Checking whether dataset is already downloaded [done]")


# ============================
#   Reading and merging data
# ============================

print("Reading and merging data...")

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

print("Reading and merging data [done]")

# =======================================
#   Selecting mean and std columns only
# =======================================

print("Selecting relevant columns...")

# Apply grep to filter subject, activity, mean and std columns
relevant_cols <- grep("subject|activity|mean|std", names(mergedData))

# select relevant columns
mergedData <- mergedData[,relevant_cols]

print("Selecting relevant columns [done]")


# =========================================================================
#   Calculating average for each feature, groupin by subject and activity
# =========================================================================

print("Calculating average and grouping by subject and activity...")

# feature and group cols
feature_cols <- grep("mean|std", names(mergedData))
group_cols   <- grep("subject|activity", names(mergedData))

# performing average calculation for each group
melted_data <- melt(mergedData, id=group_cols, measure.vars=feature_cols)
tidy_data   <- dcast(melted_data, activity + subject ~ variable, mean)

print("Calculating average and grouping by subject and activity [done]")

print("Writing output file: tidy_data.txt...")

# write table file with tidy data
write.table(tidy_data, file= "tidy_data.txt", sep=" ", quote=FALSE, row.names=FALSE)

print("Writing output file: tidy_data.txt [done]")

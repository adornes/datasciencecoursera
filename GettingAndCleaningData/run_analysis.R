print("Cleaning data in workspace...")

rm(list=ls())


# ========================================
#   Function for preparing input dataset
# ========================================
prepareData <- function() {

	# Download and unzip dataset files
	if(!file.exists("dataset") && !file.exists("dataset.zip")) {
		print("Downloading and uncompressing dataset...")
		
		download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="dataset.zip", method="curl") # use "wget" for Linux, or "curl" for Mac"
		unzip(zipfile="dataset.zip")
	}
}


# =========================================
#   Function for reading and merging data
# =========================================
readAndMergeData <- function() {

	# read features and activities
	features   <- read.table("./dataset/features.txt", as.is=TRUE, col.names=c("id", "name"))
	activities <- read.table("./dataset/activity_labels.txt", as.is=TRUE, col.names=c("id", "activity"))
	activities$activity <- as.factor(activities$activity)

	# read subject column into a vector
	data <- rbind(read.table("./dataset/train/subject_train.txt", col.names=c("subject")), read.table("./dataset/test/subject_test.txt",   col.names=c("subject")))

	# read and merge activity codes
	data <- cbind(data, rbind(read.table("./dataset/train/y_train.txt", col.names=c("activity_id")), read.table("./dataset/test/y_test.txt",   col.names=c("activity_id"))))

	# read and merge feature data
	data <- cbind(data, rbind(read.table("./dataset/train/X_train.txt", col.names=features$name), read.table("./dataset/test/X_test.txt",   col.names=features$name)))

	# merge activity labels and discards activity id
	data <- merge(activities, data, by.x="id", by.y="activity_id", all.y=TRUE, sort=TRUE)
	data <- data[,setdiff(names(data),c("activity_id","id"))]

	data
}


# =====================================================
#   Function for selecting mean and std columns only
# =====================================================
cleanData <- function(data) {

	# Apply grep to filter subject, activity, mean and std columns
	relevant_cols <- grep("subject|activity|mean|std", names(data))

	# select relevant columns
	data <- data[,relevant_cols]

	data
}


# =======================================================================================
#   Function for calculating average for each feature, grouping by subject and activity
# =======================================================================================
buildingTidyData <- function(data) {

	# feature and group cols
	feature_cols <- grep("mean|std", names(data))
	group_cols   <- grep("subject|activity", names(data))

	# performing average calculation for each group
	melted_data <- melt(data, id=group_cols, measure.vars=feature_cols)
	tidy_data   <- dcast(melted_data, activity + subject ~ variable, mean)

	tidy_data
}



# =============================
#   Perform the whole process
# =============================

print("Loading libraries needed...")

# Libraries needed
library(data.table)
library(reshape2)

print("Checking whether dataset is already downloaded...")

prepareData()

print("Reading and merging data...")

data <- readAndMergeData()

print("Selecting relevant columns...")

data <- cleanData(data)

print("Calculating average and grouping by subject and activity...")

tidy_data <- buildingTidyData(data)

print("Writing tidy_data.txt...")

# write table file with tidy data
write.table(tidy_data, file= "tidy_data.txt", sep=" ", quote=FALSE, row.names=FALSE)	

print("Done.")

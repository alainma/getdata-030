courseProject <- function() {
    
    readTxtFile <- function(filename) {
        filepath <- paste(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\", filename, sep = "")
        txt <- read.table(file = filepath)
    }
    
    mergeTrainingAndTestSets <- function() {
        
        buildDataFrame <- function(files) {
            bind <- cbind(readTxtFile(files[1]), readTxtFile(files[2]), readTxtFile(files[3]))
        }
        
        # files we need to merge
        trainFiles <- c("train\\subject_train.txt", "train\\X_train.txt", "train\\y_train.txt")
        testFiles <- c("test\\subject_test.txt", "test\\X_test.txt", "test\\y_test.txt")
        
        # merging files
        merged <- rbind(buildDataFrame(trainFiles), buildDataFrame(testFiles))
        
        # building and setting column names
        names <- readTxtFile("features.txt")
        colnames(merged) <- c("Subject", as.character(names[, 2]), "Activity")
        
        merged
    }
    
    useDescriptiveActivityNames <- function() {
        # Read activity labels and convert them to factors
        labels <- factor(readTxtFile("activity_labels.txt")[, 2])
        # Change indices with activity names
        merged$Activity <- labels[merged$Activity]
        
        merged
    }
    
    appropiatelyLabelData <- function() {
        
        changeAxisNames <- function(name) {
            
            # Check if the name contains -X, -Y or -Z
            if (grepl("-X|-Y|-Z", name)) {
                # Get the last character
                lastChar <- substr(name, nchar(name), nchar(name))
                # Delete the -X, -Y or -Z substring
                name <- gsub("-X|-Y|-Z", "", name)
                # Incorporate x-axis, y-axis or z-axis strings
                name <- gsub("-", paste(" ", tolower(lastChar), "-axis ", sep=""), name)
            } else {
                name <- gsub("-", " ", name)
            }
        }
        
        # Get column names
        names <- colnames(merged)
        
        # Clean parenthesis and rename estimate variables
        names <- gsub("mean\\(\\)", "mean", names)
        names <- gsub("std\\(\\)", "standard deviation", names)
        names <- gsub("meanFreq\\(\\)", "mean frequency", names)
        
        # Change axis names
        names <- sapply(X = names, FUN = changeAxisNames)
        
        # Complete the labelling
        names <- gsub("tBodyAccJerkMag", "Time body accelerometer jerk magnitude", names)
        names <- gsub("tBodyAccJerk", "Time body accelerometer jerk", names)
        names <- gsub("tBodyAccMag", "Time body accelerometer magnitude", names)
        names <- gsub("tBodyAcc", "Time body accelerometer", names)
        names <- gsub("tGravityAccMag", "Time gravity accelerometer magnitude", names)
        names <- gsub("tGravityAcc", "Time gravity accelerometer", names)
        names <- gsub("tBodyGyroJerkMag", "Time body gyroscope jerk magnitude", names)
        names <- gsub("tBodyGyroJerk", "Time body gyroscope jerk", names)
        names <- gsub("tBodyGyroMag", "Time body gyroscope magnitude", names)
        names <- gsub("tBodyGyro", "Time body gyroscope", names)
        names <- gsub("fBodyBodyAccJerkMag", "Frequency body accelerometer jerk magnitude", names)
        names <- gsub("fBodyBodyGyroJerkMag", "Frequency body gyroscope jerk magnitude", names)
        names <- gsub("fBodyBodyGyroMag", "Frequency body gyroscope magnitude", names)
        names <- gsub("fBodyAccJerk", "Frequency body accelerometer jerk", names)
        names <- gsub("fBodyAccMag", "Frequency body accelerometer magnitude", names)
        names <- gsub("fBodyAcc", "Frequency body accelerometer", names)
        names <- gsub("fBodyGyro", "Frequency body gyroscope", names)
        
        # Set the new column names
        colnames(merged) <- names
        
        merged
    }
    
    # Check for libraries
    if (!require('dplyr')) {
        stop('The package dplyr was not installed!')
    }
    
    # Check for directories
    if (!file.exists("getdata-projectfiles-UCI HAR Dataset")) {
        stop('Please download the data before execute this script.')
    }
    
    # 1. Merges the training and the test sets to create one data set.
    merged <- mergeTrainingAndTestSets()
    
    # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    merged <- merged[ , which(grepl("mean()|std()|Activity|Subject", colnames(merged)))]
    
    # 3. Uses descriptive activity names to name the activities in the data set
    merged <- useDescriptiveActivityNames()
    
    # 4. Appropriately labels the data set with descriptive variable names.
    merged <- appropiatelyLabelData()
    
    # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    averaged <- group_by(merged, Subject, Activity)
    averaged <- summarise_each(averaged, funs(mean = mean(., na.rm = TRUE)))
    
    write.table(averaged, "tidy.txt", row.name = FALSE)
}
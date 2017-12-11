library(plyr)

	# loading training and test data sets 
	
	  x_train <- read.table("data/dataSets/train/X_train.txt")
      y_train <- read.table("data/dataSets/train/y_train.txt")
      subject_train <- read.table("data/dataSets/train/subject_train.txt")
      
    
      x_test <- read.table("data/dataSets/test/X_test.txt")
      y_test <- read.table("data/dataSets/test/y_test.txt")
      subject_test <- read.table("data/dataSets/test/subject_test.txt")
      
    
      features <- read.table('data/dataSets/features.txt')
      
    
      activityLabels = read.table('data/dataSets/activity_labels.txt')
      
  #  Assigning column names:
      colnames(x_train) <- features[,2] 
      colnames(y_train) <-"activityId"
      colnames(subject_train) <- "subjectId"
     
      colnames(x_test) <- features[,2] 
      colnames(y_test) <- "activityId"
      colnames(subject_test) <- "subjectId"
      
      colnames(activityLabels) <- c('activityId','activityType')
      
  # Merging all data sets
      mrg_train <- cbind(y_train, subject_train, x_train)
      mrg_test <- cbind(y_test, subject_test, x_test)
      setAllInOne <- rbind(mrg_train, mrg_test)
      
# Extracting only the measurements on the mean and standard deviation for each measurement
  
  # Reading column names:
      colNames <- colnames(setAllInOne)
      
  # Create vector for defining ID, mean and standard deviation:
      mean_and_std <- (grepl("activityId" , colNames) | 
                       grepl("subjectId" , colNames) | 
                       grepl("mean.." , colNames) | 
                       grepl("std.." , colNames) 
                      )
      
  # Making nessesary subset from setAllInOne:
      setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
      
# Using descriptive activity names to name the activities in the data set:
      setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                                   by='activityId',
                                   all.x=TRUE)
      
#  Appropriately labeling the data set with descriptive variable names.
      # This step was made in previos steps =) See 1.3, 2.2, 2.3.
      
# Creating a second, independent tidy data set with the average of each variable for each activity and each subject:
      
      
      secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
      secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
      
      
      write.table(secTidySet, "Tidydataset.txt", row.name=FALSE)
        
      
      


setwd("C:/Users/MMamore/Documents/Courses/Getting and Cleaning Data/Project/data")


#load the files

activities <- read.table("activity_labels.txt",header = FALSE)

features <- read.table("features.txt",header = FALSE)

subjectTrain <- read.table("train/subject_train.txt",header = FALSE)

xTrain <- read.table("train/x_train.txt",header = FALSE)

yTrain <- read.table("train/y_train.txt",header = FALSE)


#visualize the data
activities
features
head(subjectTrain)
head(xTrain)
yTrain


#get column count
ncol(subjectTrain)
ncol(xTrain)
ncol(yTrain)


#check that number of rows match before merging
nrow(subjectTrain)
nrow(xTrain)
nrow(yTrain)


#fix column names
colnames(activities)    <- c('activityId','activityType')
colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- features[,2]
colnames(yTrain)        <- "activityId"

#create combined training data
trainingData <- cbind(subjectTrain,yTrain,xTrain)

head(trainingData)
ncol(trainingData)


# get test data
subjectTest <- read.table("test/subject_test.txt",header = FALSE)

xTest <- read.table("test/x_test.txt",header = FALSE)

yTest <- read.table("test/y_test.txt",header = FALSE)


# fix column names
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- features[,2]
colnames(yTest)       <- "activityId"


#create combined test data
testData <- cbind(subjectTest,yTest,xTest)

#-------------------------------------------------------------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
#-------------------------------------------------------------------------------------------------------------------


#combine training and test data
fullData <- rbind(trainingData,testData)


#-------------------------------------------------------------------------------------------------------------------
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
#-------------------------------------------------------------------------------------------------------------------

finalCols  <- colnames(fullData) 

# create logical vector of columns we want to retain
retainedcols <- (grepl("subjectId",finalCols) | grepl("activityId",finalCols) | grepl("-mean..",finalCols) & !grepl("-meanFreq..",finalCols) & !grepl("mean..-",finalCols) | grepl("-std..",finalCols) & !grepl("-std()..-",finalCols))

#filter full data to just the columns we want
fullData <- fullData[retainedcols==TRUE]

#double check columns
colnames(fullData)

#-------------------------------------------------------------------------------------------------------------------
# 3. Use descriptive activity names to name the activities in the data set
#-------------------------------------------------------------------------------------------------------------------

# add activity name to the full table with retained columns
fullData <- merge(fullData,activities,by='activityId',all.x=TRUE)


#-------------------------------------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names. 
#-------------------------------------------------------------------------------------------------------------------

# refresh the column names vectore to updated the columns names
colNames  <- colnames(fullData)

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])

  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  
  colNames[i] <- gsub("(Acc)","Acceleration",colNames[i])
  colNames[i] <- gsub("(Mag)","Magnitude",colNames[i])
}

colnames(fullData) <- colNames


#-------------------------------------------------------------------------------------------------------------------
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#-------------------------------------------------------------------------------------------------------------------

# copy the final table and remove the activitytype column so it is normalized
fullData2  <- fullData[,names(fullData) != 'activityType']

# get means by subject and activity
meansData <- aggregate(fullData2[,names(fullData2) != c('subjectId','activityId')],by=list(subjectId = fullData2$subjectId,activityId=fullData2$activityId),mean)

# remove duplicat columns
meansData <- meansData[,-c(3,4)]

# add the activity names
meansData <- merge(meansData,activities,by='activityId',all.x=TRUE)

#reorder columns to have activityname show up in front
colnames(meansData)
meansData <- meansData[, c(2,1,21,3:20)]


# save the dataset to a text file
write.table(meansData, 'output/tidyDataSet.txt',row.names=FALSE,sep='\t')


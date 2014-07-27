#setup dir
setwd("~/Desktop/R/Tidy_Data")

#supress the scientitifc notation  
options(scipen=100)

#Read in variable names & assign them as the column names
features=read.table('UCI HAR Dataset/features.txt', sep=" ")

#Read in data
X_traindata=read.table('UCI HAR Dataset/train/X_train.txt', header=FALSE, col.names=features[,2])
X_testdata=read.table('UCI HAR Dataset/test/X_test.txt', header=FALSE, col.names=features[,2])

Y_traindata=read.table('UCI HAR Dataset/train/Y_train.txt', header=FALSE)
Y_testdata=read.table('UCI HAR Dataset/test/Y_test.txt', header=FALSE)

#Read in subject information
trainsub=read.table('UCI HAR Dataset/train/subject_train.txt', header=FALSE)
testsub=read.table('UCI HAR Dataset/test/subject_test.txt', header=FALSE)
names(trainsub)=c("subject")
names(testsub)=c("subject")
Sub_total<-rbind(testsub, trainsub)
names(Sub_total)<-"Subject"

#Combine training and testing data sets
X_total<-rbind(X_test, X_train)
Y_total<-rbind(Y_test, Y_train)

#Extract data of means and std only
meanstdfeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X_total <- X_total[,meanstdfeatures]
names(X_total) <- features[meanstdfeatures, 2]
names(X_total) <- gsub("\\(|\\)", "", names(X_total))

# Uses descriptive activity names to name the activities in the data set
act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", as.character(activities[, 2]))
Y_total[,1] = act[Y_total[,1], 2]
names(Y_total) <- "Activity"

# Appropriately labels the data set with descriptive activity names and write the output file

clean_data <- cbind(Sub_total, Y_total, X_total)
write.table(clean_data, "clean_data.txt")

# Creates tidy data set with the average of each variable for each activity and each subject

uniqueSub = unique(Sub_total)[,1]
numSub = length(unique(Sub_total)[,1])
numAct = length(act[,1])
numCol = dim(clean_data)[2]
tidydata = clean_data[1:(numSub*numAct), ]

r = 1
for (s in 1:numSub) {
  for (a in 1:numAct) {
    tidydata[r, 1] = uniqueSub[s]
    tidydata[r, 2] = act[a, 2]
    tmp <- clean_data[clean_data$Subject==s & clean_data$Activity==act[a, 2], ]
    tidydata[row, 3:numCol] <- colMeans(tmp[, 3:numCol])
    r = r+1
  }
}
write.table(result, "tidy_data.txt")

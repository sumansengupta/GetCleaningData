library(dplyr)
# Read all required files
Xtest <- read.table("X_test.txt")
Xtrain <- read.table("X_train.txt")
Ytest <- read.table("Y_test.txt")
Ytrain <- read.table("Y_train.txt")
features <- read.table("features.txt")
subtest <- read.table("subject_test.txt")
subtrain <- read.table("subject_train.txt")
actList <- read.table("activity_labels.txt")

# header capture the features, which are column headers of Test
# and Ttaining files
header <- features[,2]

header1 <- as.vector(header)

# Assign the headers to Test and Training files
names(Xtest) <- header1
names(Xtrain) <- header1

# Assign the name of Test and Training labesls
names(Ytrain) <- "activity"
names(Ytest) <- "activity"

# Assign the names of the subject files
names(subtest) <- "subject"
names(subtrain) <- "subject"

# Assign the names of Activity data frame
names(actList) <- c("activity", "Description")

#Assign labels to all Test and Training activities
testActivity <- merge(Ytest,actList,by="activity")
trainActivity <- merge(Ytrain,actList,by="activity")

# Add the Label and Subject to Test and Training Data
XtestLabelled <- cbind(testActivity[,2],subtest, Xtest)
XtrainLabelled <- cbind(trainActivity[,2],subtrain, Xtrain)


names(XtrainLabelled)[1] <- "Activity"
names(XtestLabelled)[1] <- "Activity"

# Merges the training and the test sets to create one data set.
TestTrain <- rbind(XtestLabelled,XtrainLabelled)

a <- TestTrain[ , (1:563)]
# Extracts only the measurements on the mean and standard deviation for each measurement. 
ttMeanSD <- select(a, contains("Activity"),contains("subject"), contains("std"), contains("mean"))

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
grpttMeanSD <- group_by(ttMeanSD, Activity,subject)
p <- summarise_each(grpttMeanSD, funs(mean))
#Labels the Dataset with Descriptive variable nanes
names(p) <- gsub("\\()", "", names(p))
print(names(p))
write.table(p, file = "tidydata.txt",row.name=FALSE)

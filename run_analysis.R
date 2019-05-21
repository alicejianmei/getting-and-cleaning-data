#Set location of the data folder
data_location <- "C:/Users/whitemonarch/Desktop/Rlearning/course3"
setwd(data_location)

#Get activity list and feature list
activity_list <- read.table("activity_labels.txt", header = F, 
                            sep = " ", stringsAsFactors = F)
feature_list <- read.table('features.txt', header = F, sep = " ", stringsAsFactor = F)

#Now get the data from train folder
subject_train <- read.table("train/subject_train.txt", header = F, 
                            sep = " ", stringsAsFactors = F)
activity_label_train <- read.table("train/y_train.txt", header = F, 
                                   sep = " ", stringsAsFactors = F)
data_train <- read.table("train/X_train.txt", header = F, strip.white = T,
                         sep = "", stringsAsFactors = F)  
#Note: the data file use fixed-length delimiting.  Use strip.white=T to remove
#extra spaces between the numbers.  The sep in this case is empty ("")

#get the data from test folder
subject_test <- read.table("test/subject_test.txt", header = F, 
                           sep = " ", stringsAsFactors = F)
activity_label_test <- read.table("test/y_test.txt", header = F, 
                                  sep = " ", stringsAsFactors = F)
data_test <- read.table("test/X_test.txt", header = F, strip.white = T,
                        sep = "", stringsAsFactors = F)  

#Combine the two datasets.
combined_data <- rbind(data_train, data_test)
#add column names using the feature_list
features <- feature_list[, 2]
colnames(combined_data) <- features

#Check  if the features are either mean or std
features_of_interest <- grepl("mean", features, ignore.case = T) | grepl("std", features, ignore.case =T)
combined_data_of_interest <- combined_data[, features_of_interest] 

#combine subject_train and subject_test
subject_list <- c(subject_train[, 1], subject_test[, 1])
#combine activity_label_train and activity_label_test
activity_label <- activity_list[, 2][c(activity_label_train[, 1], activity_label_test[, 1])]

#combine into a data frame
combined_data_df <- data.frame(subject_list, activity_label, combined_data_of_interest)

z <- split(combined_data_df, as.factor(combined_data_df[,1]))
z1 <- lapply(z, function(x) {sapply(split(x, as.factor(x[,2])), function(y) {apply(y[, -1:-2], 2, mean)})})
z2 <- t(do.call(cbind.data.frame, z1))    #combine list items
z2_names <- rownames(z2)
z2_names2 <- matrix(unlist(strsplit(z2_names, "[.]")), ncol=2, byrow=T)
colnames(z2_names2) <- c("Subject", "Activity")

write.table(data.frame(z2_names2, z2), file="simplifieddata.txt", row.names= F, sep="\t", quote=F)


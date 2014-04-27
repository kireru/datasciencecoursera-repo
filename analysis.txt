merge_data <- function(directory="C:/Users/Gravity/Documents/data science/") {
  path <- paste("./", directory, "/test/X_test.txt", sep="")
  test_data <- read.table(path)
  path <- paste("./", directory, "/train/X_train.txt", sep="")
  train_data <- read.table(path)
  
path <- paste("./", directory, "/activity_labels.txt", sep="")
  activity_labels <- read.table(path)
  
  path <- paste("./", directory, "/train/subject_train.txt", sep="")
  subject_train <- read.table(path)
  path <- paste("./", directory, "/test/subject_test.txt", sep="")
  subject_test <- read.table(path)
 
  path <- paste("./", directory, "/train/y_train.txt", sep="")
  y_train <- read.table(path)
  path <- paste("./", directory, "/test/y_test.txt", sep="")
  y_test <- read.table(path)
  
  y_train_labels <- merge(y_train,activity_labels,by="V1")
  y_test_labels <- merge(y_test,activity_labels,by="V1")
  
train_data <- cbind(subject_train,y_train_labels,train_data)
  test_data <- cbind(subject_test,y_test_labels,test_data)
  
  ## now then we'll merge the test and training data together
  all_data <- rbind(train_data,test_data)

  return (all_data)
}

extract_mean_std <- function(data_set, directory) {
  path <- paste("./", directory, "/features.txt", sep="")
  features_data <- read.table(path)
   mean_std_rows <- subset(features_data,  grepl("(mean\\(\\)|std\\(\\))", features_data$V2) )
  
   colnames(data_set) <- c("Subject","Activity_Id","Activity",as.vector(features_data[,2]))
    mean_columns <- grep("mean()", colnames(data_set), fixed=TRUE)
  std_columns <- grep("std()", colnames(data_set), fixed=TRUE)
  
   mean_std_column_vector <- c(mean_columns, std_columns)
   
  mean_std_column_vector <- sort(mean_std_column_vector)
  
  extracted_data_set <- data_set[,c(1,2,3,mean_std_column_vector)]
  return (extracted_data_set)
}

melt_data_and_write_tidy_set <- function(data_set, path_to_tidyset_file) 
  
  colnames(tidy_data) <- col_names_vector
  
 
  write.table(tidy_data, file=path_to_tidyset_file, sep="\t", row.names=FALSE)
}

merged_data <- merge_data("UCI HAR Dataset")
extracted_mean_std_data_set <- extract_mean_std(merged_data, "UCI HAR Dataset")
melt_data_and_write_tidy_set(extracted_mean_std_data_set, "./tidyset.txt")

## just to double check you can read the tidyset if you wished
read_tidy_set <- function(path_to_tidyset_file) {
  tidy_set <- read.table(path_to_tidyset_file)
  
  return (tidy_set)
}

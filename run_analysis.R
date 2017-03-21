library(plyr)

#code orgainzed into a function
run_analysis <- function() {
  #read the text files
  x1 <- read.table("train/X_train.txt")
  y1 <- read.table("train/y_train.txt")
  s1 <- read.table("train/subject_train.txt")
  x2 <- read.table("test/X_test.txt")
  y2 <- read.table("test/y_test.txt")
  s2 <- read.table("test/subject_test.txt")
  
  #merge the table together
  x <- rbind(x1 ,x2)
  y <- rbind(y1, y2)
  s <- rbind(s1, s2)
  
  #get the mean and standard deviation fields only
  feat <- read.table("features.txt")
  fields <- grep("-(mean|std)\\(\\)", feat[, 2])
  reqTable <- x[, fields]
  names(reqTable) <- feat[fields, 2]
  
  #set the activities names
  activities <- read.table("activity_labels.txt")
  y[, 1] <- activities[y[, 1], 2]
  names(y) <- "activity"
  
  #label the variables
  names(s) <- "subject"
  compTable <- cbind(x, y, s)
  
  #create average table
  avgTable <- ddply(compTable, .(subject, activity), function(x) colMeans(x[, 1:66]))
  write.table(avgTable, "averages_data.txt", row.name=FALSE)
  
}
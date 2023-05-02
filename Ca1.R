# First step will be to import the heartfailure dataset using the readcsv fucntion
heartfailure <- read.csv("HeartFailure.csv")

#Next step would be to view the structure of the newly imported dataset. 
str(heartfailure) 

#Time for some data preparation
#Updating the column names of the names of some variables to more readable names
colnames(heartfailure)[1] <- "patient's age"
colnames(heartfailure)[7] <- "gender"
colnames(heartfailure)[12] <- "follow up days"
colnames(heartfailure)[13] <- "death"

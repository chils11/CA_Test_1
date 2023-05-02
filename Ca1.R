# First step will be to import the heartfailure dataset using the readcsv fucntion
heartfailure <- read.csv("HeartFailure.csv", na ="")

#Next step would be to view the structure of the newly imported dataset. 
str(heartfailure) 

# Called the first 10 rows of the dataset to further see the structure
head(heartfailure, 10)

#Time for some data preparation
#Updating the column names of the names of some variables to more readable names
colnames(heartfailure)[1] <- "patient's age"
colnames(heartfailure)[7] <- "gender"
colnames(heartfailure)[12] <- "follow up days"
colnames(heartfailure)[13] <- "death"

# Time to perform some basic data exploration so as to have a idea on how the balance of our data is
mean <- rowMeans(heartfailure[1])

# The next step is to change all the dichotomous variables into factors
# There are 6 dichotomous variables and 
j
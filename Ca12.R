# First step will be to import the heartfailure dataset using the readcsv fucntion
heartfailure <- read.csv("HeartFailure.csv", na ="")

#Next step would be to view the structure of the newly imported dataset. 
str(heartfailure) 

# Called the first 10 rows of the dataset to further see the structure
head(heartfailure, 10)

#Time for some data preparation
#Updating the column names of the names of some variables to more readable names
colnames(heartfailure)[1] <- "patients age"
colnames(heartfailure)[7] <- "platelets"
colnames(heartfailure)[10] <- "gender"
colnames(heartfailure)[12] <- "follow up days"
colnames(heartfailure)[13] <- "death"

# Time to perform some basic data exploration so as to have a idea on how the balance of our data is
mean <- rowMeans(heartfailure[1])

# The next step is to change all the dichotomous variables into factors
# There are 6 dichotomous variables and all of them would be converted to factors

# converting Anaemia to factor and assigning 0 to No and 1 to yes
heartfailure$anaemia <- factor(heartfailure$anaemia, labels = c("No", "Yes"))

# converting diabetes to factor and assigning 0 to No and 1 to yes
heartfailure$diabetes <- factor(heartfailure$diabetes, labels = c("No", "Yes"))

# converting high_blood_pressure to factor and assigning 0 to No and 1 to yes
heartfailure$high_blood_pressure <- factor(heartfailure$high_blood_pressure, labels = c("No", "Yes"))

# converting gender to factor and assigning 0 to female and 1 to male
heartfailure$gender <- factor(heartfailure$gender, labels = c("Female", "Male"))

# converting smoking to factor and assigning 0 to No and 1 to yes
heartfailure$smoking <- factor(heartfailure$smoking, labels = c("No", "Yes"))

# converting death to factor and assigning 0 to No and 1 to yes
heartfailure$death <- factor(heartfailure$death, labels = c("No", "Yes"))

# Looking at the correlation between all the variables and to see of they have high or low correlation amongst themselves

# Installing the library to check the correlations between each variable
install.packages("psych")
library(psych)
pairs.panels(heartfailure, 
             smooth =TRUE,
             density =TRUE,
             cor = TRUE)


# Research question 1
# Checking the histogram for age and ejection fraction to determine if they are normally distriuted
hist(heartfailure$`patients age`, xlab = "Age (in years)", 
     main = "Frequency chart for age of heart failure patients",
     col = "steelblue")

hist(heartfailure$ejection_fraction, xlab = "Ejection Fraction", 
     main = "Frequency chart for ejection fraction of heart failure patients",
     col = "steelblue")


qqnorm(heartfailure$`patients age`, main = "QQ chart for age of heart failure patients", ylab = "Age")
qqline(heartfailure$`patients age`)

qqnorm(heartfailure$ejection_fraction, main = "QQ chart for ejection fraction of heart failure patients", ylab = "Ejection fraction")
qqline(heartfailure$ejection_fraction)


shapiro.test(heartfailure$`patients age`)
shapiro.test(heartfailure$ejection_fraction   )

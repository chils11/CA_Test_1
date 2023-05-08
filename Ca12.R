# First step will be to import the heartfailure dataset using the readcsv fucntion
heartfailure <- read.csv("HeartFailure.csv", na ="")
heartfailure1 <- read.csv("HeartFailure.csv", na ="")
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
heartfailure$smoking1 <- factor(heartfailure$smoking, labels = c("No", "Yes"))

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

#Plotting the QQ plot for patients age and ejection fraction
qqnorm(heartfailure$`patients age`, main = "QQ chart for age of heart failure patients", ylab = "Age")
qqline(heartfailure$`patients age`)

qqnorm(heartfailure$ejection_fraction, main = "QQ chart for ejection fraction of heart failure patients", ylab = "Ejection fraction")
qqline(heartfailure$ejection_fraction)

# Checking for normality using shapiro-wilks test
shapiro.test(heartfailure$`patients age`)
shapiro.test(heartfailure$ejection_fraction)

#Testing the hypothesis using the Spearman’s Correlation Coefficient
cor.test(heartfailure$`patients age`, heartfailure$ejection_fraction, method = "spearman")

# Research question 2
# Checking the histogram for ejection fraction and platelets to determine if they are normally distributed
hist(heartfailure$ejection_fraction, xlab = "Ejection Fraction", 
     main = "Frequency chart for age of heart failure patients",
     col = "steelblue")

hist(heartfailure$platelets, xlab = "Platlets", 
     main = "Frequency chart of platlets of heart failure patients",
     col = "steelblue")

#Plotting the QQ plot for ejection fraction and platelets
qqnorm(heartfailure$ejection_fraction, main = "QQ chart for ejection fraction of heart failure patients", ylab = "Ejection fraction")
qqline(heartfailure$ejection_fraction)

qqnorm(heartfailure$platelets, main = "QQ chart for Platelets of heart failure patients", ylab = "Platelets")
qqline(heartfailure$platelets)

# Checking for normality using shapiro-wilks test
shapiro.test(heartfailure$ejection_fraction)
shapiro.test(heartfailure$platelets)

#Testing the hypothesis using the Spearman’s Correlation Coefficient
cor.test(heartfailure$ejection_fraction, heartfailure$platelets, method = "spearman")


# Research question 3
#Performing the Shapiro-Wilks test to determine if the diabetes and the death event variable are normally distributed

tapply(heartfailure1$DEATH_EVENT, heartfailure1$diabetes, shapiro.test)
tapply(heartfailure1$diabetes, heartfailure1$DEATH_EVENT, shapiro.test)
#Testing the hypothesis using the chi-squared test
chisq.test(heartfailure$diabetes, heartfailure$death)



# Research question 4
# Checking the histogram for age to determine if it is normally distributed
hist(heartfailure$`patients age`, xlab = "Age (in years)", 
     main = "Frequency chart for age of heart failure patients",
     col = "steelblue")

#Plotting the QQ plot for patients age
qqnorm(heartfailure$`patients age`, main = "QQ chart for age of heart failure patients", ylab = "Age")
qqline(heartfailure$`patients age`)


# Performing the Shapiro-Wilks tests
shapiro.test(heartfailure$`patients age`)
tapply(heartfailure$`patients age`, heartfailure1$high_blood_pressure, shapiro.test)

#Testing the hypothesis using the chi-squared test
chisq.test(heartfailure$`patients age`, heartfailure$high_blood_pressure)


# Research question 5
# Checking the histogram for creatinine phosphokinase and serum sodium to determine if they are normally distributed
hist(heartfailure$creatinine_phosphokinase, xlab = "creatinine phosphokinase", 
     main = "Frequency chart for creatinine phosphokinase of heart failure patients",
     col = "steelblue")

hist(heartfailure$serum_sodium, xlab = "Serum sodium", 
     main = "Frequency chart of serum sodium of heart failure patients",
     col = "steelblue")
#Plotting the QQ plot for creatinine phosphokinase and serum sodium
qqnorm(heartfailure$creatinine_phosphokinase, main = "QQ chart for creatinine phosphokinase of heart failure patients", ylab = "Creatinine phosphokinase")
qqline(heartfailure$creatinine_phosphokinase)

qqnorm(heartfailure$serum_sodium, main = "QQ chart for serum sodium of heart failure patients", ylab = "serum sodium")
qqline(heartfailure$serum_sodium)

# Performing the Shapiro-Wilks tests
shapiro.test(heartfailure$creatinine_phosphokinase)
shapiro.test(heartfailure$serum_sodium) 

#Testing the hypothesis using the Spearman’s Correlation Coefficient
cor.test(heartfailure$creatinine_phosphokinase, heartfailure$serum_sodium, method = "spearman")

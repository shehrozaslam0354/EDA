# Install necessary packages if not installed
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readr")
install.packages("summarytools")

# Load the libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(summarytools)

# Load Titanic dataset from URL
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic_data <- read.csv(url)

# View the first few rows
head(titanic_data)


# Summary of the data
summary(titanic_data)

# Structure of the dataset
str(titanic_data)

# Detailed view of dataset
dfSummary(titanic_data)


# Check for missing values
colSums(is.na(titanic_data))

# Impute missing values for Age with the median
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)

# Drop rows with missing values in Embarked column
titanic_data <- titanic_data[!is.na(titanic_data$Embarked),]

# Verify no more missing values
colSums(is.na(titanic_data))


# Age distribution
ggplot(titanic_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Fare distribution
ggplot(titanic_data, aes(x = Fare)) + 
  geom_histogram(binwidth = 10, fill = "green", color = "black") +
  labs(title = "Distribution of Fare", x = "Fare", y = "Frequency")

# Pclass count
ggplot(titanic_data, aes(x = factor(Pclass))) + 
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Passenger Class Count", x = "Passenger Class", y = "Count")


# Survival rate by passenger class
ggplot(titanic_data, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Proportion", fill = "Survived")

# Age distribution by survival status
ggplot(titanic_data, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Survival Status", x = "Age", y = "Count", fill = "Survived")

# Fare vs Survived
ggplot(titanic_data, aes(x = Survived, y = Fare)) +
  geom_boxplot() +
  labs(title = "Fare vs Survival Status", x = "Survived", y = "Fare")


# Survival by Age, Sex, and Pclass
ggplot(titanic_data, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram(binwidth = 5) +
  facet_grid(Sex ~ Pclass) +
  labs(title = "Survival by Age, Sex, and Pclass", x = "Age", y = "Count", fill = "Survived")

# Pair plot for numeric variables
pairs(titanic_data[, c("Age", "Fare", "Pclass")], 
      main = "Pair Plot of Numeric Variables", 
      col = titanic_data$Survived)




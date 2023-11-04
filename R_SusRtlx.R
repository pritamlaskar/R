# Import csv
df <- read.csv("C:/Users/test/Documents/Pritam/QDA/SusRtlx.csv", header = TRUE)

# View csv
df

# View first few rows
head(df)

# Install Tidyverse
install.packages("tidyverse")
library(tidyverse)

# Data cleaning and exploration

glimpse(df) # data types and columns (summary) from package 'dplyr'
class(df$ID) # data type and is built-in function
class(df$SUS.Score)
class(df$RTLX.Score)
unique(df$SUS.Score) # unique data in column
unique(df$RTLX.Score) # unique data in column

# Descriptive statistics
# sus Score
mean(df$SUS.Score)
median(df$SUS.Score)
IQR(df$SUS.Score)
sd(df$SUS.Score)
max(df$SUS.Score)
min(df$SUS.Score)

# RTLX Score
mean(df$RTLX.Score)
median(df$SUS.Score)
min(df$RTLX.Score)
max(df$RTLX.Score)
sd(df$RTLX.Score)
IQR(df$RTLX.Score)

summary(df) # Summary of stats

install.packages("psych")
library(psych)
describe(df)
describe(df$SUS.Score) # Descriptive SUS Score
describe(df$RTLX.Score) # Descriptive RTLX Score
# Conclusion (SUS): SUS is almost normally distributed as mean is nearly equal to median.
# Conclusion (RTLX): RTLX is left skewed as skew is -0.21.

# Find missing values
colSums(is.na(df)) # colSums is column-wise Sums 
# No nulls found

# Checking for outliers using boxplot
boxplot(df$SUS.Score, main = "Boxplot of SUS Score")
boxplot(df$RTLX.Score, main = 'Boxplot of RTLX Score')
# No outliers found

# Checking for negative values (Since both questionnaire begins from 0)
which(df$SUS.Score<0)
which(df$RTLX.Score<0)
df[92,2] # checking value at 92nd row of SUS Score (col 2)
df$SUS.Score[df$SUS.Score<0] <- mean(df$SUS.Score) # Replacing negative value with mean of column

# Checking for duplicates
duplicated(df)
# No duplicates found

# Graph Visualisation
# Distribution
#SUS Score
hist(df$SUS.Score, breaks = 10, col = 'maroon', main = 'Distribution of SUS Score', xlab = 'SUS Score', ylab = 'Frequency')
plot(density(df$SUS.Score), main = 'Kernel Density Plot', xlab = 'SUS Score', ylab = 'Density')
# SUS Score's distribution is normal

# RTLX Score
hist(df$RTLX.Score, breaks = 10, col = 'maroon', main = 'Distribution of RTLX Score', xlab = 'RTLX Score', ylab = 'Frequency')
plot(density(df$RTLX.Score), main = 'Kernel Density - RTLX', xlab = 'RTLX Score', ylab = 'Density')
# RTLX distribution is left skewed (negative).

# Scatterplot
plot((df$SUS.Score), (df$RTLX.Score), pch = 19, col = 'black', mains = 'Scatterplot - RTLX vs SUS', xlab = 'SUS Score', ylab = 'RTLX Score')
# Conclusion: As SUS Score increases, RTLX score also increases. This suggest a linear relation.

# Statistic Test (Correlation Analysis)
cor(df$SUS.Score, df$RTLX.Score)
# Correlation is .68.

# Correlation Test
cor.test(df$SUS.Score, df$RTLX.Score) # Pearson's Correlation Test
# P Value = 4.281e-15; Significance Level: 0.05. Result: We accept H1. 

# Conclusion:
cat("Correlation between SUS Score and RTLX Score is 0.68. This means there exists a significant statistical relationship between SUS Score and RTLX Score. Also, the p-value is 4.281e-15, which is below our decided significance level of 0.05. As a result, we have sufficient evidence to reject the H0 and accept H1. \n")

# Install Packages
install.packages("tidyverse") # Install Tidyverse
library(tidyverse)
install.packages("psych")
library(psych)

# H0: No significant statistical relation exist between SUS Score and RTLX Score.
# H1: Significant statistical relation exist between SUS Score and RTLX Score.
# Significance Level: 0.05.
# Method to accept H1: When P-Level < Significance Level, we reject H0 and accept H1.
# Statistical test used: Pearson's Correlation Test


## Data Exploration and Cleaning:


# Import csv
df <- read.csv("C:/Users/Pritam Laskar/Documents/UCD Study Materials/Study Materials/Assignments/QDA/SusRtlx.csv", header = TRUE)
# View csv
df
# View first few rows
head(df)

# Checking data types and structure
glimpse(df) # data types and columns (summary) from package 'dplyr'
class(df$ID) # data type and is built-in function
class(df$SUS.Score) # class: numeric
class(df$RTLX.Score) # class: numeric
unique(df$SUS.Score) # unique data in SUS Score
unique(df$RTLX.Score) # unique data in RTLX Score
str(df) # Structure of data

# Checking for values outside range: 
which(df$SUS.Score<0) # Checking data less than min
df[92,2] # checking value at 92nd row of SUS Score (col 2)

# Replacing negative value with mean of SUS Score
df$SUS.Score[df$SUS.Score<0] <- mean(df$SUS.Score
                                     ) 
which(df$SUS.Score>100) # Checking data beyond max

# Replacing data above 100 by 100 as 100 is max for SUS Score:
df$SUS.Score[df$SUS.Score>100] <- 100 

which(df$RTLX.Score<0) # Checking data less than min for RTLX Score
which(df$RTLX.Score>126) # Checking data more than max for RTLX Score

# Checking for duplicates
sum(duplicated(df)) 
# No duplicates found

# Find missing values
colSums(is.na(df)) # colSums is column-wise Sums 
# No nulls found


## Descriptive Statistics:


# Describe:
describe(df)
describe(df$SUS.Score) # Descriptive SUS Score
describe(df$RTLX.Score) # Descriptive RTLX Score

# Feature Column Stats:
# SUS Score
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
# Conclusion (SUS): SUS is almost normally distributed as mean is nearly equal to median.
# Conclusion (RTLX): RTLX is left skewed as skew is -0.21.


## Graphical Presentations:


# Checking for outliers using boxplot
boxplot(df$SUS.Score, main = "Boxplot of SUS Score")
boxplot(df$RTLX.Score, main = 'Boxplot of RTLX Score')
# No outliers found

# Distribution
# SUS Score
hist(df$SUS.Score, breaks = 10, col = 'maroon', main = 'Distribution of SUS Score', xlab = 'SUS Score', ylab = 'Frequency')
plot(density(df$SUS.Score), main = 'Kernel Density Plot of SUS Score', xlab = 'SUS Score', ylab = 'Density')
# Conclusion: SUS Score's distribution is normal
# RTLX Score
hist(df$RTLX.Score, breaks = 10, col = 'maroon', main = 'Distribution of RTLX Score', xlab = 'RTLX Score', ylab = 'Frequency')
plot(density(df$RTLX.Score), main = 'Kernel Density Plot of RTLX Score', xlab = 'RTLX Score', ylab = 'Density')
# Conclusion: RTLX distribution is left skewed (negative).

# Scatterplot
plot((df$SUS.Score), (df$RTLX.Score), pch = 19, col = 'black', main = 'Scatterplot - RTLX vs SUS', xlab = 'SUS Score', ylab = 'RTLX Score')
# Conclusion: As SUS Score increases, RTLX score also increases. This suggest a linear relation.


## Inferential Statistics:


# Statistic Test (Correlation Analysis)
cor(df$SUS.Score, df$RTLX.Score)
# Conclusion: Correlation is .68, which means, correlation exists.

# Pearson's correlation test:
cor.test(df$SUS.Score, df$RTLX.Score, method = "pearson") # Pearson's Correlation Test
# P Value = 4.183x10^-15; Significance Level: 0.05. Result: As P < Significance, we accept H1. 

# Conclusion:
cat("Correlation between SUS Score and RTLX Score is 0.68. This means there exists a significant statistical relationship between SUS Score and RTLX Score. Also, the p-value is 4.183e-15, which is below our decided significance level of 0.05. As a result, we have sufficient evidence to reject the H0 and accept H1. \n")
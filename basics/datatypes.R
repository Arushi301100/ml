#Create Different Data Types in Statistics


#Categorical Data

#Creating Nominal Data of 25 students with Male and Female----
nominal = sample(c('Male', 'Female'), size = 25, replace = T, prob = c(0.6, 0.4))
nominal
table(nominal)
prop.table(table(nominal))


#Creating Ordinal Data of Zomato Ratings----
grades = sample(c('A', 'B', 'C', 'D'), size = 25, replace = T, prob = c(0.45, 0.3, 0.15, 0.1)) 

table(grades)
prop.table(table(grades))

grades1 = factor(grades, ordered = T, levels = c('C', 'B', 'A', 'D'))
grades1
sort(grades1)
length(grades1)

sort(grades1)[length(grades1)/2]
sort(grades1)[c(12, 13)]



#Creating Continuos data or Ratio scale of Students Marks----
marks = round(rnorm(25, mean = 70, sd = 4))
marks
sort(table(marks))
sd(marks)
range(marks)
min(marks)
max(marks)
quantile(marks)


#Creating Discrete data of student ranks----
rank = sample(1:25, size = 25, replace = F)
rank



 
# Measure of Central tendency----

# Mean
(meanrank = mean(rank))
(meanmarks = mean(marks))


# Median
(med_rank = median(rank))
(med_marks = median(marks))




# Mode
grades1= sort(grades1)
(grade_table = table(grades1))
# Find mode
mode_grade <- names(grade_table)[which.max(grade_table)]

mode_grade



# Day 2----


# Create Data Frame manually----
c_code <- c('AUCG', 'AUCG', 'AUMP', 'AUMP', 'AUMH', 'AUUP')
dept <- c('Computer Science', 'Management', 'Law', 'Engineering', 'Pharmacy', 'Education')
faculty_count <- c(30, 20, 18, 50, 15, 10)
std_count <- c(600, 400, 350, 1200, 200, 150)
avg_tlr_score <- c(78.5, 74.3, 81.2, 76.7, 80.5, 72.1)


# Dataframe
(univ_data <- data.frame(c_code, dept, faculty_count, std_count, avg_tlr_score))
str(univ_data)


# Perform Descriptive Analysis ----

#Load libraries
library(psych)
library(pastecs)
library(doBy)

# 1. Basic descriptive statistics
summary(univ_data)
#this will give you min, max, mean, median, quartile for numeric columns


# 2. Apply functions using sapply(), across columns
sapply(univ_data[, c('faculty_count', 'std_count', 'avg_tlr_score')], mean)
#Finds mean of each numeric column
vapply(univ_data[, c('faculty_count', 'std_count', 'avg_tlr_score')], FUN = sd)
#finds standard deviation of each numeric column
?sapply()

sapply(univ_data[, c('faculty_count', 'std_count', 'avg_tlr_score')], FUN = function(x) c(sum =sum(x), mean = mean(x), sd = sd(x)))


# 3.Detailed descriptive stats using stat.desc()
pastecs::stat.desc(univ_data[, c('faculty_count', 'std_count', 'avg_tlr_score')])
#includes variance, coefficient of variation, skewness, kurtosis, etc.

plot(mtcars$wt, mtcars$mpg)
cov(mtcars$wt, mtcars$mpg)
cor(mtcars$wt, mtcars$mpg)
sqrt(208)
round(cor(mtcars), 2)

plot(mtcars$wt, mtcars$hp)
round(cor(mtcars[, c('wt', 'mpg', 'hp')]), 2)



# 4. describe() for descriptive stats (from psych package)
psych::describe(univ_data[, c('faculty_count', 'std_count', 'avg_tlr_score')])

 # 5. aggregate : mean of student counts grouped by campus
aggregate(std_count ~ c_code, data = univ_data, FUN = mean)

# 6. Coefficient of variation (CV) manually:
cv_std_count <- (sd(univ_data$std_count)/mean(univ_data$std_count))

# 7. Group wise summary using summaryBy()
summaryBy(std_count + faculty_count + avg_tlr_score ~ Dept, data = univ_data, FUN=c(mean, sd))

# 8. describeBy() for descriptive stats campus-wise
describeBy(univ_data$avg_tlr_score, group = univ_data$c_code)
#tells your descriptive stats of Avg TLR score grouped by each campus 




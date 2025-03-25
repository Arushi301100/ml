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


# To clear the environment
rm(list=ls())

#------------Import the data set and present few rows------------
healthcare_data = read.csv("D:/DA_SEM_1/Intro Data Sci R-Python/Mid_term_assignment/health_dataset.csv",header = TRUE)
head(healthcare_data)

#------------Data Quality Checks------------
# Check the datatype of each columns
str(healthcare_data)

# Convert Age from Char to Num
healthcare_data$Age <- as.numeric(healthcare_data$Age)

# Verify the updated conversion
str(healthcare_data)

# Summary of the data
summary_health_data <- summary(healthcare_data)
summary_health_data

# Check the duplicate values
sum(duplicated(healthcare_data))

# Check for null values
colSums(is.na(healthcare_data))

# Print to total row and column count
nrow(healthcare_data)
length(healthcare_data[])

#------------Data Cleaning------------

# Removing Room number and Medication as it not required
healthcare_data <- subset(healthcare_data,select = -c(Room.Number, Medication))

# Remove the duplicates from data
healthcare_data <- healthcare_data[!duplicated(healthcare_data), ]

# Check for duplicates
nrow(healthcare_data[duplicated(healthcare_data), ])

# Remove the missing or Null values from data
healthcare_data = na.omit(healthcare_data)

# Check for the null or empty value
colSums(is.na(healthcare_data))

# Print to total row and column count
nrow(healthcare_data)
length(healthcare_data[])

#------------Data Visualization------------

library(ggplot2)
library(dplyr)

#Scatter Bar Plot
barplot(table(healthcare_data$Medical.Condition, healthcare_data$Blood.Type),
        main = 'Medical condition by Blood Group',
        xlab = 'Blood Group',
        col = c('darkblue', 'red', 'yellow', 'pink', 'skyblue','green'),
        legend = TRUE,
        beside = TRUE,
        args.legend = list(x = "topright", inset = c(0.05, 1), 
                           horiz = TRUE, cex = 0.6, bty = "n"))

#Boxplot
boxplot(healthcare_data$Billing.Amount ~ healthcare_data$Medical.Condition,
        main = 'Billing Amount By Medical Condition', 
        xlab = 'Medical Condition',
        ylab = 'Billing Amount',
        col = c('lightblue', 'lightgreen', 'lightpink'),
        notch = TRUE,
        outline = FALSE)

#Scatter plot
ggplot(healthcare_data, aes(x = Insurance.Provider, y = Age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Age by Insurance Provider", x = "Insurance Provider", y = "Age") +
  theme_minimal()

#Histogram plot
ggplot(healthcare_data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Frequency") +
  theme_minimal()

#------------Descriptive Statistics------------
# Summary Statistics and standard deviation for patient age
summary(healthcare_data$Age)
print(paste('Mean for Age: ', mean(healthcare_data$Age)))
print(paste('Median for Age: ', median(healthcare_data$Age)))
print(paste('Standard Deviation for Age: ', sd(healthcare_data$Age)))

summary(healthcare_data$Billing.Amount)
print(paste('Mean for Billing amount: ', mean(healthcare_data$Billing.Amount)))
print(paste('Median for Billing amount: ', median(healthcare_data$Billing.Amount)))
print(paste('Standard Deviation for Billing Amount: ', sd(healthcare_data$Billing.Amount)))

#------------Grouping And Aggregation Data------------
#Aggregate data to find the average billing amount by Insurance Provider
avg_billing_amount <- healthcare_data %>%
  group_by(Insurance.Provider) %>%
  summarise(
    avg_bill = mean(Billing.Amount)
  )
avg_billing_amount

#Print total bill amount by Hospital
print(aggregate(Billing.Amount ~ Hospital, data = healthcare_data, sum))

#Aggregate data to find the average Age by Blood Group and Medical condition
avg_age_data <- healthcare_data %>%
  group_by(Gender, Medical.Condition) %>%
  summarise(
    ave_age = mean(Age)
  )
avg_age_data

#------------Min-Max Analysis------------

# Min and max for Age
min_age <- min(healthcare_data$Age)
max_age <- max(healthcare_data$Age)
print(paste("Min:", min_age, "Max:", max_age))

# Min and max for Billing amount
min_bill_amount <- min(healthcare_data$Billing.Amount)
max_bill_amount <- max(healthcare_data$Billing.Amount)
print(paste("Min:", min_bill_amount, "Max:", max_bill_amount))

# Min and max for Date of Admission 
min_admit_date <- min(healthcare_data$Date.of.Admission)
max_admit_date <- max(healthcare_data$Date.of.Admission)
print(paste("Min:", min_admit_date, "Max:", max_admit_date))

# Min and max for Discharge Date
min_discharge_date <- min(healthcare_data$Discharge.Date)
max_discharge_date <- max(healthcare_data$Discharge.Date)
print(paste("Min:", min_discharge_date, "Max:", max_discharge_date))

#------------Conclusion Visualization------------
# line visualization of Hospital bill over Insurance Provider
ggplot(healthcare_data, aes(x = Hospital)) +
  geom_line(aes(y = Billing.Amount, color = "Billing Amount")) +
  labs(title = "Hospital billing amount over hospital",
       x = "Hospital", y = "Hospital Bill") +
  scale_color_manual("", values = c("Billing Amount" = "blue")) +
  theme_minimal()


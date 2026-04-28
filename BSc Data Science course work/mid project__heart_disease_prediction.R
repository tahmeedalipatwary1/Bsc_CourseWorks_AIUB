install.packages(c("dplyr", "ggplot2"))
install.packages("naniar")
install.packages("modeest")
install.packages("readxl")
library(dplyr)       # For data cleaning and manipulation
library(ggplot2) 
library(naniar) 
library(modeest)
library(readxl)


heart_data<- read_xlsx("E:/10th semester( the last one)/INTRODUCTION TO DATA SCIENCE/mid project/Dataset_MIdterm_sectoin(D).xlsx")
heart_data
str(heart_data)
head(heart_data)
summary(heart_data)

#_________________________________________________________________________________________________________

# Check how many missing values are in each column
is.na(heart_data)
colSums(is.na(heart_data))
missing_rows <- list(Age = which(is.na(heart_data$Age)), Gender = which(is.na(heart_data$Gender)))
print(missing_rows)
total_missing <- sum(is.na(heart_data))
cat("Total Missing: ", total_missing, "\n")



# Visualize missing values using a bar chart
gg_miss_var(heart_data)
vis_miss(heart_data) #in percantage

#handaling missing value
heart_data$Age[is.na(heart_data$Age)] <- median(heart_data$Age, na.rm = TRUE)
 
heart_data$Gender[is.na(heart_data$Gender)] <-  mfv(heart_data$Gender, na_rm = TRUE) # find most frequent value
colSums(is.na(heart_data))

heart_data$BloodPressure <- as.numeric(heart_data$BloodPressure)
str(heart_data$BloodPressure)

#heart_data$Heart_Rate <- as.numeric(heart_data$Heart_Rate)
#str(heart_data$Heart_Rate)

#heart_data <- heart_data[!is.na(heart_data$BloodPressure) & !is.na(heart_data$Heart_Rate), ]

#colSums(is.na(heart_data))


#__________________________________________________________________________________________________________________________

# Show a boxplot to visually detect outliers in Age
boxplot(heart_data$Age, main = "Boxplot of Age", col = "lightblue")

boxplot(heart_data$Cholesterol, main = "Boxplot of Cholesterol", col = "lightblue")
boxplot(heart_data$QuantumPatternFeature, main = "Boxplot of Quantum Pattern Feature", col = "lightblue")



# Calculate IQR (Interquartile Range) to define outlier limits
Q1 <- quantile(heart_data$Age, 0.25)
Q3 <- quantile(heart_data$Age, 0.75)
IQR <- Q3 - Q1

# Set lower and upper boundaries
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

outliers_age <- which(heart_data$Age < lower | heart_data$Age > upper)
heart_data$Age[outliers_age] <- median(heart_data$Age)

heart_data[outliers_age, "Age"]
heart_data[outliers_age, ]

summary(heart_data["Age"])

#__________________________________________________________________________________________

#convert attribute

heart_data$Gender <- factor(heart_data$Gender, 
                            levels = c(0, 1), 
                            labels = c("Female", "Male"))
heart_data$Gender[is.na(heart_data$Gender)] <-  mfv(heart_data$Gender, na_rm = TRUE) 
unique(heart_data$Gender)





summary(heart_data$Cholesterol)

heart_data$Cholesterol_level <- cut(
  heart_data$Cholesterol,
  breaks = c(0, 199, 239, max(heart_data$Cholesterol, na.rm = TRUE)),
  labels = c("Low", "Borderline", "High"),
  right = TRUE
)
summary(heart_data$Cholesterol_level)


colnames(heart_data)
View(heart_data) # Opens the spreadsheet-like viewer in RStudio
head(heart_data)  # Shows first 6 rows of all columns


#______________________________________________________________


table(heart_data$HeartDisease)
majority_class <- heart_data %>% filter(HeartDisease == names(which.max(table(HeartDisease))))
minority_class <- heart_data %>% filter(HeartDisease == names(which.min(table(HeartDisease))))

majority_class_undersampled <- majority_class %>% sample_n(nrow(minority_class))
balanced_data_under <- bind_rows(minority_class, majority_class_undersampled)
balanced_data_under <- balanced_data_under %>% sample_frac(1)
table(balanced_data_under$HeartDisease)


minority_class_oversampled <- minority_class %>%
  sample_n(nrow(majority_class), replace = TRUE)
balanced_data_over <- bind_rows(majority_class, minority_class_oversampled)
balanced_data_over <- balanced_data_over %>% sample_frac(1)
table(balanced_data_over$HeartDisease)


heart_data <- balanced_data_over



age_gender_summary <- heart_data %>%
  group_by(Gender) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = mfv(Age, na_rm = TRUE)[1]
  )
age_gender_summary



age_hr_summary <- heart_data %>%
  group_by(Heart_Rate) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = mfv(Age, na_rm = TRUE)[1]
  )
age_hr_summary


age_gender_spread <- heart_data %>%
  group_by(Gender) %>%
  summarise(
    Min_Age = min(Age),
    Max_Age = max(Age),
    Range_Age = paste0(min(Age), "–", max(Age)),
    IQR_Age = IQR(Age),
    Variance_Age = var(Age),
    SD_Age = sd(Age)
  )
age_gender_spread




min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

heart_data$Age <- min_max(heart_data$Age)
head(heart_data$Age)  

heart_data$Cholesterol <- min_max(heart_data$Cholesterol)
head(heart_data$Cholesterol)  


heart_data$BloodPressure <- as.numeric(heart_data$BloodPressure)

heart_data$BloodPressure <- min_max(heart_data$BloodPressure)
head(heart_data$BloodPressure)  

heart_data$QuantumPatternFeature <- min_max(heart_data$QuantumPatternFeature)
head(heart_data$QuantumPatternFeature)  







summary(heart_data)
train_index <- sample(1:nrow(heart_data), size = 0.8 * nrow(heart_data))
train_data <- heart_data[train_index, ]
test_data <- heart_data[-train_index, ]
dim(train_data)
dim(test_data)





#_______________________________________________________________________________











# Install related packages 
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pheatmap")
install.packages("e1071")

# Load required libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization
library(e1071)

# Load the data
data <- read.csv("C:\\Users\\mingr\\OneDrive\\Documents\\titanic.csv")

## PART 2: EDA
### 1. Descriptive Statistics & 2. Data Cleaning
# View the first few rows
head(data)

# View the data has how many columns and rows 
nrow(data)
ncol(data)

# Summary statistics for the entire data
summary(data)

# Replace empty strings with NA
data[data == ""] <- NA
# Check for missing values
colSums(is.na(data))
# drop column 'Cabin' as it has too many missing values
data <- select(data, -Cabin)
# drop missing values
data <- na.omit(data)
# Confirm the dataset no longer has missing values in these columns
sum(is.na(data$Age))      # Should return 0
sum(is.na(data$Embarked)) # Should return 0

# Check duplicate rows
duplicated_rows <- data[duplicated(data), ]
duplicated_rows

# Check outliers for Age
boxplot(data$Age, main = "Boxplot of Age", ylab = "Age", col = "lightblue")
outliers_age <- boxplot.stats(data$Age)$out
outliers_age
cat("Number of outliers for age: ", length(outliers_age), "\n")
# skewness and kurtosis for Age
skewness(data$Age, na.rm = TRUE)  # Skewness > 0: right-skewed, < 0: left-skewed
kurtosis(data$Age, na.rm = TRUE)  # Kurtosis > 3: heavy tails

# Check outliers for Fare
boxplot(data$Fare, main = "Boxplot of Fare", ylab = "Fare", col = "lightblue")
outliers_fare <- boxplot.stats(data$Fare)$out
outliers_fare
cat("Number of outliers for fare: ", length(outliers_fare), "\n")
# Skewness and kurtosis for Fare
skewness(data$Fare, na.rm = TRUE)  # Skewness > 0: right-skewed, < 0: left-skewed
kurtosis(data$Fare, na.rm = TRUE)  # Kurtosis > 3: heavy tails

# Summary statistics for the cleaned data
summary(data)
# Save the cleaned data
write.csv(data, "C:/Users/User/Downloads/titanic_cleaned.csv", row.names = FALSE)

### 3. Univariate Analysis

## a. Descriptive Statistics for Age
# Summary statistics for Age
summary(data$Age)

# Variance, Standard Deviation, and IQR for Age
var(data$Age, na.rm = TRUE) 
sd(data$Age, na.rm = TRUE)     
IQR(data$Age, na.rm = TRUE) 

# Histogram for Age
hist(data$Age, breaks = 20, col = "pink", border = "white",
     main = "Histogram of Age", xlab = "Age", ylab = "Frequency")

# Adding Mean, Median, and Mode lines
mean_age <- round(mean(data$Age, na.rm = TRUE), 2)
median_age <- round(median(data$Age, na.rm = TRUE), 2)
mode_age <- round(as.numeric(names(sort(-table(data$Age)))[1]), 2)

abline(v = c(mean_age, median_age, mode_age), col = c("red", "blue", "green"), lwd = 2)
legend("topright", legend = c(paste("Mean:", mean_age), paste("Median:", median_age), paste("Mode:", mode_age)),
       col = c("red", "blue", "green"), lwd = 2) 

## b. Descriptive Statistics for Fare
# Summary statistics for Fare
summary(data$Fare)

# Variance, Standard Deviation, and IQR for Age
var(data$Fare, na.rm = TRUE) 
sd(data$Fare, na.rm = TRUE)     
IQR(data$Fare, na.rm = TRUE) 

# Histogram for Fare
hist(data$Fare, breaks = 20, col = "lightblue", border = "white",
     main = "Histogram of Fare", xlab = "Fare", ylab = "Frequency")

# Adding Mean, Median, and Mode lines
mean_fare <- round(mean(data$Fare, na.rm = TRUE), 2)
median_fare <- round(median(data$Fare, na.rm = TRUE), 2)
mode_fare <- round(as.numeric(names(sort(-table(data$Fare)))[1]), 2)

abline(v = c(mean_fare, median_fare, mode_fare), col = c("red", "blue", "green"), lwd = 2)
legend("topright", legend = c(paste("Mean:", mean_fare), paste("Median:", median_fare), paste("Mode:", mode_fare)),
       col = c("red", "blue", "green"), lwd = 2)

## c. Distribution of Survived

# Summary statistics for Age
summary(data$Survived)

# Variance, Standard Deviation, and IQR for Age
var(data$Age, na.rm = TRUE) 
sd(data$Age, na.rm = TRUE)     
IQR(data$Age, na.rm = TRUE) 

table(data$Survived)
prop.table(table(data$Survived))

# Bar Chart for Survived
survived_table <- table(data$Survived)
bar_positions <- barplot(survived_table,
                         main = "Survival Distribution",
                         col = c("red", "green"),
                         names.arg = c("Died", "Survived"),
                         ylab = "Count")
text(x = bar_positions, y = survived_table, labels = survived_table, pos = 3, cex = 0.8)

# Pie Chart for Survived
survived_table <- table(data$Survived)
pie(survived_table,
    labels = paste0(c("Died", "Survived"), " (", round(prop.table(survived_table) * 100, 1), "%)"),
    col = c("red", "green"),
    main = "Survival Proportion")

## d. Distribution of Sex
table(data$Sex)
prop.table(table(data$Sex))

# Bar Chart for Sex
sex_table <- table(data$Sex)
barplot(sex_table,
        main = "Gender Distribution",
        col = c("lightcoral", "lightblue"),
        ylab = "Count")
text(x = bar_positions, y = sex_table, labels = sex_table, pos = 3, cex = 0.8)

# Pie Chart for Sex
sex_table <- table(data$Sex)
pie(sex_table,
    labels = paste0(names(sex_table), " (", round(prop.table(sex_table) * 100, 1), "%)"),
    col = c("lightcoral", "lightblue"),
    main = "Sex Proportion")

## e. Distribution of Embarked
table(data$Embarked)
prop.table(table(data$Embarked))

# Bar Chart for Embarked
embarked_table <- table(data$Embarked)
barplot(embarked_table,
        main = "Port of Embarkation",
        col = c("lightblue", "lightgreen", "pink"),
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        ylab = "Count")
text(x = bar_positions, y = embarked_table, labels = embarked_table, pos = 3, cex = 0.8)

# Pie Chart for Embarked
embarked_table <- table(data$Embarked)
pie(embarked_table,
    labels = paste0(names(embarked_table), " (", round(prop.table(embarked_table) * 100, 1), "%)"),
    col = c("lightblue", "lightgreen", "pink"),
    main = "Embarked Proportion")

## f. Distribution of Passenger Class (Pclass)
table(data$Pclass)
prop.table(table(data$Pclass))

# Bar Chart for Pclass
pclass_table <- table(data$Pclass)
barplot(pclass_table,
        col = c("lightblue", "lightgreen", "pink"),
        main = "Passenger Class Distribution",
        xlab = "Pclass",
        ylab = "Count")
text(x = bar_positions, y = pclass_table, labels = pclass_table, pos = 3, cex = 0.8)

# Pie Chart for Pclass
pclass_table <- table(data$Pclass)
pie(pclass_table,
    labels = paste0(names(pclass_table), " (", round(prop.table(pclass_table) * 100, 1), "%)"),
    col = c("lightblue", "lightgreen", "pink"),
    main = "Passenger Class Proportion")

## g. Distribution of SibSp 
# No. of siblings / spouses aboard the Titanic
table(data$SibSp)
prop.table(table(data$SibSp))

# Bar Chart for SibSp
sibsp_table <- table(data$SibSp)
bar_positions <- barplot(sibsp_table,
                         col = c("lightblue", "lightcoral", "lightgreen", "purple", "cyan", "pink"),
                         main = "SibSp Distribution",
                         xlab = "Number of Siblings/Spouses",
                         ylab = "Count")
text(x = bar_positions, y = sibsp_table, labels = sibsp_table, pos = 3, cex = 0.8)

## h. Distribution of Parch
# No. of parents / children aboard the Titanic
table(data$Parch)
prop.table(table(data$Parch))

# Bar Chart for Parch
parch_table <- table(data$Parch)
bar_positions <- barplot(parch_table,
                         col = c("lightblue", "lightcoral", "lightgreen", "purple", "cyan", "pink", "orange"),
                         main = "Parch Distribution",
                         xlab = "Number of Parents/Children",
                         ylab = "Count")
text(x = bar_positions, y = parch_table, labels = parch_table, pos = 3, cex = 0.8)

### 4. Bivariate Analysis
## Correlation Matrix for Numerical Variables
# Select numerical columns
num_cols <- data[, sapply(data, is.numeric)]

# Correlation matrix
cor_matrix <- cor(num_cols, use = "complete.obs")
print(cor_matrix)

## a. Survival Status by Age Distribution 
# Correlation between Age and Survived
cor(data$Age, data$Survived, use = "complete.obs")

# Boxplot of Age by Survived
boxplot(Age ~ Survived, data = data,
        main = "Age Distribution by Survival Status",
        xlab = "Survived (0 = No, 1 = Yes)",
        ylab = "Age",
        col = c("red", "green"))

## b. Survival Status by Age Group
# Create age groups
data$AgeGroup <- cut(data$Age, 
                     breaks = c(0, 12, 18, 35, 60, 100), 
                     labels = c("Child", "Teen", "Young Adult", "Adult", "Senior"))

# Calculate survival rate by age group
survival_age_group <- aggregate(Survived ~ AgeGroup, data = data, FUN = mean)
print(survival_age_group)

# Barplot for survival rate by age group
bar_positions <- barplot(survival_age_group$Survived,
                         names.arg = survival_age_group$AgeGroup,
                         main = "Survival Rate by Age Group",
                         col = "purple",
                         ylab = "Survival Rate")
text(x = bar_positions, y = survival_age_group$Survived, labels = round(survival_age_group$Survived, 2), pos = 3,  cex = 0.8, col = "black")

## c. Survival Status by Fare Distribution 
# Correlation between Fare and Survived
cor_fare_survived <- cor(data$Fare, data$Survived, use = "complete.obs")
print("Correlation between Fare and Survived:")
print(cor_fare_survived)

# Boxplot of Fare by Survived
boxplot(Fare ~ Survived, data = data,
        main = "Fare Distribution by Survival Status",
        xlab = "Survived (0 = No, 1 = Yes)",
        ylab = "Fare",
        col = c("orange", "blue"))

## d. Survival Status by Pclass
# Pclass by Survival Status
# Calculate survival rate by Pclass and Sex
survival_pclass_sex <- aggregate(Survived ~ Pclass + Sex, data = data, FUN = mean)
print("Pclass by Survival Status:")
print(survival_pclass_sex)

# Survival Rate by Pclass
# Calculate survival rate by Pclass
survival_pclass <- aggregate(Survived ~ Pclass, data = data, FUN = mean)
print("Survival Rate by Pclass:")
print(survival_pclass)

# Barplot for survival rate by Pclass
bar_positions <- barplot(survival_pclass$Survived,
                         names.arg = survival_pclass$Pclass,
                         main = "Survival Rate by Pclass",
                         col = "purple",
                         ylab = "Survival Rate")
text(x = bar_positions, y = survival_pclass$Survived, labels = round(survival_pclass$Survived, 2), pos = 3, cex = 0.8, col = "black")

### 5. Multivariate Analysis

## a. Survival rates by Pclass and Sex

# Calculate survival rates by Pclass and Sex
survival_pclass_sex <- aggregate(Survived ~ Pclass + Sex, data = data, FUN = mean)
print(survival_pclass_sex)

# Separate survival rates for males and females
male_survival <- survival_pclass_sex$Survived[survival_pclass_sex$Sex == "male"]
female_survival <- survival_pclass_sex$Survived[survival_pclass_sex$Sex == "female"]

# Combine into a matrix for plotting
survival_matrix <- rbind(male_survival, female_survival)

# Bar chart
bar_positions <- barplot(survival_matrix, 
                         beside = TRUE, 
                         col = c("lightblue", "pink"), 
                         main = "Survival Rates by Pclass and Sex", 
                         xlab = "Passenger Class", 
                         ylab = "Survival Rate", 
                         names.arg = unique(survival_pclass_sex$Pclass), 
                         legend.text = c("Male", "Female"), 
                         args.legend = list(title = "Sex", x = "topright"))
text(x = bar_positions, y = survival_matrix, labels = round(survival_matrix, 2), pos = 3, cex = 0.8)

## b. Survival rates by Age Group and Sex

# Create a grouped table for AgeGroup, Sex, and Survived
age_sex_survival <- table(data$AgeGroup, data$Sex, data$Survived)
print(age_sex_survival)

# Convert table to proportions
prop.table(age_sex_survival, margin = c(1, 2))

# Calculate survival rates by AgeGroup and Sex
survival_agegroup_sex <- aggregate(Survived ~ AgeGroup + Sex, data = data, FUN = mean)

# Separate survival rates for males and females
male_survival <- survival_agegroup_sex$Survived[survival_agegroup_sex$Sex == "male"]
female_survival <- survival_agegroup_sex$Survived[survival_agegroup_sex$Sex == "female"]

# Combine into a matrix for plotting
survival_matrix <- rbind(male_survival, female_survival)

# Set colors and labels
bar_colors <- c("lightblue", "pink")
agegroup_labels <- unique(survival_agegroup_sex$AgeGroup)

# Bar chart with customizations
bar_positions <- barplot(
  survival_matrix,
  beside = TRUE,
  col = bar_colors,
  main = "Survival Rates by Age Group and Sex",
  xlab = "Age Group",
  ylab = "Survival Rate",
  names.arg = agegroup_labels,
  ylim = c(0, 1),
  legend.text = c("Male", "Female"),
  args.legend = list(title = "Sex", x = "topright", cex = 0.8)
)
text(x = bar_positions, y = survival_matrix, labels = paste0(round(survival_matrix * 100, 1), "%"), pos = 3, cex = 0.8, col = "black")


# Part 3: Data Visualization
# Demographics and Characteristics of Titanic Passengers # 
# Distribution of Passenger Age 
# Round off Age to the nearest integer
data$Age <- round(data$Age)

## Distribution of Passenger Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +  # Histogram
  geom_density(aes(y = ..count..), color = "red", size = 1) +  # Density line
  labs(
    title = "Distribution of Passenger Age",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

## Distribution of Sex
# Calculate percentage distribution of Sex
sex_distribution <- data %>%
  group_by(Sex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Bar plot with count and percentage labels
ggplot(sex_distribution, aes(x = Sex, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  labs(
    title = "Sex Distribution with Counts and Percentages",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()

## Analysis of Passenger Age and Sex 
# Create the density plot for Age distribution by Sex
ggplot(data, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Distribution of Passengers by Age and Sex", 
       x = "Age", 
       y = "Density") +
  scale_fill_manual(values = c("pink", "lightblue")) +  # Set custom colors for male and female
  theme_minimal()

# Box Plot for Age distribution by Sex
ggplot(data, aes(x = Sex, y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Sex", 
       x = "Sex", 
       y = "Age") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()

## Distribution of Family Size 
## Distribution of Number of Sibillings/Spouses abroad
# Calculate the total count for percentage calculation
total_count <- nrow(data)

# Create a bar plot for the number of siblings/spouses aboard with percentages
ggplot(data, aes(x = factor(SibSp))) +
  geom_bar(aes(y = (..count..)), fill = "steelblue", color = "black") +
  geom_text(
    stat = "count",
    aes(label = paste0(..count.., " (", round(..count.. / total_count * 100, 1), "%)")),
    vjust = -0.5, size = 4
  ) +
  labs(
    title = "Distribution of Number of Siblings/Spouses Aboard",
    x = "Number of Siblings/Spouses",
    y = "Count"
  ) +
  theme_minimal()

# Calculate the total count for percentage calculation
total_count <- nrow(data)

# Create a bar plot for the number of parents/children aboard
ggplot(data, aes(x = factor(Parch))) +
  geom_bar(aes(y = ..count..), fill = "coral", color = "black") +
  geom_text(
    stat = "count",
    aes(label = paste0(..count.., " (", round(..count.. / total_count * 100, 1), "%)")),
    vjust = -0.5, size = 4
  ) +
  labs(
    title = "Distribution of Number of Parents/Children Aboard",
    x = "Number of Parents/Children",
    y = "Count"
  ) +
  theme_minimal()

# Sum up the SibSp and Parch columns to create a new column for total family members
data$Number_of_Family_Members <- data$SibSp + data$Parch

# Create a bar chart for the distribution of Number of Family Members
ggplot(data, aes(x = Number_of_Family_Members)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Number of Family Members Aboard",
    x = "Number of Family Members",
    y = "Count"
  ) +
  theme_minimal()

## Distribution of passenger class (Pclass)
# Calculate count and percentage for Pclass
pclass_distribution <- data %>%
  group_by(Pclass) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 1),
    PclassLabel = factor(Pclass, labels = c("Upper Class", "Middle Class", "Lower Class"))
  )

# Bar plot with count and percentage labels
ggplot(pclass_distribution, aes(x = PclassLabel, y = Count, fill = PclassLabel)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("goldenrod", "gray", "lightblue")) +
  labs(
    title = "Distribution of passenger class",
    x = "passenger class",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Create a boxplot to visualize the number of family members by passenger class
ggplot(data, aes(x = factor(Pclass, labels = c("Upper Class", "Middle Class", "Lower Class")), y = Number_of_Family_Members, fill = factor(Pclass))) +
  geom_boxplot() +
  labs(
    title = "Number of Family Members by passenger class",
    x = "passenger class",
    y = "Number of Family Members"
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme_minimal()

# Create a boxplot for Age distribution by passenger class (Pclass)
ggplot(data, aes(x = factor(Pclass, labels = c("Upper Class", "Middle Class", "Lower Class")), y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by passenger class and Sex",
    x = "passenger class",
    y = "Age"
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +  # Assign colors for male and female
  theme_minimal()  # Clean theme for better readability

## Fare Analysis 
# Box plot of Fare by passenger class
ggplot(data, aes(x = factor(Pclass, labels = c("Upper Class", "Middle Class", "Lower Class")), y = Fare, fill = factor(Pclass))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Fare Distribution by passenger class",
    x = "passenger class",
    y = "Fare"
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme_minimal() 

# Create a scatter plot to analyze the relationship between Age, Fare, and Sex
ggplot(data, aes(x = Age, y = Fare, color = Sex)) +
  geom_point(alpha = 0.6, size = 3) + # Scatter plot points
  geom_smooth(method = "loess", aes(color = Sex), se = FALSE, size = 1) + # Trend lines for each sex
  scale_color_manual(values = c("blue", "pink"), labels = c("Male", "Female")) + # Custom colors for Sex
  labs(
    title = "Anal on the Titanic",
    x = "Age",
    y = "Fare",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top", # Position legend at the top
    plot.title = element_text(hjust = 0.5) # Center-align the title
  )

## Distribution of Port of Firstion
# Create a pie chart for the distribution of Embarked with renamed labels, percentage, and different colors for each port
embarked_count <- table(data$Embarked)
embarked_percentage <- prop.table(embarked_count) * 100

# Rename the levels
names(embarked_count) <- c("Cherbourg", "Queenstown", "Southampton")

# Define custom colors for each port
port_colors <- c("Cherbourg" = "lightcoral", "Queenstown" = "lightblue", "Southampton" = "lightgreen")

# Plot pie chart with renamed labels and percentage
pie(embarked_count, 
    labels = paste(names(embarked_count), "\n", 
                   round(embarked_percentage, 1), "%", 
                   " (", embarked_count, ")", sep = ""),
    col = port_colors[names(embarked_count)],  # Assign different color to each port
    main = "Pie Chart of Port of 1stion")


## Identify Key Factors Influencing the Survival Rates ## 
# Survival Based on Age and Sex
data$Survived <- factor(data$Survived, levels = c(0, 1), labels = c("Died", "Survived"))

## Survival count 
ggplot(data, aes(x = factor(Survived))) + 
  geom_bar(fill = c("red", "green")) +
  scale_x_discrete(labels = c("0" = "Died", "1" = "Survived")) +
  labs(title = "Survival Count", x = "Survived", y = "Count") +
  theme_minimal()

## Survival rate
# Calculate survival count and percentage
survival_data <- data %>%
  group_by(Survived) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create the pie chart with percentage labels
ggplot(survival_data, aes(x = "", y = percentage, fill = factor(Survived))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("red", "green"), labels = c("0" = "Died", "1" = "Survived")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Position labels in the middle
  labs(title = "Survival Rate", fill = "Survived") +
  theme_void() +
  theme(axis.text.x = element_blank())  # Hide x-axis labels

## Survival  based on Sex and Age 
# Survival based on Sex (Count)
ggplot(data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +  # Use dodge for count-based bars
  labs(
    title = "Survival Count Based on Sex",
    x = "Sex",
    y = "Count",
    fill = "Survived"
  ) +
  scale_fill_manual(values = c("red", "green"), labels = c("0" = "Died", "1" = "Survived")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Survival based on Age 
ggplot(data %>% filter(Survived == 'Died'), aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.6) + 
  labs(
    title = "Death Count Based on Age",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

ggplot(data %>% filter(Survived == 'Survived'), aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.6) + 
  labs(
    title = "Survived Count Based on Age",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# Plot survival based on Age and Sex with custom labels for Survived
ggplot(data, aes(x = Age, fill = factor(Sex))) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black", alpha = 0.6) +  # Use dodge for count-based bars
  facet_wrap(~ Survived, labeller = labeller(Survived = c("Died","Survived"))) +  # Relabel Survived status
  labs(
    title = "Survival Based on Age and Sex",
    x = "Age",
    y = "Count",
    fill = "Sex"
  ) +
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("Male", "Female")) +  # Set colors for Male and Female
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

# Box plot for Sex vs Age vs Survival
ggplot(data, aes(x = factor(Sex), y = Age, fill = factor(Survived))) +
  geom_boxplot()  + 
  labs(
    title = "Age Distribution by Sex and Survival",
    x = "Sex",
    y = "Age",
    fill = "Survived (0 = No, 1 = Yes)"
  ) +
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +
  theme_minimal()

# Survival based on family size
ggplot(data, aes(x = factor(Number_of_Family_Members), fill = factor(Survived))) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +  # Bar chart for survival count
  labs(
    title = "Survival Count Based on Family Size",
    x = "Family Size",
    y = "Count",
    fill = "Survival Status"
  ) +
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +  # Color based on survival status
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "top"  # Position the legend at the top
  )

## Survival by passenger class 
ggplot(data, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival Count by passenger class",
    x = "passenger class (Pclass)",
    y = "Count"
  ) +
  scale_x_discrete(labels = c("Upper Class", "Middle Class", "Lower Class")) +  # Rename passenger class labels
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +
  theme_minimal()

## Fare Distribution by Survival
ggplot(data, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(
    title = "Fare Distribution by Survival",
    x = "Survival Status",  # Adjusted axis label
    y = "Fare"
  ) +
  scale_x_discrete(labels = c("Died", "Survived")) +  
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +  # Color based on survival status
  theme_minimal()

## Fare based on Survival
ggplot(data, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(
    title = "Fare Distribution Based on Survival",
    x = "Survival Status",
    y = "Fare",
    fill = "Survival Status"
  ) +
  scale_x_discrete(labels = c("Died", "Survived")) + 
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +  # Color based on survival
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14)  # Adjust axis title size
  )

## Fare based on Survival and passenger class 
ggplot(data, aes(x = factor(Pclass), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(
    title = "Fare Distribution Based on Survival and passenger class",
    x = "passenger class",
    y = "Fare",
    fill = "Survival Status"
  ) +
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +  # Color based on survival
  scale_x_discrete(labels = c("Upper Class", "Middle Class", "Lower Class")) +  # Rename passenger class labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14)  # Adjust axis title size
  )

## Survival Count base on Port of Embarkation 
ggplot(data, aes(x = factor(Embarked), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival Count by Port of Embarkation",
    x = "Port of Embarkation",
    y = "Count",
    fill = "Survived"
  ) +
  scale_x_discrete(labels = c("C" = "Cherbourg", "Q" = "Queenstown", "S" = "Southampton")) +
  scale_fill_manual(values = c("red", "green"), labels = c("Died", "Survived")) +
  theme_minimal()

## Scatterplot of Age vs Fare by Survival
ggplot(data, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatterplot of Age vs Fare by Survival", x = "Age", y = "Fare") +
  scale_color_manual(values = c("red", "green"), labels = c("Died", "Survived")) +
  theme_minimal()

## Heatmap ##
# Ensure the columns are numeric for correlation (convert if necessary)
data$Age <- as.numeric(data$Age)
data$SibSp <- as.numeric(data$SibSp)
data$Parch <- as.numeric(data$Parch)
data$Fare <- as.numeric(data$Fare)
data$Survived <- as.numeric(data$Survived)

# Check for constant columns (standard deviation = 0)
constant_vars <- sapply(data[, c("Age", "SibSp", "Parch", "Fare", "Sex", "Embarked", "Survived")], sd)
constant_vars <- names(constant_vars[constant_vars == 0])

# Print the constant variables
if(length(constant_vars) > 0) {
  cat("The following variables have constant values and will be excluded from the correlation calculation:\n")
  print(constant_vars)
}

# Remove constant variables from the data before calculating correlation
data_clean <- data %>%
  select(-one_of(constant_vars))

# Calculate the correlation matrix for the cleaned data
cor_matrix <- data_clean %>%
  select(Age, SibSp, Parch, Fare, Survived) %>%
  cor(use = "complete.obs")  # 'complete.obs' excludes NA values from the correlation calculation

# Check if there are any NA, NaN, or Inf values in the correlation matrix
if(any(is.na(cor_matrix)) | any(is.nan(cor_matrix)) | any(is.infinite(cor_matrix))) {
  cor_matrix[is.na(cor_matrix)] <- 0
  cor_matrix[is.nan(cor_matrix)] <- 0
  cor_matrix[is.infinite(cor_matrix)] <- 0
}

# Create a heatmap
pheatmap(cor_matrix, 
         display_numbers = TRUE,  # Show correlation values in each cell
         color = colorRampPalette(c("blue", "white", "red"))(50),  # Color gradient
         main = "Correlation Heatmap of Numeric Variables in Titanic data",
         cluster_rows = TRUE,  # Cluster rows
         cluster_cols = TRUE,  # Cluster columns
         number_color = "black"  # Color of the numbers
)


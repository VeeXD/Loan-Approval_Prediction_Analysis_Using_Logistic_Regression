library(readr)
library(readr)
library(dplyr)
library(ggplot2)
loan <- read_csv("Loan_Train.csv")
View(loan)

summary(loan)
colSums(is.na(loan))

## HANDLING MISSING VALUES.
# Creating a Function to get "Mode".
get_mode <- function(v) {
  unique_v <- unique(v)
  unique_v[which.max(tabulate(match(v, unique_v)))]
}
# calculating mode and filling in Gender
mode_value <- get_mode(loan$Gender)
loan$Gender[is.na(loan$Gender)] <- mode_value

# calculating mode and filling in married
mode_value <- get_mode(loan$Married)
loan$Married[is.na(loan$Married)] <- mode_value

# calculating mode and filling in Dependents
mode_value <- get_mode(loan$Dependents)
loan$Dependents[is.na(loan$Dependents)] <- mode_value

# calculating mode and filling in Self_Employed
mode_value <- get_mode(loan$Self_Employed)
loan$Self_Employed[is.na(loan$Self_Employed)] <- mode_value

# calculating mode and filling in Credit_History
mode_value <- get_mode(loan$Credit_History)
loan$Credit_History[is.na(loan$Credit_History)] <- mode_value

# calculating Mdeian and filling in Self_Employed
loan$LoanAmount[is.na(loan$LoanAmount)] <- median(loan$LoanAmount, na.rm = TRUE)

# calculating Mdeian and filling in Self_Employed
loan$Loan_Amount_Term[is.na(loan$Loan_Amount_Term)] <- median(loan$Loan_Amount_Term, na.rm = TRUE)

#checking if any Duplicates
duplicates <- duplicated(loan)
loan[duplicates, ]    # Display only the duplicate rows

#Normalising numerical data
loan$ApplicantIncome <- scale(loan$ApplicantIncome)
loan$CoapplicantIncome <- scale(loan$CoapplicantIncome)
loan$LoanAmount <- scale(loan$LoanAmount)
loan$Loan_Amount_Term <- scale(loan$Loan_Amount_Term)

# appears to have some inconsistencies in data like lona term has negative values whish is not possible.
# there are many outliers in loanamount, co applicant income, applicant income. 
# removing the negative values in loanterm
# EDA

# applying Log transformation to reduce outliers.
Loan <- loan[loan$Loan_Amount_Term >= 0, ]
# Box plot for Loan_Amount_Term
boxplot(Loan$Loan_Amount_Term, 
        main = "Box Plot of Loan Amount Term", 
        ylab = "Loan Amount Term", 
        col = "lightyellow")


# applying Log transformation to reduce outliers.
Loan <- loan[loan$LoanAmount >= 0, ] # To remove the negative values.
Loan$LoanAmount_log <- log(Loan$LoanAmount) # Apply log transformation to LoanAmount to reduce the effect of outliers
# Box plot for LoanAmount
boxplot(Loan$LoanAmount_log, 
        main = "Box Plot of Loan Amount", 
        ylab = "Loan Amount", 
        col = "lightcoral")

# applying Log transformation to reduce outliers.
Loan <- loan[loan$ApplicantIncome >= 0, ] # To remove the negative values.
Loan$ApplicantIncome_log <- log(Loan$ApplicantIncome) 
# Box plot for ApplicantIncome
boxplot(Loan$ApplicantIncome_log, 
        main = "Box Plot of Applicant Income", 
        ylab = "Applicant Income", 
        col = "lightblue")

# applying Log transformation to reduce outliers.
Loan <- loan[loan$CoapplicantIncome >= 0, ] # To remove the negative values.
Loan$CoapplicantIncome_log <- log(Loan$CoapplicantIncome) 
# Box plot for CoapplicantIncome
boxplot(Loan$CoapplicantIncome_log, 
        main = "Box Plot of Coapplicant Income", 
        ylab = "Coapplicant Income", 
        col = "lightgreen")

# Box plot for Credit History
boxplot(Loan$Credit_History, 
        main = "Box Plot of Credit History", 
        ylab = "Credit History", 
        col = "lightpink")

# finding the relations
set.seed(123)

# Randomly sample 50% of the dataset
subset_data <- Loan[sample(nrow(Loan), nrow(Loan) * 0.1), ]

# View the subset to confirm
head(subset_data)

plot(subset_data$ApplicantIncome, subset_data$CoapplicantIncome,
     main = "Scatter Plot of Applicant Income vs Coapplicant Income",
     xlab = "Applicant Income",
     ylab = "Coapplicant Income",
     col = "blue",
     pch = 17)

# Logistic Regression for Loan Approval Prediction
Loan$Loan_Status <- ifelse(Loan$Loan_Status == "Y", 1, 0)

# Build logistic regression model
logistic_model <- glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, 
                      data = Loan, family = "binomial")

# Summary of the logistic model
summary(logistic_model)

# Predict the probability of loan approval
Loan$Approval_Probability <- predict(logistic_model, Loan, type = "response")
head(Loan[, c("Loan_ID", "Approval_Probability")])

# Scatter plots to visualize relationships

# 1. Applicant Income vs Loan Approval Probability
plot(Loan$ApplicantIncome, Loan$Approval_Probability,
     main = "Applicant Income vs Loan Approval Probability",
     xlab = "Applicant Income", ylab = "Approval Probability", col = "blue")
abline(lm(Approval_Probability ~ ApplicantIncome, data = Loan), col = "blue")

# 2. Coapplicant Income vs Loan Approval Probability
plot(Loan$CoapplicantIncome, Loan$Approval_Probability,
     main = "Coapplicant Income vs Loan Approval Probability",
     xlab = "Coapplicant Income", ylab = "Approval Probability", col = "green")
abline(lm(Approval_Probability ~ CoapplicantIncome, data = Loan), col = "green")

# 3. Loan Amount vs Loan Approval Probability
plot(Loan$LoanAmount, Loan$Approval_Probability,
     main = "Loan Amount vs Loan Approval Probability",
     xlab = "Loan Amount", ylab = "Approval Probability", col = "red")

# 4. Loan Term vs Loan Approval Probability
plot(Loan$Loan_Amount_Term, Loan$Approval_Probability,
     main = "Loan Term vs Loan Approval Probability",
     xlab = "Loan Term", ylab = "Approval Probability", col = "orange")

# Bar plots to visualize categorical variable relationships

# 5. Credit History vs Loan Approval Probability
ggplot(Loan, aes(x = factor(Credit_History), y = Approval_Probability)) + 
  geom_bar(stat = "summary", fun = mean, fill = "lightblue") + 
  labs(title = "Credit History vs Loan Approval Probability", x = "Credit History", y = "Approval Probability")

# 6. Property Area vs Loan Approval Probability
ggplot(Loan, aes(x = factor(Property_Area), y = Approval_Probability)) + 
  geom_bar(stat = "summary", fun = mean, fill = "lightgreen") + 
  labs(title = "Property Area vs Loan Approval Probability", x = "Property Area", y = "Approval Probability")

# 7. Education vs Loan Approval Probability
ggplot(Loan, aes(x = factor(Education), y = Approval_Probability)) + 
  geom_bar(stat = "summary", fun = mean, fill = "lightcoral") + 
  labs(title = "Education vs Loan Approval Probability", x = "Education", y = "Approval Probability")

# 8. Self Employment vs Loan Approval Probability
ggplot(Loan, aes(x = factor(Self_Employed), y = Approval_Probability)) + 
  geom_bar(stat = "summary", fun = mean, fill = "lightyellow") + 
  labs(title = "Self-Employed vs Loan Approval Probability", x = "Self-Employed", y = "Approval Probability")


# Predicted probabilities (using the logistic regression model already trained)
predicted_probs <- predict(logistic_model, newdata = Loan, type = "response")

# Check summary statistics of the predictions
summary(predicted_probs)

# Plot the distribution of predicted probabilities
hist(predicted_probs, breaks = 20, main = "Distribution of Predicted Probabilities", 
     xlab = "Predicted Probability", col = "lightblue")





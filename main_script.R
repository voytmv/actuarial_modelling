# 1. Explorative data analysis using ggplot,plotly
# 2. Data preprocessing using dplyr 

#To perform an explorative data analysis and predictive modeling on simulated data for 
#variables: annuities,incorporating biometric risks, policyholder behaviors. 
#The project will demonstrate proficiency in data manipulation, statistical analysis, and reporting using R.

# Annuities are financial products sold by insurance companies 

# Biometric risks refer to the uncertainties related to the life and health of individuals, 
# which can significantly impact financial products like life insurance and annuities

#Policyholder behaviors can significantly affect the financial outcomes of insurance products.
# These behaviors might include lapse rates (the rate at which policyholders stop paying
#premiums and allow their policies to expire without claim), surrender behaviors (choosing to terminate 
#the policy early and take out the cash value), and choice of different payout options.

# Load necessary libraries
library(dplyr)
library(ggplot2)



# Set the number of policies
n_policies <- 10000
#Policyholder Demographics: Age, gender, start date of the policy.
#Policy Details: Policy amount, duration, type of annuity.
#Biometric Risks:
# Mortality: Likelihood of the policyholder passing away during the policy term.
#Morbidity: Risk of falling ill or becoming disabled.
#Lapse: Probability that the policyholder will surrender the policy.
# Persistency: Probability that the policyholder will continue the policy until the end.

# Simulate data using normal distribution for age and biometric risks, log normal for policy amount
set.seed(123)  # for reproducibility
policy_data <- tibble(
  policy_id = 1:n_policies,
  age = round(rnorm(n_policies, mean = 40, sd = 10)),
  gender = sample(c("Male", "Female"), n_policies, replace = TRUE),
  start_date = as.Date('2020-01-01') + sample(0:365, n_policies, replace = TRUE),
  policy_amount = exp(rnorm(n_policies, log(50000), 0.5)),
  duration_years = sample(15:25, n_policies, replace = TRUE),
  mortality_prob = plogis(rnorm(n_policies, 0, 0.3)),
  morbidity_prob = plogis(rnorm(n_policies, 0, 0.3)),
  lapse_prob = plogis(rnorm(n_policies, -1, 0.5)),
  persistency_prob = 1 - lapse_prob  
)

# check the data
head(policy_data)

# data exploration and cleaning

# Summary statistics
summary(policy_data) # hence make some assumptions

# Check for missing values
sum(is.na(policy_data)) # no n/a, that's obvious because we have generated it.

# Visualizations
# Histogram for Age
ggplot(policy_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Distribution of Policyholder Age") # age is indeed normally distributed

# Bar Chart for Gender
ggplot(policy_data, aes(x = gender)) +
  geom_bar(fill = "orange", color = "black") +
  ggtitle("Gender Distribution") # gender is indeed equally distributed

# Scatter Plot for Age vs. Mortality Probability
ggplot(policy_data, aes(x = age, y = mortality_prob)) +
  geom_point(alpha = 0.5) +
  ggtitle("Age vs. Mortality Probability") # hard to say but the mean age has the probability of mortality



# Logistic Regression for Mortality Probability
mortality_model <- glm(mortality_prob ~ age + gender + policy_amount, 
                       family = binomial(link = "logit"), data = policy_data)
summary(mortality_model)

# Visualization of Mortality Model Effects
ggplot(policy_data, aes(x = age, y = mortality_prob, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  ggtitle("Mortality Probability by Age and Gender")

# Logistic Regression for Lapse Probability
lapse_model <- glm(lapse_prob ~ age + gender + policy_amount + duration_years, 
                   family = binomial(link = "logit"), data = policy_data)
summary(lapse_model)

# Visualization of Lapse Model Effects
ggplot(policy_data, aes(x = duration_years, y = lapse_prob, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  ggtitle("Lapse Probability by Policy Duration and Gender")

# Example new data
new_data <- tibble(
  age = c(90, 50),
  gender = c("Male", "Female"),
  policy_amount = c(55000, 60000)
)

# Predict using the mortality model
new_data$mortality_pred <- predict(mortality_model, newdata = new_data, type = "response")
print(new_data)


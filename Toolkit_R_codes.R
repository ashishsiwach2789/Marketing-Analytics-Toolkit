# Load necessary libraries
library(ggplot2)

# Simulate example customer data with demographic features
set.seed(123)
n <- 300
# Simulate customer age (years) and annual income (in dollars)
customer_data <- data.frame(
  age = round(rnorm(n, mean = 45, sd = 12)),
  income = round(rnorm(n, mean = 60000, sd = 15000))
)

# Perform K-Means clustering to segment into 4 clusters
k <- 4
set.seed(123)  # for reproducibility of clusters
kmeans_result <- kmeans(customer_data, centers = k)

# Add the cluster assignment to the data
customer_data$cluster <- as.factor(kmeans_result$cluster)

# Create a scatter plot to visualize the clusters
cluster_plot <- ggplot(customer_data, aes(x = age, y = income, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "4-Cluster Diagram for Insurance Customer Segmentation",
    x = "Age (years)",
    y = "Annual Income ($)",
    color = "Cluster"
  ) +
  theme_minimal()

# Display the plot
print(cluster_plot)

# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate example data for 200 observations
n <- 200
# Generate random advertising spend (in thousands of dollars)
Google_Ads  <- runif(n, 100, 1000)
Meta_Ads    <- runif(n, 50, 500)
TV          <- runif(n, 200, 1200)
Print_media <- runif(n, 30, 300)

# Simulate website traffic as a response variable, assuming an underlying relationship:
# website_traffic = 10 + 0.5*Google_Ads + 0.3*Meta_Ads + 0.2*TV + 0.1*Print_media + noise
website_traffic <- 10 + 0.5 * Google_Ads + 0.3 * Meta_Ads + 0.2 * TV + 0.1 * Print_media + rnorm(n, mean = 0, sd = 10)

# Create a data frame
df <- data.frame(website_traffic, Google_Ads, Meta_Ads, TV, Print_media)

# Build the linear regression model (market response model)
model <- lm(website_traffic ~ Google_Ads + Meta_Ads + TV + Print_media, data = df)

# Display the model summary to view coefficients and statistical significance
summary(model)

# Extract the coefficients (excluding the intercept) for plotting
coefficients_matrix <- summary(model)$coefficients
coeff_df <- data.frame(
  Channel = rownames(coefficients_matrix)[-1],
  Coefficient = coefficients_matrix[-1, 1]
)

# Plot the coefficient estimates for each marketing channel
coef_plot <- ggplot(coeff_df, aes(x = Channel, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Effect of Marketing Channels on Website Traffic",
       x = "Marketing Channel",
       y = "Coefficient Estimate") +
  theme_minimal()

print(coef_plot)

# INSTALL NEW PACKAGES (if not already installed)
# install.packages("conjoint")
# install.packages("tidyverse")
# install.packages("ggplot2")

# LOAD REQUIRED PACKAGES
library(conjoint)
library(tidyverse)
library(ggplot2)

##########################
# SIMULATE THE CONJOINT DATA FOR VIVO MOBILE
##########################

# In this use case, the smartphone configuration has 2 attributes:
# - RAM: levels "6GB" (baseline) and "8GB" (+5 utility points)
# - ROM: levels "128GB" (baseline) and "256GB" (+10 utility points)

# Create the full factorial design (profiles) for the 2 attributes.
profiles <- expand.grid(
  RAM = factor(c("6GB", "8GB"), levels = c("6GB", "8GB")),
  ROM = factor(c("128GB", "256GB"), levels = c("128GB", "256GB"))
)
# Optional: Reorder profiles if necessary
profiles <- profiles[c(1, 2, 3, 4), ]

# Define the "true" mean preference score for each profile.
# Baseline intercept = 50, plus a utility boost of +5 for "8GB" and +10 for "256GB"
profiles$true_mean <- with(profiles,
                           50 +
                             ifelse(RAM == "8GB", 5, 0) +
                             ifelse(ROM == "256GB", 10, 0)
)

# Simulate respondent ratings.
# Assume each profile is rated by 30 respondents.
set.seed(123)
n_resp <- 30
df_sim <- profiles[rep(1:nrow(profiles), each = n_resp), ]
df_sim$response <- with(df_sim, true_mean + rnorm(nrow(df_sim), mean = 0, sd = 5))

# Create a ratings matrix (rows = respondents, columns = profiles)
rating_matrix <- matrix(df_sim$response, nrow = n_resp, byrow = FALSE)
rating_matrix <- as.data.frame(rating_matrix)
colnames(rating_matrix) <- paste0("Profile", 1:ncol(rating_matrix))

# Create the profile matrix (contains the attribute levels for each profile)
profile_data <- profiles[, c("RAM", "ROM")]
# Remove any extra columns (such as true_mean)
profile_data <- profile_data %>% select(RAM, ROM)

# Define the attribute levels in the order corresponding to the attributes.
# For RAM: "6GB" and "8GB"; for ROM: "128GB" and "256GB"
attr.level <- c("6GB", "8GB", "128GB", "256GB")
attr.level <- data.frame(attr.level)

##########################
# CONJOINT ANALYSIS
##########################

# Calculate the matrix of individual part-worth utilities.
# y = rating matrix; x = profile data; z = attribute levels
part_utilities <- caPartUtilities(y = rating_matrix, x = profile_data, z = attr.level)
print("Individual part-worth utilities for each respondent:")
print(part_utilities)
summary(part_utilities)

# Calculate overall attribute importance (returns percentage importance per attribute).
imp <- caImportance(y = rating_matrix, x = profile_data)
print("Attribute Importance (percentage):")
print(imp)

##########################
# PREPARE DATA FOR HEATMAP PLOTTING
##########################

# Create a data frame of individual utilities and add a Respondent ID.
# Remove the intercept (first) column if present.
part_df <- data.frame(Respondent = 1:n_resp, part_utilities[,-1])
# View(part_df)

# Reshape the utilities from wide format to long format.
# (Each row: one respondent's estimated utility for an attribute level)
part_long <- part_df %>%
  pivot_longer(cols = -Respondent, names_to = "AttributeLevel", values_to = "Utility") %>%
  mutate(Utility = as.numeric(Utility))

# If your column names in part_utilities are coded as, for example, "RAM8GB", "ROM256GB",
# you can split them into separate Attribute and Level columns.
# Here we assume the naming follows the format "AttributeLevel" (e.g., "RAM8GB").
part_long <- part_long %>%
  mutate(
    Attribute = str_extract(AttributeLevel, "^[A-Za-z]+"),
    Level = str_replace(AttributeLevel, "^[A-Za-z]+", "")
  )

##########################
# PLOT THE HEATMAP: INDIVIDUAL PART-WORTH UTILITIES BY RESPONDENT
##########################

heatmap_plot <- ggplot(part_long, aes(x = Level, y = factor(Respondent), fill = Utility)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap: Individual Part-Worth Utilities for VIVO Mobile",
       x = "Attribute Level",
       y = "Respondent",
       fill = "Utility") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the heatmap
print(heatmap_plot)

##########################
# OPTIONAL: PLOT OVERALL ATTRIBUTE IMPORTANCE AS A BAR CHART
##########################

# Create a data frame from the attribute importance results.
imp_df <- data.frame(Attribute = names(imp), Importance = imp)
importance_plot <- ggplot(imp_df, aes(x = reorder(Attribute, Importance), y = Importance)) +
  geom_col(fill = "skyblue") +
  geom_point(aes(y = Importance), size = 4, color = "darkblue") +
  geom_text(aes(label = round(Importance, 1)), vjust = -0.5, fontface = "bold") +
  coord_flip() +
  labs(title = "Attribute Importance for VIVO Mobile Smartphone Configuration",
       x = "Attribute",
       y = "Importance (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Display the bar chart of attribute importance
print(importance_plot)

# ---------------------------
# SIMULATE THE DATA
# ---------------------------
set.seed(18)
n <- 300

# Simulate predictor variables
gender <- factor(sample(c("Male", "Female"), n, replace = TRUE))
monthly_payment <- round(runif(n, 50, 150), 1)      # Payment in dollars
data_limit <- round(runif(n, 1, 50), 1)               # Data limit in GB
service <- round(runif(n, 1, 10), 1)                  # Service rating (1-10)
Ott_subscriptions <- factor(sample(c("Yes", "No"), n, replace = TRUE))
customer_support <- round(runif(n, 1, 10), 1)         # Customer support rating (1-10)
payment_type <- factor(sample(c("Prepaid", "Postpaid"), n, replace = TRUE))

# Define true coefficients for simulation
beta0 <- -2                           # intercept
beta_monthly_payment <- 0.03          # increase in churn probability with higher monthly payment
beta_data_limit <- -0.05              # more data reduces churn probability
beta_service <- -0.10                 # better service reduces churn
beta_customer_support <- -0.10        # better support reduces churn

# For categorical variables we use treatment coding (baseline level effect = 0)
# Assume baseline for gender is Male and for Ott_subscriptions is "No", and for payment_type is "Prepaid"
beta_gender_Female <- 0.20            # slight increase in churn for Female
beta_Ott_Yes <- -0.70                 # having OTT subscription reduces churn
beta_payment_Postpaid <- 0.30         # Postpaid users more likely to churn relative to Prepaid

# Calculate the linear predictor (eta) for each observation
eta <- beta0 + 
  beta_monthly_payment * monthly_payment +
  beta_data_limit * data_limit +
  beta_service * service +
  beta_customer_support * customer_support +
  ifelse(gender == "Female", beta_gender_Female, 0) +
  ifelse(Ott_subscriptions == "Yes", beta_Ott_Yes, 0) +
  ifelse(payment_type == "Postpaid", beta_payment_Postpaid, 0)

# Convert the linear predictor to churn probabilities via the logistic function
p_churn <- exp(eta) / (1 + exp(eta))

# Simulate the churn outcome using a Bernoulli distribution
churn <- rbinom(n, 1, p_churn)

# Combine all variables into a data frame
df <- data.frame(
  churn,
  gender,
  monthly_payment,
  data_limit,
  service,
  Ott_subscriptions,
  customer_support,
  payment_type
)

# ---------------------------
# FIT THE LOGISTIC REGRESSION MODEL
# ---------------------------
lr <- glm(churn ~ gender + monthly_payment + data_limit + 
            service + Ott_subscriptions + customer_support + payment_type,
          family = binomial(link = "logit"),
          data = df)
summary(lr)

# ---------------------------
# EXTRACT COEFFICIENTS (EXCLUDING THE INTERCEPT)
# ---------------------------
coef_matrix <- summary(lr)$coefficients
coef_df <- data.frame(Predictor = rownames(coef_matrix), Coefficient = coef_matrix[, "Estimate"])
coef_df <- coef_df[coef_df$Predictor != "(Intercept)", ]  # Remove the intercept

# ---------------------------
# PLOT COEFFICIENTS ON A HORIZONTAL BAR CHART USING ggplot2
# ---------------------------
library(ggplot2)

ggplot(coef_df, aes(x = reorder(Predictor, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Create a horizontal bar chart
  labs(title = "Predictor Coefficient Estimates on Churn",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


# Load necessary package
library(ggplot2)

# ---------------------------
# SIMULATE A SIMPLE DATASET (if needed)
# ---------------------------
set.seed(123)
n <- 300
# For demonstration, simulate a binary churn variable (0: Not Churn, 1: Churn)
churn <- rbinom(n, 1, 0.3)  # assume 30% churn rate
# Create a data frame
df <- data.frame(churn = churn)

# Optionally, convert churn variable to a factor with descriptive labels.
df$churn <- factor(df$churn, levels = c(0, 1), labels = c("Not Churn", "Churn"))

# ---------------------------
# PLOT THE BAR CHART FOR CHURN VS NOT CHURN
# ---------------------------
ggplot(df, aes(x = churn)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Customer Choices: Churn vs Not Churn",
       x = "Customer Choice",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# ---------------------------------
# MARKET RESPONSE MODEL
# ---------------------------------
# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate example data for 200 observations
n <- 200
# Generate random advertising spend (in thousands of dollars)
Google_Ads  <- runif(n, 100, 1000)
Meta_Ads    <- runif(n, 50, 500)
TV          <- runif(n, 200, 1200)
Print_media <- runif(n, 30, 300)

# Simulate website traffic as a response variable, assuming an underlying relationship:
# website_traffic = 10 + 0.5*Google_Ads + 0.3*Meta_Ads + 0.2*TV + 0.1*Print_media + noise
website_traffic <- 10 + 0.5 * Google_Ads + 0.3 * Meta_Ads + 0.2 * TV + 0.1 * Print_media + rnorm(n, mean = 0, sd = 10)

# Create a data frame
df <- data.frame(website_traffic, Google_Ads, Meta_Ads, TV, Print_media)

# Build the linear regression model (market response model)
model <- lm(website_traffic ~ Google_Ads + Meta_Ads + TV + Print_media, data = df)

# Display the model summary to view coefficients and statistical significance
summary(model)

# Extract the coefficients (excluding the intercept) for plotting
coefficients_matrix <- summary(model)$coefficients
coeff_df <- data.frame(
  Channel = rownames(coefficients_matrix)[-1],
  Coefficient = coefficients_matrix[-1, 1]
)

# Plot the coefficient estimates for each marketing channel
coef_plot <- ggplot(coeff_df, aes(x = Channel, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Effect of Marketing Channels on Website Traffic",
       x = "Marketing Channel",
       y = "Coefficient Estimate") +
  theme_minimal()

print(coef_plot)
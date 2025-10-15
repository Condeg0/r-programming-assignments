# Load required libraries
# install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# --- Step 1: Data Ingress and Preprocessing ---

# Load the built-in mtcars dataset
data(mtcars)
df <- mtcars # Assign to a standard data frame name for clarity

# Convert categorical variables to factors for accurate modeling/plotting
df$am <- factor(df$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
df$cyl <- factor(df$cyl)
cat("Data loaded and categorical variables converted to factors.\n")

# --- Step 2: Feature Engineering (Creating a Key Performance Metric) ---

# Create a new feature: Horsepower per Unit Weight (hp/wt)
# This serves as an engineered performance metric crucial for quantitative analysis.
df <- df %>%
  mutate(
    HP_Per_Weight = hp / wt,
    Car_Name = rownames(mtcars) # Add car names for labeling
  )

cat("Engineered feature 'HP_Per_Weight' created (HP divided by 1000s of lbs).\n")
print(head(df[, c("mpg", "wt", "hp", "HP_Per_Weight")]))

# --- Step 3: Statistical Modeling (Linear Regression) ---

# Fit a linear model to predict Miles per Gallon (mpg) using Weight (wt),
# Horsepower (hp), and the new engineered feature (HP_Per_Weight).
# This tests the predictive power of the engineered metric.
model <- lm(mpg ~ wt + hp + HP_Per_Weight, data = df)

cat("\nLinear Regression Model Summary (Predicting MPG):\n")
print(summary(model))

# --- Step 4: Visualization (Scatter Plot with Regression Line) ---

# Plot the relationship between the key features and the target variable (mpg)
# Specifically visualize the strong inverse relationship between Weight (wt) and mpg,
# segmented by the transmission type (am).

p <- ggplot(df, aes(x = wt, y = mpg, color = am)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) + # Add linear regression line for each group
  labs(
    title = "MPG vs. Weight: Assessing Fuel Efficiency Risk by Transmission Type",
    subtitle = "Analysis using mtcars dataset. Regression line added for trend quantification.",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon (MPG)",
    color = "Transmission"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Save the plot to a file (required for the blog post "screenshots")
ggsave("mtcars_mpg_vs_wt_plot.png", plot = p, width = 8, height = 5)

cat("\nVisualization 'mtcars_mpg_vs_wt_plot.png' saved to working directory.\n")

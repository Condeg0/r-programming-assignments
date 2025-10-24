# Assignment #9: Visualization in R
# Rafael Condé Gomes
# LIS 4370 - R Programming for Data Science

# --- 0. Setup ---
# Install packages if they are not already installed
if (!require("Ecdat")) install.packages("Ecdat")
if (!require("lattice")) install.packages("lattice")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")

# Load libraries
library(Ecdat)
library(lattice)
library(ggplot2)
library(lubridate)

# Load and inspect the SP500 dataset
data("SP500", package = "Ecdat")
sp_df <- as.data.frame(SP500)

# Engineer features for analysis: Date and Year
# The dataset starts on the second trading day of 1981 and ends in 1991.
# Create a sequence of dates for plotting.
sp_df$Date <- seq.Date(from = as.Date("1981-01-02"), by = "day", length.out = nrow(sp_df))
sp_df$Year <- as.factor(year(sp_df$Date))

# Inspect the prepared data
head(sp_df)
str(sp_df)

# --- 1. Base R Graphics ---
# Objective: Create at least two plots using base R functions.

# Plot 1: Time series of S&P 500 daily returns
# This plot is essential for visualizing volatility clusters and market shocks.
plot(sp_df$Date, sp_df$r500,
     type = 'l',
     col = 'blue',
     main = "Base R: S&P 500 Daily Returns (1981-1991)",
     xlab = "Date",
     ylab = "Daily Return (r500)")
# Add a horizontal line at y=0 for reference
abline(h = 0, col = "red", lty = 2)
# Highlight the 1987 crash ("Black Monday")
crash_date <- as.Date("1987-10-19")
crash_return <- sp_df$r500[which.min(sp_df$r500)]


# Plot 2: Histogram of the distribution of daily returns
# This plot helps assess the statistical properties of returns, such as skewness and kurtosis.
hist(sp_df$r500,
     breaks = 100,
     col = 'gray',
     main = "Base R: Distribution of S&P 500 Daily Returns",
     xlab = "Daily Return (r500)",
     ylab = "Frequency")

# --- 2. Lattice Graphics ---
# Objective: Produce conditioned or multivariate plots.

# Plot 3: Box-and-whisker plot of returns conditioned by year
# This visualizes how the distribution and volatility of returns changed annually.
bwplot(r500 ~ Year,
       data = sp_df,
       main = "Lattice: Annual Distribution of S&P 500 Returns",
       xlab = "Year",
       ylab = "Daily Return (r500)",
       scales = list(x = list(rot = 45)))

# --- 3. ggplot2 Graphics ---
# Objective: Create layered visuals using the grammar of graphics.

# Plot 4: Faceted histogram of returns by year
# This provides a more detailed view of the annual return distributions than the boxplot.
ggplot(sp_df, aes(x = r500)) +
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "black") +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "ggplot2: Distribution of Daily Returns by Year",
       subtitle = "Faceting reveals shifts in volatility and tail events",
       x = "Daily Return (r500)",
       y = "Frequency") +
  theme_minimal()

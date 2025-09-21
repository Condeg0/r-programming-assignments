# Define patient data
Frequency     <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
BloodPressure <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176)
FirstAssess   <- c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1)    # bad=1, good=0
SecondAssess  <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)    # low=0, high=1
FinalDecision <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1)    # low=0, high=1

df_hosp <- data.frame(
  Frequency, BloodPressure, FirstAssess,
  SecondAssess, FinalDecision, stringsAsFactors = FALSE
)

# Inspect and handle missing values
summary(df_hosp)
df_hosp <- na.omit(df_hosp)

# Boxplots of BP by assessments and decision
boxplot(BloodPressure ~ FirstAssess,
        data = df_hosp,
        names = c("Good","Bad"),
        ylab = "Blood Pressure",
        main = "BP by First MD Assessment")

boxplot(BloodPressure ~ SecondAssess,
        data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Second MD Assessment")

boxplot(BloodPressure ~ FinalDecision,
        data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Final Decision")

# Histograms
hist(df_hosp$Frequency,
     breaks = seq(0, 1, by = 0.1),
     xlab = "Visit Frequency",
     main = "Histogram of Visit Frequency")

hist(df_hosp$BloodPressure,
     breaks = 8,
     xlab = "Blood Pressure",
     main = "Histogram of Blood Pressure")

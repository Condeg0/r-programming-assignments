library(ggplot2)

# Define vectors
Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC_poll   <- c(4, 62, 51, 21, 2, 14, 15)
CBS_poll   <- c(12, 75, 43, 19, 1, 21, 19)

# Create data frame
df_polls <- data.frame(Name, ABC_poll, CBS_poll)

head(df_polls)

# Compute summary stats
mean(df_polls$ABC_poll)
median(df_polls$CBS_poll)
range(df_polls[, c("ABC_poll","CBS_poll")])

# Add extra (difference) column
df_polls$Diff <- df_polls$CBS_poll - df_polls$ABC_pol

# Graph
ggplot(data=df_polls, aes(x=Name, y=Diff, fill=Name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Difference Between CBS and ABC Polls (CBS - ABC)",
       y = "Difference", x = "Candidate")

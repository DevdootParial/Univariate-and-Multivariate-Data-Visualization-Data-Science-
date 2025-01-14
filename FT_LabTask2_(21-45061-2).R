library(ggplot2)
library(dplyr)
library(moments)
library(GGally)


data <- read.csv("E:/Aiub 12th Semester/Data Science/Final-Term/online_gaming_behavior_dataset.csv", stringsAsFactors = FALSE)
data



ggplot(data, aes(x = AvgSessionDurationMinutes)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Histogram of AvgSessionDurationMinutes", x = "AvgSessionDurationMinutes", y = "Frequency")


ggplot(data, aes(x = AvgSessionDurationMinutes)) +
  geom_density(color = "blue", size = 1.2) +
  labs(title = "Line Histogram of AvgSessionDurationMinutes", x = "AvgSessionDurationMinutes", y = "Density")


skewness <- skewness(data$AvgSessionDurationMinutes, na.rm = TRUE)
print(paste("Skewness Value:", skewness))

if (skewness > 0) {
  skew_type <- "Positive skew"
} else if (skewness < 0) {
  skew_type <- "Negative skew"
} else {
  skew_type <- "Symmetrical distribution"
}
print(paste("The distribution has:", skew_type))


mean_AvgSessionDurationMinutes <- mean(data$AvgSessionDurationMinutes, na.rm = TRUE)
median_AvgSessionDurationMinutes <- median(data$AvgSessionDurationMinutes, na.rm = TRUE)
mode_AvgSessionDurationMinutes <- as.numeric(names(sort(table(data$AvgSessionDurationMinutes), decreasing = TRUE))[1])
skewness_plot <- ggplot(data, aes(x = AvgSessionDurationMinutes)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  geom_vline(xintercept = mean_AvgSessionDurationMinutes, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_AvgSessionDurationMinutes, color = "red", linetype = "dotted", size = 1) +
  geom_vline(xintercept = mode_AvgSessionDurationMinutes, color = "orange", linetype = "dotdash", size = 1) +
  annotate("text", x = mean_AvgSessionDurationMinutes, y = 0.02, label = "Mean", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = median_AvgSessionDurationMinutes, y = 0.02, label = "Median", color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_AvgSessionDurationMinutes, y = 0.02, label = "Mode", color = "orange", angle = 90, vjust = -0.5) +
  labs(title = "Density Plot of AvgSessionDurationMinutes with Mean, Median, and Mode", x = "AvgSessionDurationMinutes", y = "Density")
print(skewness_plot)





correlations <- data %>%
  filter(!is.na(SessionsPerWeek) & !is.na(AvgSessionDurationMinutes)) %>% # Remove NA values
  group_by(EngagementLevel) %>%
  summarise(
    Correlation = cor(SessionsPerWeek, AvgSessionDurationMinutes, method = "pearson")
  )

print(correlations)


scatterplot <- ggplot(data, aes(x = SessionsPerWeek, y = AvgSessionDurationMinutes, color = EngagementLevel)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatterplot of SessionsPerWeek vs AvgSessionDurationMinutes", x = "SessionsPerWeek", y = "AvgSessionDurationMinutes")
print(scatterplot)



engagement_colors <- ifelse(dataset$EngagementLevel == "High", "red",
                            ifelse(dataset$EngagementLevel == "Medium", "blue", "green"))

numeric_dataset <- dataset[c("Age", "SessionsPerWeek", "PlayerLevel")]

pairs(numeric_dataset, 
      main = "Scatter Matrix for Numeric Attributes by EngagementLevel", 
      col = engagement_colors,  
      pch = 19,  
      labels = c("Age", "Sessions Per Week", "Player Level")) 

legend("topright",
       legend = c("High", "Medium", "Low"),
       col = c("red", "blue", "green"),
       pch = 19)


violin_plot <- ggplot(data, aes(x = GameGenre, y = Age, fill = GameGenre)) +
  geom_violin(trim = FALSE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) + 
  labs(
    title = "Violin Plot of Age by GameGenre",
    x = "GameGenre",
    y = "Age"
  ) +
  theme_minimal()

print(violin_plot)



line_graph <- ggplot(data, aes(x = GameGenre, y = Age, group = 1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2, alpha = 0.7) +  
  labs(
    title = "Line Graph: Age vs GameGenre",
    x = "GameGenre",
    y = "Age"
  ) +
  theme_minimal()


print(line_graph)

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggcorrplot)
library(VIM) # missingness visualization

df <- read.csv("subsampledData.csv")
# changing char into numeric
df_numeric <- df %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))

# ================= Missingness Analysis ===================
# calculate missing data
missing_data <- colSums(is.na(df_numeric))
missing_pct <- missing_data / nrow(df_numeric) * 100
missing_summary <- data.frame(Variable = names(missing_data), Missing_Count = missing_data, Missing_Percentage = missing_pct)

print(missing_summary[order(-missing_summary$Missing_Percentage), ])

miss_map <- aggr(df_numeric, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                 labels=names(df_numeric), cex.axis=.7, gap=3, ylab=c("Missing Data Pattern", "Count"))



# ================== basic stats ==========================
summary_stats <- df_numeric %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    iqr = ~ IQR(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

summary_long <- melt(summary_stats)

# visualization
hist_plot <- ggplot(melt(df_numeric), aes(x = value)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Histogram of Numeric Variables", x = "Value", y = "Frequency")

print(hist_plot)

# boxplot
box_plot <- ggplot(melt(df_numeric), aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Numeric Variables", x = "Variables", y = "Values")

print(box_plot)

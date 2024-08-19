
################################################################################
# simulation of the study
library(DT)
library(data.table)

# Parameters
restaurants <- 10
days <- 22
baseline_revenue <- 5625
std_dev <- 500
effect_size <- 562.50  # 10% increase

# scenario 1
# Impact of ordering method on daily revenue
set.seed(0)  # For reproducibility

# Create a data table
ordering_data <- data.table(Day = rep(1:days, times = restaurants),
                            Restaurant_ID = rep(1:restaurants, each = days))

# Generating revenue data for each ordering method
# Adjusted for rounding to one decimal place
ordering_data[, `:=`(Revenue_A_la_carte = round(rnorm(n = .N, mean = baseline_revenue, sd = std_dev), digits = 1),
                     Revenue_Buffet = round(rnorm(n = .N, mean = baseline_revenue + effect_size, sd = std_dev), digits = 1))]


# Viewing the data
datatable(data = ordering_data)

# scenario 2
# impact of different lighting on revenue
set.seed(123)  # For reproducibility

# Create a data table
lighting_data <- data.table(Day = rep(1:days, times = restaurants),
                            Restaurant_ID = rep(1:restaurants, each = days))

# Generating revenue data for each lighting type
# Adjusted for rounding to one decimal place
lighting_data[, `:=`(Revenue_Warm = round(rnorm(n = .N, mean = baseline_revenue + effect_size, sd = std_dev), digits = 1),
                     Revenue_Cold = round(rnorm(n = .N, mean = baseline_revenue + effect_size, sd = std_dev), digits = 1),
                     Revenue_Natural = round(rnorm(n = .N, mean = baseline_revenue, sd = std_dev), digits = 1))]


# Viewing the data
datatable(data = lighting_data)

#scenario 3
#impact of offering takeout
set.seed(999)  # For reproducibility
# Create a data table
takeout_data <- data.table(Day = rep(1:days, times = restaurants),
                           Restaurant_ID = rep(1:restaurants, each = days))

# Generating revenue data for take-out options
# Adjusted for rounding to one decimal place
takeout_data[, `:=`(Revenue_With_Takeout = round(rnorm(n = .N, mean = baseline_revenue + effect_size, sd = std_dev), digits = 1),
                    Revenue_Without_Takeout = round(rnorm(n = .N, mean = baseline_revenue, sd = std_dev), digits = 1))]


# Viewing the data
datatable(data = takeout_data)

################################################################################
# First Part of the Simulation
#####################################################
# Data Exploration
#################################
# Descriptive statistics for ordering method scenario
ordering_stats <- ordering_data[, .(Mean_A_la_carte = mean(Revenue_A_la_carte),
                                    SD_A_la_carte = sd(Revenue_A_la_carte),
                                    Mean_Buffet = mean(Revenue_Buffet),
                                    SD_Buffet = sd(Revenue_Buffet)),
                                    by = Restaurant_ID]


print(ordering_stats)



# Descriptive statistics for lighting scenario
lighting_stats <- lighting_data[, .(Mean_Warm = mean(Revenue_Warm),
                                    SD_Warm = sd(Revenue_Warm),
                                    Mean_Cold = mean(Revenue_Cold),
                                    SD_Cold = sd(Revenue_Cold),
                                    Mean_Natural = mean(Revenue_Natural),
                                    SD_Natural = sd(Revenue_Natural)), by = Restaurant_ID]
print(lighting_stats)

# Reshaping data for ggplot
lighting_long <- melt(lighting_data, id.vars = c("Day", "Restaurant_ID"), 
                      variable.name = "Lighting_Type", value.name = "Revenue")

# Boxplot for lighting scenario
ggplot(lighting_long, aes(x = Lighting_Type, y = Revenue, fill = Lighting_Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Revenue by Lighting Type", x = "Lighting Type", y = "Revenue")
#################################
# Descriptive statistics for take-out scenario
takeout_stats <- takeout_data[, .(Mean_With_Takeout = mean(Revenue_With_Takeout),
                                  SD_With_Takeout = sd(Revenue_With_Takeout),
                                  Mean_Without_Takeout = mean(Revenue_Without_Takeout),
                                  SD_Without_Takeout = sd(Revenue_Without_Takeout)), by = Restaurant_ID]
print(takeout_stats)

# Reshaping data for ggplot
takeout_long <- melt(takeout_data, id.vars = c("Day", "Restaurant_ID"), 
                     variable.name = "Takeout_Option", value.name = "Revenue")

# Boxplot for take-out scenario
library(ggplot2)
ggplot(takeout_long, aes(x = Takeout_Option, y = Revenue, fill = Takeout_Option)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Revenue by Take-Out Option", x = "Take-Out Option", y = "Revenue")
#####################################################
#Statistical Testing
#################################
# scenario 1
ordering_long <- melt(ordering_data, id.vars = c("Day", "Restaurant_ID"),
                      variable.name = "Ordering_Method", value.name = "Revenue")

# ANOVA
anova_ordering <- aov(Revenue ~ Ordering_Method, data = ordering_long)
summary(anova_ordering)

# Post-hoc test if ANOVA is significant
if (summary(anova_ordering)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_ordering <- TukeyHSD(anova_ordering)
  print(posthoc_ordering)
}

#################################
# scenario 2
lighting_long <- melt(lighting_data, id.vars = c("Day", "Restaurant_ID"),
                      variable.name = "Lighting_Type", value.name = "Revenue")
# ANOVA
anova_lighting <- aov(Revenue ~ Lighting_Type, data = lighting_long)
summary(anova_lighting)

# Post-hoc test if ANOVA is significant
if (summary(anova_lighting)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_lighting <- TukeyHSD(anova_lighting)
  print(posthoc_lighting)
}

#################################
takeout_long <- melt(takeout_data, id.vars = c("Day", "Restaurant_ID"),
                     variable.name = "Takeout_Option", value.name = "Revenue")

# ANOVA
anova_takeout <- aov(Revenue ~ Takeout_Option, data = takeout_long)
summary(anova_takeout)

# Post-hoc test if ANOVA is significant
if (summary(anova_takeout)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_takeout <- TukeyHSD(anova_takeout)
  print(posthoc_takeout)
}

#####################################################
################################################################################



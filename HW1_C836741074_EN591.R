# Load packages
library(tidyverse)
library(skimr)


###install.packages("skimr")
# Set seed (use YOUR CWID)
set.seed(836741074)   # Replace with your CWID

# Load data
df <- read.csv("us_tornado_dataset_1950_2021.csv")

#df <- read.csv("C:/Users/Iamco/Documents/us_tornado_dataset_1950_2021.csv")

list.files("C:/Users/Iamco/Documents/", pattern = "tornado")
readLines("C:/Users/Iamco/Documents/us_tornado_dataset_1950_2021.csv", n = 5)

# Basic information
cat (" Dimensions :", dim( df ) , "\n")
str ( df )
head (df , 10)
summary (df)

 # Comprehensive overview
skim (df) #summary of dataset

 # Check for duplicates
cat (" Duplicate rows :", sum( duplicated ( df ) ) , "\n")

##questions in assignments
## Completeness: What percentage of values are missing overall? Which variables have the most missing data? Is missingness concerning for your analysis?
str(df) #completenesss and data types 

## tARGET DISTRIBUTION
table(df$target)
prop.table(table(df$target))
summary(df$target)

## PART 4 EXPLORATORY DATA ANALYSIS
# Target variable distribution

summary(df$mag)
table(df$mag)

library(tidyverse)
names(df)
#
#


readLines("us_tornado_dataset_1950_2021.csv", n = 5)

#
# Remove invalid placeholder magnitudes (e.g., -9)
df_clean <- df %>% filter(mag >= 0)

# Create class labels
df_clean <- df_clean %>%
  mutate(
    mag_class = factor(mag,
                       levels = 0:5,
                       labels = c("EF0", "EF1", "EF2", "EF3", "EF4", "EF5"))
  )

# Compute percentages
class_counts <- df_clean %>%
  count(mag_class) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Bar plot Percentage of Tornadoes vs EF Rating
ggplot(class_counts, aes(x = mag_class, y = percent, fill = mag_class)) +
  geom_col() +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Reds") +
  labs(
    title = "Distribution of Tornado EF Ratings",
    x = "EF Rating",
    y = "Percentage of Tornadoes"
  ) +
  theme_minimal(base_size = 14)


###Plot histogram + density
ggplot(df_clean, aes(x = mag)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "steelblue",
    color = "white",
    alpha = 0.7
  ) +
  geom_density(
    aes(y = 0.5 * after_stat(count)),
    color = "darkred",
    linewidth = 1.2
  ) +
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  labs(
    title = "Distribution of Tornado Magnitude (Regression View)",
    x = "Magnitude (EF Scale)",
    y = "Number of Tornadoes"
  ) +
  theme_minimal(base_size = 14)


## 5.2 Univariate Analysis

# Plot numeric predictors
numeric_vars <- c("len", "wid", "inj", "fat")

df_clean %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(title = "Distribution of Numeric Predictors",
       x = "Value", y = "Count") +
  theme_minimal(base_size = 14)


# Categorical Variables (Bar Plot with Percentages) Month and state distribution
df_clean <- df %>% filter(mag >= 0)

df_clean %>%
  count(mo) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = factor(mo), y = percent)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5) +
  labs(
    title = "Distribution of Tornadoes by Month",
    x = "Month",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14)


#### state Bar Plot with Percentages
df_clean %>%
  count(st) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = st, y = percent)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 3.0) +
  labs(
    title = "Distribution of Tornadoes by State",
    x = "State",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 6        # ← smaller text size for readability
    )
  )

### 5.3 Bivariate Analysis


#Numeric Predictor vs Target (Boxplot)
df_clean <- df %>%
  filter(mag >= 0) %>%
  mutate(
    mag_class = factor(
      mag,
      levels = 0:5,
      labels = c("EF0", "EF1", "EF2", "EF3", "EF4", "EF5")
    )
  )

## Figure 6: Numeric predictor vs. target: Scatterplot (regression) or boxplot/violin (classification)
ggplot(df_clean, aes(x = mag_class, y = wid, fill = mag_class)) +
  geom_boxplot() +
  labs(
    title = "Track Width vs EF Rating",
    x = "EF Rating",
    y = "Track Width (yards)"
  ) +
  theme_minimal(base_size = 14)



## Figure 7: Categorical predictor vs. target: Grouped bar chart, mosaic plot, or stacked bar
df_clean %>%
  count(mo, mag_class) %>%
  group_by(mo) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(mo), y = percent, fill = mag_class)) +
  geom_col() +
  labs(
    title = "EF Rating Distribution by Month",
    x = "Month",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14)

##Figure 8: Two predictors of your choice Interaction plot (two numeric predictors)
ggplot(df_clean, aes(x = len, y = wid, color = mag_class)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Joint Distribution of Track Length and Width",
    x = "Track Length (miles)",
    y = "Track Width (yards)"
  ) +
  theme_minimal(base_size = 14)


##Figure 9: Geographic or temporal pattern (if applicable): Map, time series, or spatial plot
df_clean %>%
  count(yr) %>%
  ggplot(aes(x = yr, y = n)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  labs(
    title = "Tornado Frequency Over Time",
    x = "Year",
    y = "Number of Tornadoes"
  ) +
  theme_minimal(base_size = 14)








##### 5.4 Correlation & Multicollinearity Analysis


##Figure 10: Spearman correlation of all numerical variables
library(GGally)
library(tidyverse)
# Select only the four variables you want
vars <- df_clean %>% 
  select(len, wid, inj, fat)

# Spearman correlation matrix plot
ggpairs(
  vars,
  lower = list(continuous = wrap("cor", method = "spearman")),
  upper = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
  diag  = list(continuous = wrap("densityDiag"))
) +
  theme_bw() +
  ggtitle("Spearman Correlation Matrix: len, wid, inj, fat")



## print spearman table Figure 11: Spearman correlation heatmap  plot
library(tidyverse)
library(reshape2)
library(ggplot2)

# Compute Spearman correlation Table 1: Spearman correlation table for numerical variables used with Minitab (L) and R programming (R)
spearman_table <- cor(vars, method = "spearman", use = "complete.obs")

# Convert to long format
spearman_long <- melt(spearman_table)
spearman_long$value <- round(spearman_long$value, 3)

# Plot the table as a heatmap-style table
ggplot(spearman_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Spearman\nCorrelation") +
  theme_minimal() +
  labs(title = "Spearman Correlation Table (len, wid, inj, fat)",
       x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###heatmap of n^2 values 
## Table 3: Spearman correlation table for numerical variables  
##: For categorical targets: Use point-biserial correlation or ANOVA-based measures
library(effectsize)
library(tidyverse)

# Compute eta-squared for the four variables
eta_results <- map_df(c("len", "wid", "inj", "fat"), function(var) {
  formula <- as.formula(paste(var, "~ mag"))
  aov_model <- aov(formula, data = df_clean)
  eta_sq <- eta_squared(aov_model, partial = FALSE)
  
  tibble(
    variable = var,
    eta_squared = eta_sq$Eta2[1]
  )
})

eta_results


#####
## Part 5: Data Quality & Preprocessing Plan
## 6.1 Missing Data Analysis

#Missing data analysis

#install.packages("naniar")
library (naniar)
# Visualize overall missingness
vis_miss(data)

 # Missing by variable

library(naniar)
library(ggplot2)

# Missingness by variable (works even with 0% missing)
gg_miss_var(df_clean, show_pct = TRUE) +
  labs(
    title = "Missing Data by Variable",
    x = "Variables",
    y = "Number of Missing Values"
  ) +
  theme_minimal(base_size = 14)




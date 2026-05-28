############################################################
# ATMOSPHERIC GREENHOUSE GAS ANALYSIS - CAPE GRIM
# GitHub Repository Version
############################################################

############################
# 1. INSTALL / LOAD PACKAGES
############################

install.packages(c("tidyverse", "lubridate", "readxl", "gridExtra", "reshape2"))

library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(reshape2)

theme_set(theme_bw())

############################
# 2. IMPORT DATA
############################

ghg <- read_excel("data/GHG Dataset from CSIRO (Cleaned) from 1995 to 2025.xlsx")

ghg <- ghg %>%
  select(Date, `CO2(PPM)`, `CH4(PPB)`, `N2O(PPB)`) %>%
  rename(
    CO2 = `CO2(PPM)`,
    CH4 = `CH4(PPB)`,
    N2O = `N2O(PPB)`
  )

ghg$Date <- as.Date(ghg$Date)
ghg$Year <- year(ghg$Date)
ghg$Month <- month(ghg$Date, label = TRUE, abbr = TRUE)

############################
# 3. DATASET OVERVIEW
############################

cat("\n================ DATASET OVERVIEW ================\n")
str(ghg)
summary(ghg)
cat("Number of observations:", nrow(ghg), "\n")

############################
# 4. DESCRIPTIVE STATISTICS
############################

desc_stats <- data.frame(
  Gas = c("CO2", "CH4", "N2O"),
  Mean = c(mean(ghg$CO2, na.rm = TRUE),
           mean(ghg$CH4, na.rm = TRUE),
           mean(ghg$N2O, na.rm = TRUE)),
  Median = c(median(ghg$CO2, na.rm = TRUE),
             median(ghg$CH4, na.rm = TRUE),
             median(ghg$N2O, na.rm = TRUE)),
  SD = c(sd(ghg$CO2, na.rm = TRUE),
         sd(ghg$CH4, na.rm = TRUE),
         sd(ghg$N2O, na.rm = TRUE)),
  Q1 = c(quantile(ghg$CO2, 0.25, na.rm = TRUE),
         quantile(ghg$CH4, 0.25, na.rm = TRUE),
         quantile(ghg$N2O, 0.25, na.rm = TRUE)),
  Q3 = c(quantile(ghg$CO2, 0.75, na.rm = TRUE),
         quantile(ghg$CH4, 0.75, na.rm = TRUE),
         quantile(ghg$N2O, 0.75, na.rm = TRUE)),
  Min = c(min(ghg$CO2, na.rm = TRUE),
          min(ghg$CH4, na.rm = TRUE),
          min(ghg$N2O, na.rm = TRUE)),
  Max = c(max(ghg$CO2, na.rm = TRUE),
          max(ghg$CH4, na.rm = TRUE),
          max(ghg$N2O, na.rm = TRUE))
)

print(desc_stats)

############################
# 5. NORMALITY ANALYSIS
############################

p1 <- ggplot(ghg, aes(CO2)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "CO2 Histogram")

p2 <- ggplot(ghg, aes(CH4)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "CH4 Histogram")

p3 <- ggplot(ghg, aes(N2O)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "N2O Histogram")

grid.arrange(p1, p2, p3, nrow = 1)

ggsave("figures/co2_histogram.png", p1, width = 6, height = 5)
ggsave("figures/ch4_histogram.png", p2, width = 6, height = 5)
ggsave("figures/n2o_histogram.png", p3, width = 6, height = 5)

d1 <- ggplot(ghg, aes(CO2)) + geom_density(colour = "black")
d2 <- ggplot(ghg, aes(CH4)) + geom_density(colour = "black")
d3 <- ggplot(ghg, aes(N2O)) + geom_density(colour = "black")

grid.arrange(d1, d2, d3, nrow = 1)

ggsave("figures/co2_density.png", d1, width = 6, height = 5)
ggsave("figures/ch4_density.png", d2, width = 6, height = 5)
ggsave("figures/n2o_density.png", d3, width = 6, height = 5)

############################
# 6. TIME SERIES ANALYSIS
############################

p_co2 <- ggplot(ghg, aes(Date, CO2)) +
  geom_line(colour = "black") +
  labs(title = "CO2 Time Series")

p_ch4 <- ggplot(ghg, aes(Date, CH4)) +
  geom_line(colour = "black") +
  labs(title = "CH4 Time Series")

p_n2o <- ggplot(ghg, aes(Date, N2O)) +
  geom_line(colour = "black") +
  labs(title = "N2O Time Series")

ggsave("figures/co2_timeseries.png", p_co2, width = 8, height = 5)
ggsave("figures/ch4_timeseries.png", p_ch4, width = 8, height = 5)
ggsave("figures/n2o_timeseries.png", p_n2o, width = 8, height = 5)

############################################################
# 7. CORRELATION ANALYSIS
############################################################

# Scatter Plot: CO2 vs CH4
scatter_co2_ch4 <- ggplot(ghg, aes(CO2, CH4)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "CO2 vs CH4",
    x = "CO2 (PPM)",
    y = "CH4 (PPB)"
  )

print(scatter_co2_ch4)

ggsave(
  "figures/scatter_co2_ch4.png",
  scatter_co2_ch4,
  width = 7,
  height = 5
)

# Scatter Plot: CO2 vs N2O
scatter_co2_n2o <- ggplot(ghg, aes(CO2, N2O)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "CO2 vs N2O",
    x = "CO2 (PPM)",
    y = "N2O (PPB)"
  )

print(scatter_co2_n2o)

ggsave(
  "figures/scatter_co2_n2o.png",
  scatter_co2_n2o,
  width = 7,
  height = 5
)

# Scatter Plot: CH4 vs N2O
scatter_ch4_n2o <- ggplot(ghg, aes(CH4, N2O)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "CH4 vs N2O",
    x = "CH4 (PPB)",
    y = "N2O (PPB)"
  )

print(scatter_ch4_n2o)

ggsave(
  "figures/scatter_ch4_n2o.png",
  scatter_ch4_n2o,
  width = 7,
  height = 5
)

# Spearman Correlation Matrix
cor_matrix <- cor(
  ghg[, c("CO2", "CH4", "N2O")],
  method = "spearman",
  use = "complete.obs"
)

cat("\n================ SPEARMAN CORRELATION MATRIX ================\n")
print(cor_matrix)

write.csv(
  cor_matrix,
  "outputs/spearman_correlation_matrix.csv"
)

############################################################
# 8. LINEAR REGRESSION ANALYSIS
############################################################

# Linear Regression Models
model_co2 <- lm(CO2 ~ Year, data = ghg)
model_ch4 <- lm(CH4 ~ Year, data = ghg)
model_n2o <- lm(N2O ~ Year, data = ghg)

# Model Summaries
cat("\n================ REGRESSION SUMMARIES ================\n")

summary(model_co2)
summary(model_ch4)
summary(model_n2o)

# Regression Summary Table
regression_summary <- data.frame(
  Gas = c("CO2", "CH4", "N2O"),
  Slope = c(
    coef(model_co2)[2],
    coef(model_ch4)[2],
    coef(model_n2o)[2]
  ),
  Intercept = c(
    coef(model_co2)[1],
    coef(model_ch4)[1],
    coef(model_n2o)[1]
  ),
  R2 = c(
    summary(model_co2)$r.squared,
    summary(model_ch4)$r.squared,
    summary(model_n2o)$r.squared
  )
)

cat("\n================ REGRESSION SUMMARY TABLE ================\n")
print(regression_summary)

# CO2 Regression Plot
reg_co2 <- ggplot(ghg, aes(Year, CO2)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "CO2 Linear Regression",
    x = "Year",
    y = "CO2 (PPM)"
  )

print(reg_co2)

ggsave(
  "figures/co2_regression.png",
  reg_co2,
  width = 7,
  height = 5
)

# CH4 Regression Plot
reg_ch4 <- ggplot(ghg, aes(Year, CH4)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "CH4 Linear Regression",
    x = "Year",
    y = "CH4 (PPB)"
  )

print(reg_ch4)

ggsave(
  "figures/ch4_regression.png",
  reg_ch4,
  width = 7,
  height = 5
)

# N2O Regression Plot
reg_n2o <- ggplot(ghg, aes(Year, N2O)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(
    title = "N2O Linear Regression",
    x = "Year",
    y = "N2O (PPB)"
  )

print(reg_n2o)

ggsave(
  "figures/n2o_regression.png",
  reg_n2o,
  width = 7,
  height = 5
)

# Export Regression Summary
write.csv(
  regression_summary,
  "outputs/regression_summary.csv",
  row.names = FALSE
)

############################
# 9. SHORT TERM VARIABILITY
############################

ghg <- ghg %>%
  arrange(Date) %>%
  mutate(
    CO2_change = abs(CO2 - lag(CO2)),
    CH4_change = abs(CH4 - lag(CH4)),
    N2O_change = abs(N2O - lag(N2O))
  )

variability_stats <- data.frame(
  Gas = c("CO2", "CH4", "N2O"),
  Median_Absolute_Change = c(
    median(ghg$CO2_change, na.rm = TRUE),
    median(ghg$CH4_change, na.rm = TRUE),
    median(ghg$N2O_change, na.rm = TRUE)
  ),
  SD_Monthly_Change = c(
    sd(ghg$CO2_change, na.rm = TRUE),
    sd(ghg$CH4_change, na.rm = TRUE),
    sd(ghg$N2O_change, na.rm = TRUE)
  )
)

############################################################
# 10. SEASONAL ANALYSIS
############################################################

# Calculate Monthly Median Concentrations
monthly_medians <- ghg %>%
  group_by(Month) %>%
  summarise(
    CO2 = median(CO2, na.rm = TRUE),
    CH4 = median(CH4, na.rm = TRUE),
    N2O = median(N2O, na.rm = TRUE)
  )

# Reshape Data for Plotting
monthly_long <- melt(monthly_medians, id.vars = "Month")

############################################################
# CO2 SEASONALITY GRAPH
############################################################

season_co2 <- ggplot(
  monthly_medians,
  aes(Month, CO2, group = 1)
) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  labs(
    title = "Seasonal Trend of CO2",
    x = "Month",
    y = "Median CO2 Concentration (PPM)"
  ) +
  theme_bw()

print(season_co2)

ggsave(
  "figures/co2_seasonality.png",
  season_co2,
  width = 7,
  height = 5
)

############################################################
# CH4 SEASONALITY GRAPH
############################################################

season_ch4 <- ggplot(
  monthly_medians,
  aes(Month, CH4, group = 1)
) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  labs(
    title = "Seasonal Trend of CH4",
    x = "Month",
    y = "Median CH4 Concentration (PPB)"
  ) +
  theme_bw()

print(season_ch4)

ggsave(
  "figures/ch4_seasonality.png",
  season_ch4,
  width = 7,
  height = 5
)

############################################################
# N2O SEASONALITY GRAPH
############################################################

season_n2o <- ggplot(
  monthly_medians,
  aes(Month, N2O, group = 1)
) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  labs(
    title = "Seasonal Trend of N2O",
    x = "Month",
    y = "Median N2O Concentration (PPB)"
  ) +
  theme_bw()

print(season_n2o)

ggsave(
  "figures/n2o_seasonality.png",
  season_n2o,
  width = 7,
  height = 5
)

############################################################
# COMBINED SEASONALITY GRAPH
############################################################

season_combined <- ggplot(
  monthly_long,
  aes(Month, value, group = variable)
) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Monthly Median Seasonal Trends",
    x = "Month",
    y = "Median Concentration"
  ) +
  theme_bw()

print(season_combined)

ggsave(
  "figures/combined_seasonality.png",
  season_combined,
  width = 10,
  height = 6
)

############################################################
# SEASONAL AMPLITUDE CALCULATION
############################################################

seasonal_amplitude <- monthly_long %>%
  group_by(variable) %>%
  summarise(
    Seasonal_Amplitude = max(value) - min(value)
  )

cat("\n================ SEASONAL AMPLITUDE ================\n")
print(seasonal_amplitude)

write.csv(
  seasonal_amplitude,
  "outputs/seasonal_amplitude.csv",
  row.names = FALSE
)

############################
# 11. EXPORT RESULTS
############################

write.csv(desc_stats, "outputs/descriptive_statistics.csv", row.names = FALSE)
write.csv(regression_summary, "outputs/regression_summary.csv", row.names = FALSE)
write.csv(variability_stats, "outputs/variability_statistics.csv", row.names = FALSE)
write.csv(seasonal_amplitude, "outputs/seasonal_amplitude.csv", row.names = FALSE)

cat("Analysis complete. Outputs saved in /outputs and figures saved in /figures.\n")


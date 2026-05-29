############################################################
# ATMOSPHERIC GREENHOUSE GAS ANALYSIS - CAPE GRIM
# FINAL CODE (Select the repository folder as working directory)
############################################################

############################
# 1. INSTALL / LOAD PACKAGES
############################

install.packages(c(
  "tidyverse",
  "lubridate",
  "readxl",
  "gridExtra",
  "reshape2"
))

library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(reshape2)

############################################################
# GLOBAL THEME
############################################################

theme_set(
  theme_bw() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

############################################################
# COLOUR PALETTE
############################################################

co2_col <- "#0072B2"
ch4_col <- "#D55E00"
n2o_col <- "#009E73"

############################################################
# 2. IMPORT DATA
############################################################

ghg <- read_excel(
  "data/GHG Dataset from CSIRO (Cleaned) from 1995 to 2025.xlsx"
)

ghg <- ghg %>%
  select(
    Date,
    `CO2(PPM)`,
    `CH4(PPB)`,
    `N2O(PPB)`
  ) %>%
  rename(
    CO2 = `CO2(PPM)`,
    CH4 = `CH4(PPB)`,
    N2O = `N2O(PPB)`
  )

ghg$Date <- as.Date(ghg$Date)

ghg$Year <- year(ghg$Date)

ghg$Month <- month(
  ghg$Date,
  label = TRUE,
  abbr = TRUE
)

############################################################
# 3. DATASET OVERVIEW
############################################################

cat("\n================ DATASET OVERVIEW ================\n")

str(ghg)

summary(ghg)

cat(
  "\nNumber of observations:",
  nrow(ghg),
  "\n"
)

############################################################
# 4. DESCRIPTIVE STATISTICS
############################################################

desc_stats <- data.frame(
  Gas = c("CO2", "CH4", "N2O"),
  
  Mean = c(
    mean(ghg$CO2, na.rm = TRUE),
    mean(ghg$CH4, na.rm = TRUE),
    mean(ghg$N2O, na.rm = TRUE)
  ),
  
  Median = c(
    median(ghg$CO2, na.rm = TRUE),
    median(ghg$CH4, na.rm = TRUE),
    median(ghg$N2O, na.rm = TRUE)
  ),
  
  SD = c(
    sd(ghg$CO2, na.rm = TRUE),
    sd(ghg$CH4, na.rm = TRUE),
    sd(ghg$N2O, na.rm = TRUE)
  ),
  
  Q1 = c(
    quantile(ghg$CO2, 0.25, na.rm = TRUE),
    quantile(ghg$CH4, 0.25, na.rm = TRUE),
    quantile(ghg$N2O, 0.25, na.rm = TRUE)
  ),
  
  Q3 = c(
    quantile(ghg$CO2, 0.75, na.rm = TRUE),
    quantile(ghg$CH4, 0.75, na.rm = TRUE),
    quantile(ghg$N2O, 0.75, na.rm = TRUE)
  ),
  
  Min = c(
    min(ghg$CO2, na.rm = TRUE),
    min(ghg$CH4, na.rm = TRUE),
    min(ghg$N2O, na.rm = TRUE)
  ),
  
  Max = c(
    max(ghg$CO2, na.rm = TRUE),
    max(ghg$CH4, na.rm = TRUE),
    max(ghg$N2O, na.rm = TRUE)
  )
)

print(desc_stats)

write.csv(
  desc_stats,
  "outputs/descriptive_statistics.csv",
  row.names = FALSE
)

############################################################
# 5. NORMALITY ANALYSIS
############################################################

############################
# HISTOGRAMS
############################

hist_co2 <- ggplot(ghg, aes(CO2)) +
  geom_histogram(
    bins = 30,
    fill = co2_col,
    colour = "black",
    alpha = 0.8
  ) +
  labs(
    title = "CO2 Histogram",
    x = "CO2 (PPM)",
    y = "Frequency"
  )

hist_ch4 <- ggplot(ghg, aes(CH4)) +
  geom_histogram(
    bins = 30,
    fill = ch4_col,
    colour = "black",
    alpha = 0.8
  ) +
  labs(
    title = "CH4 Histogram",
    x = "CH4 (PPB)",
    y = "Frequency"
  )

hist_n2o <- ggplot(ghg, aes(N2O)) +
  geom_histogram(
    bins = 30,
    fill = n2o_col,
    colour = "black",
    alpha = 0.8
  ) +
  labs(
    title = "N2O Histogram",
    x = "N2O (PPB)",
    y = "Frequency"
  )

grid.arrange(
  hist_co2,
  hist_ch4,
  hist_n2o,
  nrow = 1
)

ggsave("figures/co2_histogram.png", hist_co2, width = 6, height = 5)
ggsave("figures/ch4_histogram.png", hist_ch4, width = 6, height = 5)
ggsave("figures/n2o_histogram.png", hist_n2o, width = 6, height = 5)

############################
# DENSITY PLOTS
############################

density_co2 <- ggplot(ghg, aes(CO2)) +
  geom_density(
    colour = co2_col,
    linewidth = 1.2
  ) +
  labs(
    title = "CO2 Density Plot",
    x = "CO2 (PPM)",
    y = "Density"
  )

density_ch4 <- ggplot(ghg, aes(CH4)) +
  geom_density(
    colour = ch4_col,
    linewidth = 1.2
  ) +
  labs(
    title = "CH4 Density Plot",
    x = "CH4 (PPB)",
    y = "Density"
  )

density_n2o <- ggplot(ghg, aes(N2O)) +
  geom_density(
    colour = n2o_col,
    linewidth = 1.2
  ) +
  labs(
    title = "N2O Density Plot",
    x = "N2O (PPB)",
    y = "Density"
  )

grid.arrange(
  density_co2,
  density_ch4,
  density_n2o,
  nrow = 1
)

ggsave("figures/co2_density.png", density_co2, width = 6, height = 5)
ggsave("figures/ch4_density.png", density_ch4, width = 6, height = 5)
ggsave("figures/n2o_density.png", density_n2o, width = 6, height = 5)

############################
# QQ PLOTS
############################

qq_co2 <- ggplot(
  ghg,
  aes(sample = CO2)
) +
  stat_qq(colour = co2_col) +
  stat_qq_line(
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "CO2 QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

print(qq_co2)

ggsave(
  "figures/co2_qqplot.png",
  qq_co2,
  width = 6,
  height = 5
)

qq_ch4 <- ggplot(
  ghg,
  aes(sample = CH4)
) +
  stat_qq(colour = ch4_col) +
  stat_qq_line(
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "CH4 QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

print(qq_ch4)

ggsave(
  "figures/ch4_qqplot.png",
  qq_ch4,
  width = 6,
  height = 5
)

qq_n2o <- ggplot(
  ghg,
  aes(sample = N2O)
) +
  stat_qq(colour = n2o_col) +
  stat_qq_line(
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "N2O QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

print(qq_n2o)

ggsave(
  "figures/n2o_qqplot.png",
  qq_n2o,
  width = 6,
  height = 5
)

############################################################
# 6. TIME SERIES ANALYSIS
############################################################

time_co2 <- ggplot(
  ghg,
  aes(Date, CO2)
) +
  geom_line(
    colour = co2_col,
    linewidth = 0.8
  ) +
  labs(
    title = "CO2 Time Series",
    x = "Date",
    y = "CO2 (PPM)"
  )

time_ch4 <- ggplot(
  ghg,
  aes(Date, CH4)
) +
  geom_line(
    colour = ch4_col,
    linewidth = 0.8
  ) +
  labs(
    title = "CH4 Time Series",
    x = "Date",
    y = "CH4 (PPB)"
  )

time_n2o <- ggplot(
  ghg,
  aes(Date, N2O)
) +
  geom_line(
    colour = n2o_col,
    linewidth = 0.8
  ) +
  labs(
    title = "N2O Time Series",
    x = "Date",
    y = "N2O (PPB)"
  )

print(time_co2)
print(time_ch4)
print(time_n2o)

ggsave(
  "figures/co2_timeseries.png",
  time_co2,
  width = 8,
  height = 5
)

ggsave(
  "figures/ch4_timeseries.png",
  time_ch4,
  width = 8,
  height = 5
)

ggsave(
  "figures/n2o_timeseries.png",
  time_n2o,
  width = 8,
  height = 5
)

############################################################
# 7. CORRELATION ANALYSIS
############################################################

############################
# SCATTER PLOTS
############################

scatter_co2_ch4 <- ggplot(
  ghg,
  aes(CO2, CH4)
) +
  geom_point(
    colour = co2_col,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = ch4_col
  ) +
  labs(
    title = "CO2 vs CH4",
    x = "CO2 (PPM)",
    y = "CH4 (PPB)"
  )

scatter_co2_n2o <- ggplot(
  ghg,
  aes(CO2, N2O)
) +
  geom_point(
    colour = co2_col,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = n2o_col
  ) +
  labs(
    title = "CO2 vs N2O",
    x = "CO2 (PPM)",
    y = "N2O (PPB)"
  )

scatter_ch4_n2o <- ggplot(
  ghg,
  aes(CH4, N2O)
) +
  geom_point(
    colour = ch4_col,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = n2o_col
  ) +
  labs(
    title = "CH4 vs N2O",
    x = "CH4 (PPB)",
    y = "N2O (PPB)"
  )

print(scatter_co2_ch4)
print(scatter_co2_n2o)
print(scatter_ch4_n2o)

ggsave(
  "figures/scatter_co2_ch4.png",
  scatter_co2_ch4,
  width = 7,
  height = 5
)

ggsave(
  "figures/scatter_co2_n2o.png",
  scatter_co2_n2o,
  width = 7,
  height = 5
)

ggsave(
  "figures/scatter_ch4_n2o.png",
  scatter_ch4_n2o,
  width = 7,
  height = 5
)

############################
# SPEARMAN CORRELATION
############################

cor_matrix <- cor(
  ghg[, c("CO2", "CH4", "N2O")],
  method = "spearman",
  use = "complete.obs"
)

cat(
  "\n================ SPEARMAN CORRELATION MATRIX ================\n"
)

print(cor_matrix)

write.csv(
  cor_matrix,
  "outputs/spearman_correlation_matrix.csv"
)

############################################################
# 8. LINEAR REGRESSION ANALYSIS
############################################################

############################
# REGRESSION MODELS
############################

model_co2 <- lm(
  CO2 ~ Year,
  data = ghg
)

model_ch4 <- lm(
  CH4 ~ Year,
  data = ghg
)

model_n2o <- lm(
  N2O ~ Year,
  data = ghg
)

############################
# MODEL SUMMARIES
############################

summary(model_co2)
summary(model_ch4)
summary(model_n2o)

############################
# REGRESSION SUMMARY TABLE
############################

regression_summary <- data.frame(
  Gas = c(
    "CO2",
    "CH4",
    "N2O"
  ),
  
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

print(regression_summary)

write.csv(
  regression_summary,
  "outputs/regression_summary.csv",
  row.names = FALSE
)

############################
# REGRESSION PLOTS
############################

reg_co2 <- ggplot(
  ghg,
  aes(Year, CO2)
) +
  geom_point(
    colour = co2_col,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "CO2 Linear Regression",
    x = "Year",
    y = "CO2 (PPM)"
  )

reg_ch4 <- ggplot(
  ghg,
  aes(Year, CH4)
) +
  geom_point(
    colour = ch4_col,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "CH4 Linear Regression",
    x = "Year",
    y = "CH4 (PPB)"
  )

reg_n2o <- ggplot(
  ghg,
  aes(Year, N2O)
) +
  geom_point(
    colour = n2o_col,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 1
  ) +
  labs(
    title = "N2O Linear Regression",
    x = "Year",
    y = "N2O (PPB)"
  )

print(reg_co2)
print(reg_ch4)
print(reg_n2o)

ggsave(
  "figures/co2_regression.png",
  reg_co2,
  width = 7,
  height = 5
)

ggsave(
  "figures/ch4_regression.png",
  reg_ch4,
  width = 7,
  height = 5
)

ggsave(
  "figures/n2o_regression.png",
  reg_n2o,
  width = 7,
  height = 5
)

############################################################
# 9. SHORT-TERM VARIABILITY ANALYSIS
############################################################

ghg <- ghg %>%
  arrange(Date) %>%
  mutate(
    CO2_change = abs(CO2 - lag(CO2)),
    CH4_change = abs(CH4 - lag(CH4)),
    N2O_change = abs(N2O - lag(N2O))
  )

############################
# VARIABILITY STATISTICS
############################

variability_stats <- data.frame(
  Gas = c(
    "CO2",
    "CH4",
    "N2O"
  ),
  
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

print(variability_stats)

write.csv(
  variability_stats,
  "outputs/variability_statistics.csv",
  row.names = FALSE
)

############################################################
# 10. WILCOXON RANK-SUM TESTS
############################################################

wilcox_ch4_co2 <- wilcox.test(
  ghg$CH4_change,
  ghg$CO2_change,
  alternative = "two.sided"
)

wilcox_ch4_n2o <- wilcox.test(
  ghg$CH4_change,
  ghg$N2O_change,
  alternative = "two.sided"
)

cat(
  "\n================ WILCOXON TEST RESULTS ================\n"
)

print(wilcox_ch4_co2)
print(wilcox_ch4_n2o)

############################################################
# 11. SEASONAL ANALYSIS
############################################################

############################
# MONTHLY MEDIANS
############################

monthly_medians <- ghg %>%
  group_by(Month) %>%
  summarise(
    CO2 = median(CO2, na.rm = TRUE),
    CH4 = median(CH4, na.rm = TRUE),
    N2O = median(N2O, na.rm = TRUE)
  )

monthly_long <- melt(
  monthly_medians,
  id.vars = "Month"
)

############################
# CO2 SEASONALITY
############################

season_co2 <- ggplot(
  monthly_medians,
  aes(
    Month,
    CO2,
    group = 1
  )
) +
  geom_line(
    colour = co2_col,
    linewidth = 1
  ) +
  geom_point(
    colour = co2_col,
    size = 2.5
  ) +
  labs(
    title = "Seasonal Trend of CO2",
    x = "Month",
    y = "Median CO2 Concentration (PPM)"
  )

print(season_co2)

ggsave(
  "figures/co2_seasonality.png",
  season_co2,
  width = 7,
  height = 5
)

############################
# CH4 SEASONALITY
############################

season_ch4 <- ggplot(
  monthly_medians,
  aes(
    Month,
    CH4,
    group = 1
  )
) +
  geom_line(
    colour = ch4_col,
    linewidth = 1
  ) +
  geom_point(
    colour = ch4_col,
    size = 2.5
  ) +
  labs(
    title = "Seasonal Trend of CH4",
    x = "Month",
    y = "Median CH4 Concentration (PPB)"
  )

print(season_ch4)

ggsave(
  "figures/ch4_seasonality.png",
  season_ch4,
  width = 7,
  height = 5
)

############################
# N2O SEASONALITY
############################

season_n2o <- ggplot(
  monthly_medians,
  aes(
    Month,
    N2O,
    group = 1
  )
) +
  geom_line(
    colour = n2o_col,
    linewidth = 1
  ) +
  geom_point(
    colour = n2o_col,
    size = 2.5
  ) +
  labs(
    title = "Seasonal Trend of N2O",
    x = "Month",
    y = "Median N2O Concentration (PPB)"
  )

print(season_n2o)

ggsave(
  "figures/n2o_seasonality.png",
  season_n2o,
  width = 7,
  height = 5
)

############################
# COMBINED SEASONALITY PLOT
############################

season_combined <- ggplot(
  monthly_long,
  aes(
    Month,
    value,
    colour = variable,
    group = variable
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_colour_manual(
    values = c(
      "CO2" = co2_col,
      "CH4" = ch4_col,
      "N2O" = n2o_col
    )
  ) +
  facet_wrap(
    ~variable,
    scales = "free_y"
  ) +
  labs(
    title = "Monthly Median Seasonal Trends",
    x = "Month",
    y = "Median Concentration",
    colour = "Gas"
  )

print(season_combined)

ggsave(
  "figures/combined_seasonality.png",
  season_combined,
  width = 10,
  height = 6
)

############################################################
# 12. SEASONAL AMPLITUDE
############################################################

seasonal_amplitude <- monthly_long %>%
  group_by(variable) %>%
  summarise(
    Seasonal_Amplitude = max(value) - min(value)
  )

cat(
  "\n================ SEASONAL AMPLITUDE ================\n"
)

print(seasonal_amplitude)

write.csv(
  seasonal_amplitude,
  "outputs/seasonal_amplitude.csv",
  row.names = FALSE
)

############################################################
# FINAL COMPLETION MESSAGE
############################################################

cat(
  "\n====================================================\n",
  "Analysis complete.\n",
  "Outputs saved in /outputs\n",
  "Figures saved in /figures\n",
  "====================================================\n"
)


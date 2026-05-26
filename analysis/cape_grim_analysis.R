############################################################
# ATMOSPHERIC GREENHOUSE GAS ANALYSIS - CAPE GRIM
# Single Complete R Script
# Author: Nirzar Shah, Ullas HS, Mihir Vyas, Sasikiran Challa, Yash Sharma, Bharani Yemula
# Group: Leviathan Group
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

ghg <- read_excel("GHG Dataset from CSIRO (Cleaned) from 1995 to 2025.xlsx")

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

cat("\n================ DESCRIPTIVE STATISTICS ================\n")
print(desc_stats)

############################
# 5. NORMALITY ANALYSIS
############################

p1 <- ggplot(ghg, aes(CO2)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "CO2 Histogram", x = "CO2 (PPM)", y = "Frequency")

p2 <- ggplot(ghg, aes(CH4)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "CH4 Histogram", x = "CH4 (PPB)", y = "Frequency")

p3 <- ggplot(ghg, aes(N2O)) +
  geom_histogram(bins = 30, fill = "white", colour = "black") +
  labs(title = "N2O Histogram", x = "N2O (PPB)", y = "Frequency")

grid.arrange(p1, p2, p3, nrow = 1)

d1 <- ggplot(ghg, aes(CO2)) +
  geom_density(colour = "black") +
  labs(title = "CO2 Density")

d2 <- ggplot(ghg, aes(CH4)) +
  geom_density(colour = "black") +
  labs(title = "CH4 Density")

d3 <- ggplot(ghg, aes(N2O)) +
  geom_density(colour = "black") +
  labs(title = "N2O Density")

grid.arrange(d1, d2, d3, nrow = 1)

par(mfrow = c(1, 3))

qqnorm(ghg$CO2, main = "CO2 QQ Plot")
qqline(ghg$CO2)

qqnorm(ghg$CH4, main = "CH4 QQ Plot")
qqline(ghg$CH4)

qqnorm(ghg$N2O, main = "N2O QQ Plot")
qqline(ghg$N2O)

par(mfrow = c(1, 1))

############################
# 6. TIME SERIES ANALYSIS
############################

p_co2 <- ggplot(ghg, aes(Date, CO2)) +
  geom_line(colour = "black") +
  labs(title = "CO2 Time Series", y = "CO2 (PPM)", x = "Date")

p_ch4 <- ggplot(ghg, aes(Date, CH4)) +
  geom_line(colour = "black") +
  labs(title = "CH4 Time Series", y = "CH4 (PPB)", x = "Date")

p_n2o <- ggplot(ghg, aes(Date, N2O)) +
  geom_line(colour = "black") +
  labs(title = "N2O Time Series", y = "N2O (PPB)", x = "Date")

grid.arrange(p_co2, p_ch4, p_n2o, nrow = 3)

############################
# 7. CORRELATION ANALYSIS
############################

ggplot(ghg, aes(CO2, CH4)) +
  geom_point(colour = "black") +
  labs(title = "CO2 vs CH4")

ggplot(ghg, aes(CO2, N2O)) +
  geom_point(colour = "black") +
  labs(title = "CO2 vs N2O")

ggplot(ghg, aes(CH4, N2O)) +
  geom_point(colour = "black") +
  labs(title = "CH4 vs N2O")

cor_matrix <- cor(
  ghg[, c("CO2", "CH4", "N2O")],
  method = "spearman",
  use = "complete.obs"
)

cat("\n================ SPEARMAN CORRELATION MATRIX ================\n")
print(cor_matrix)

############################
# 8. LINEAR REGRESSION ANALYSIS
############################

model_co2 <- lm(CO2 ~ Year, data = ghg)
model_ch4 <- lm(CH4 ~ Year, data = ghg)
model_n2o <- lm(N2O ~ Year, data = ghg)

cat("\n================ REGRESSION SUMMARIES ================\n")
summary(model_co2)
summary(model_ch4)
summary(model_n2o)

regression_summary <- data.frame(
  Gas = c("CO2", "CH4", "N2O"),
  Slope = c(coef(model_co2)[2],
            coef(model_ch4)[2],
            coef(model_n2o)[2]),
  Intercept = c(coef(model_co2)[1],
                coef(model_ch4)[1],
                coef(model_n2o)[1]),
  R2 = c(summary(model_co2)$r.squared,
         summary(model_ch4)$r.squared,
         summary(model_n2o)$r.squared)
)

cat("\n================ REGRESSION SUMMARY TABLE ================\n")
print(regression_summary)

ggplot(ghg, aes(Year, CO2)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "CO2 Linear Regression")

ggplot(ghg, aes(Year, CH4)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "CH4 Linear Regression")

ggplot(ghg, aes(Year, N2O)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "N2O Linear Regression")

############################
# 9. SHORT-TERM VARIABILITY
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

cat("\n================ SHORT-TERM VARIABILITY ================\n")
print(variability_stats)

############################
# 10. WILCOXON TESTS
############################

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

cat("\n================ WILCOXON TEST RESULTS ================\n")
print(wilcox_ch4_co2)
print(wilcox_ch4_n2o)

############################
# 11. SEASONAL ANALYSIS
############################

monthly_medians <- ghg %>%
  group_by(Month) %>%
  summarise(
    CO2 = median(CO2, na.rm = TRUE),
    CH4 = median(CH4, na.rm = TRUE),
    N2O = median(N2O, na.rm = TRUE)
  )

monthly_long <- melt(monthly_medians, id.vars = "Month")

ggplot(monthly_long, aes(Month, value, group = variable)) +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Monthly Median Seasonal Trends",
    x = "Month",
    y = "Median Concentration"
  )

############################
# 12. SEASONAL AMPLITUDE
############################

seasonal_amplitude <- monthly_long %>%
  group_by(variable) %>%
  summarise(
    Seasonal_Amplitude = max(value) - min(value)
  )

cat("\n================ SEASONAL AMPLITUDE ================\n")
print(seasonal_amplitude)

############################
# 13. EXPORT RESULTS
############################

write.csv(desc_stats, "descriptive_statistics.csv", row.names = FALSE)
write.csv(regression_summary, "regression_summary.csv", row.names = FALSE)
write.csv(variability_stats, "variability_statistics.csv", row.names = FALSE)
write.csv(seasonal_amplitude, "seasonal_amplitude.csv", row.names = FALSE)

cat("\nAnalysis complete. CSV outputs exported successfully.\n")
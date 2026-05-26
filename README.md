# cape-grim-greenhouse-gas-analysis
Exploratory statistical analysis of atmospheric greenhouse gas concentrations (CO2, CH4, and N2O) from Cape Grim Baseline Air Pollution Station.
Investigation of Atmospheric Greenhouse Gas Emissions at Kennaook-Cape Grim Station

\## Author / Project Team



Leviathan Group



\- Nirzar Shah

\- Mihir Ketan Vyas

\- Yash Sharma

\- Ullas HS

\- SasiKiran Challa

\- Bharani Vemula



\---



\# Atmospheric Greenhouse Gas Analysis at Cape Grim Baseline Air Pollution Station



\## Project Overview



This repository presents an exploratory statistical analysis of atmospheric greenhouse gas concentrations measured at the \*\*Cape Grim Baseline Air Pollution Station (Kennaook/Cape Grim), Tasmania, Australia\*\*.



The study investigates long-term temporal trends, short-term variability, correlation patterns, and seasonal behaviour of three major greenhouse gases:



\- \*\*Carbon Dioxide (CO₂)\*\* — measured in Parts Per Million (PPM)

\- \*\*Methane (CH₄)\*\* — measured in Parts Per Billion (PPB)

\- \*\*Nitrous Oxide (N₂O)\*\* — measured in Parts Per Billion (PPB)



using monthly mean observations collected between \*\*1995 and 2025\*\*.



This analysis was developed as part of an exploratory data analysis/statistical investigation focusing on greenhouse gas dynamics in the Southern Hemisphere.



\---



\## Project Objectives



The key objectives of this project were to:



\- Analyse long-term atmospheric growth trends in CO₂, CH₄, and N₂O

\- Assess statistical distribution characteristics of each gas

\- Examine inter-gas relationships using correlation analysis

\- Compare growth rates using linear regression

\- Quantify short-term month-to-month variability

\- Investigate whether methane exhibits significantly greater short-term fluctuation

\- Explore seasonal behaviour and cyclical concentration changes

\- Build a reproducible statistical workflow using R



\---



\## Dataset Information



\### Data Source



The dataset used in this repository is based on greenhouse gas observations from:



\*\*CSIRO – Cape Grim Baseline Air Pollution Station\*\*



Official source:

https://www.csiro.au/greenhouse-gases/



Cape Grim is one of the world’s most significant atmospheric monitoring stations and provides long-term measurements of baseline greenhouse gas concentrations in the Southern Hemisphere.



\---



\### Dataset Scope



| Attribute | Details |

|---------|---------|

| Location | Cape Grim Baseline Air Pollution Station, Tasmania, Australia |

| Time Period | 1995–2025 |

| Frequency | Monthly |

| Number of Observations | 372 |

| Data Type | Continuous numerical time-series |

| Format | Excel (.xlsx) |



\---



\### Variables Used



The analysis uses the following dataset columns:



| Repository Variable | Original Excel Column |

|--------------------|----------------------|

| Date | `Date` |

| CO2 | `CO2(PPM)` |

| CH4 | `CH4(PPB)` |

| N2O | `N2O(PPB)` |



\---



\## Repository Structure



```text

cape-grim-greenhouse-gas-analysis/

│

├── data/

│   └── GHG Dataset from CSIRO (Cleaned) from 1995 to 2025.xlsx

│

├── analysis/

│   └── cape\\\_grim\\\_analysis.R

│

├── outputs/

│   ├── descriptive\\\_statistics.csv

│   ├── regression\\\_summary.csv

│   ├── variability\\\_statistics.csv

│   ├── seasonal\\\_amplitude.csv

│   └── spearman\\\_correlation\\\_matrix.csv

│

├── figures/

│   ├── co2\\\_histogram.png

│   ├── ch4\\\_histogram.png

│   ├── n2o\\\_histogram.png

│   ├── co2\\\_density.png

│   ├── ch4\\\_density.png

│   ├── n2o\\\_density.png

│   ├── co2\\\_timeseries.png

│   ├── ch4\\\_timeseries.png

│   ├── n2o\\\_timeseries.png

│   └── seasonal\\\_trends.png

│

├── README.md

├── .gitignore

└── LICENSE

```



\---



\## File Descriptions



\### `data/`

Contains the cleaned raw dataset used for analysis.



\*\*File:\*\*

\- `GHG Dataset from CSIRO (Cleaned) from 1995 to 2025.xlsx`



\---



\### `analysis/`

Contains the full reproducible R analysis script.



\*\*File:\*\*

\- `cape\\\_grim\\\_analysis.R`



This script performs:

\- data import

\- preprocessing

\- statistical analysis

\- visualisation

\- result export



\---



\### `outputs/`

Contains machine-readable exported statistical outputs.



Files include:



\#### `descriptive\\\_statistics.csv`

Summary statistics:

\- mean

\- median

\- standard deviation

\- quartiles

\- min/max values



\---



\#### `regression\\\_summary.csv`

Linear regression model outputs:

\- slope coefficients

\- intercepts

\- R² values



\---



\#### `variability\\\_statistics.csv`

Short-term variability metrics:

\- median absolute monthly change

\- standard deviation of monthly change



\---



\#### `seasonal\\\_amplitude.csv`

Estimated seasonal amplitude:

\- max monthly median − min monthly median



\---



\#### `spearman\\\_correlation\\\_matrix.csv`

Spearman rank correlation coefficients between:

\- CO₂

\- CH₄

\- N₂O



\---



\### `figures/`

Contains exported visualisations generated by the R script.



Includes:

\- Histograms

\- Density plots

\- Time series plots

\- Seasonal trend plots



\---



\## Statistical Methods Used



The following methods were applied:



\### 1. Descriptive Statistics

To summarise:

\- central tendency

\- dispersion

\- distribution spread



Metrics:

\- Mean

\- Median

\- Standard deviation

\- Quartiles

\- Minimum / Maximum



\---



\### 2. Normality Assessment

Distribution shape was explored using:

\- Histograms

\- Kernel density plots

\- Quantile-Quantile (QQ) plots



This informed the use of non-parametric methods.



\---



\### 3. Correlation Analysis

Relationships between greenhouse gases were evaluated using:



\*\*Spearman Rank Correlation\*\*



Chosen because:

\- data distributions were not assumed normal

\- monotonic relationships were of interest



\---



\### 4. Linear Regression

Simple linear regression models were fitted:



```math

Gas Concentration \\\~ Year

```



Outputs:

\- slope (annual growth rate)

\- intercept

\- R² goodness of fit



\---



\### 5. Short-Term Variability Analysis

Month-to-month changes were quantified using:



```text

Absolute Monthly Change = |Current Month - Previous Month|

```



Metrics:

\- median absolute monthly change

\- standard deviation of monthly change



\---



\### 6. Wilcoxon Rank-Sum Testing

Used to compare methane variability against:



\- CO₂

\- N₂O



This non-parametric approach was chosen due to non-normal distributions.



\---



\### 7. Seasonal Trend Analysis

Median monthly concentrations were calculated across all years to identify seasonal behaviour.



\---



\### 8. Seasonal Amplitude Estimation

Calculated as:



```text

Maximum Monthly Median − Minimum Monthly Median

```



to estimate cyclical concentration range.



\---



\## Software Requirements



To run this project locally:



\### Required Software



\- \*\*R (version 4.0+)\*\*

\- \*\*RStudio (recommended)\*\*

\- \*\*Git\*\*

\- \*\*GitHub Desktop (optional but beginner-friendly)\*\*



\---



\## Required R Libraries



Install with:



```r

install.packages(c(

\&#x20; "tidyverse",

\&#x20; "lubridate",

\&#x20; "readxl",

\&#x20; "gridExtra",

\&#x20; "reshape2"

))

```



Libraries used:



| Package | Purpose |

|--------|---------|

| tidyverse | data manipulation + plotting |

| lubridate | date handling |

| readxl | Excel import |

| gridExtra | plot arrangement |

| reshape2 | data reshaping |



\---



\## How to Run the Analysis



\### Step 1 — Clone repository



```bash

git clone https://github.com/YOUR-USERNAME/cape-grim-greenhouse-gas-analysis.git

```



\---



\### Step 2 — Open project folder



Set your working directory to:



```text

cape-grim-greenhouse-gas-analysis

```



This is important because the script uses relative paths.



\---



\### Step 3 — Run script



Open:



```text

analysis/cape\\\_grim\\\_analysis.R

```



Then run the full script.



\---



\### Step 4 — Generated outputs



After execution:



\- CSV outputs appear in `outputs/`

\- figures appear in `figures/`



\---



\## Limitations



This project is exploratory in nature.



Key limitations include:



\- Monthly observations are time-series data and may exhibit autocorrelation

\- Wilcoxon rank-sum tests assume independence between observations

\- Linear regression assumes approximate linear trend behaviour

\- Seasonal analysis is descriptive rather than predictive



Results should therefore be interpreted as exploratory statistical insights rather than definitive inferential conclusions.



\---



\## Future Extensions



Potential improvements:



\- ARIMA forecasting

\- STL decomposition

\- autocorrelation analysis

\- lagged cross-correlation analysis

\- non-linear trend modelling

\- inclusion of environmental covariates:

&#x20; - temperature

&#x20; - rainfall

&#x20; - wind direction

&#x20; - atmospheric pressure



\---



\## License



This repository is released under the MIT License.



\---



\## Acknowledgements



Data source:



CSIRO Cape Grim Baseline Air Pollution Station



https://www.csiro.au/greenhouse-gases/


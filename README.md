# Statistical Analysis in R: Regression, Simulation, and Bootstrap Inference

Four statistical analyses in R from my M.S. in Data Science coursework (ANA605: Analytic Models & Data Systems, National University). Each project works through a real or simulated dataset from data cleaning to modeling to written interpretation, and every analysis is fully reproducible from the code in this repository.

## Projects

| # | Project | Methods | Data |
|---|---|---|---|
| 01 | [Predicting Fuel Economy](01-fuel-economy-regression/analysis.md) | Data cleaning, variable recoding, correlation, multivariable regression with categorical predictors, residual comparison (SAD/SSD) | EPA Fuel Economy data, ~47,000 vehicles (1984–2025) |
| 02 | [Sampling Distributions of the Mean and SD](02-sampling-distributions/analysis.md) | Simulation, descriptive statistics (skewness, kurtosis), sampling distributions, standard error, Central Limit Theorem | Simulated: 1,000 samples × 100 ages |
| 03 | [Bootstrapping Property Values and Mortgage Payments](03-housing-survey-bootstrap/analysis.md) | Bootstrap resampling, standard error, percentile confidence intervals for skewed data | English Housing Survey 2008–09, 1,980 households |
| 04 | [Educational Attainment and State Median Income](04-education-income-regression/analysis.md) ([report](04-education-income-regression/report.pdf)) | Multivariable regression, confidence intervals, influence diagnostics (Cook's *D*), bootstrap vs. classical inference, APA-style report | U.S. Statistical Abstract 2012, 50 states + DC |

## Highlights

- **Fuel economy:** a model using engine displacement and drivetrain explained 55.5% of the variation in combined MPG across ~47,000 vehicles and outperformed a displacement + cylinders model on *R*², residual error, and SAD/SSD.
- **Sampling distributions:** from a flat, uniform population, the distribution of 1,000 sample means was nearly normal, and the estimated population mean was far closer to the true value than any single sample.
- **Housing survey:** despite heavy skew (skewness up to 5.6), bootstrap standard errors matched *SD*/√*n* within about 1%, supporting normal-based inference for the mean at *n* = 1,980.
- **Education and income:** high school and advanced degree attainment explained 55% of state-level variation in median income. The bootstrap showed the advanced-degree slope's standard error was more than twice the classical estimate, because one influential observation (DC) drove a bimodal sampling distribution.

## Tools

R · dplyr · ggplot2 · mosaic · knitr · R Markdown

## Repository structure

```
.
├── data/                              # datasets used by the analyses
├── 01-fuel-economy-regression/
│   ├── analysis.Rmd                   # code and write-up
│   ├── analysis.md                    # rendered report (view on GitHub)
│   └── figures/
├── 02-sampling-distributions/
├── 03-housing-survey-bootstrap/
└── 04-education-income-regression/
    └── report.pdf                     # full APA-style written report
```

## How to reproduce

1. Install R (4.x) and RStudio.
2. Install the packages:
   ```r
   install.packages(c("rmarkdown", "knitr", "dplyr", "ggplot2", "mosaic"))
   ```
3. Clone the repository, open any `analysis.Rmd`, and click **Knit**. Paths are relative to the project folder, and every simulation and bootstrap uses a fixed seed.

## Contact

Robby Dosanjh

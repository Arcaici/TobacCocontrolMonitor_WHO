# TobacCocontrolMonitor_WHO
Exploratory Data Analysis of Tobacco Usage (2010-2020)
------------------------------------------------------
This repository contains an exploratory data analysis (EDA) of tobacco usage trends from 2010 to 2020. The analysis utilizes data from the World Health Organization (WHO) and aims to provide insights into tobacco consumption patterns, differences between genders, and variations across geographic regions.

## Introduction

Tobacco usage is a significant global health concern, leading to millions of deaths each year. This EDA seeks to understand and visualize tobacco consumption trends over the last decade, shedding light on differences between male and female populations and variations among different geographic areas.

## Dataset

The dataset used for this analysis is called 'Non-age-standardized estimates of current tobacco use, tobacco smoking, and cigarette smoking (Tobacco Control: Monitor),' provided by the World Health Organization. It contains information on tobacco consumption percentages for various countries, years, and genders. The data is based on a statistical model that estimates tobacco use in populations aged 15 and over.

## Exploratory Analysis

### Value Distribution

- A frequency distribution plot of tobacco consumption values for both sexes in 2020 reveals a peak around 10 percent, with a concentration between 5 and 10 percent.
- Further analysis shows that the density distribution of males appears close to normal, while females exhibit a different distribution.

### Distribution by Geographic Areas

- Violin plots depict the distribution of tobacco consumption values across six geographic areas in 2020.
- The Western Pacific area has the highest percentage of tobacco consumers when considering both sexes.
- When divided by sex, the highest values are found in different areas for males and females.

### Mean Value Over Geographic Areas

- Histogram plots display the mean tobacco consumption values for various geographic areas in 2020.
- Europe and South-East Asia have higher mean values compared to other areas, with variations between males and females.

### Mean Value Over Years

- Time series plots reveal the trend in mean tobacco consumption values from 2000 to 2020.
- All areas show a decreasing trend in mean values, with variations among geographic regions and between genders.

## Statistical Tests

- Shapiro-Wilk tests indicate that the data does not follow a normal distribution.
- Bartlett tests confirm homoscedasticity, while sphericity tests suggest no significant impact.
- The Friedman test and repeated measures ANOVA are applied to test the hypothesis regarding mean values.
- Post-hoc Mann-Whitney tests with Bonferroni and Benjamini-Hochberg corrections identify significant differences between years.

## Conclusion

- Males have a higher tobacco consumption rate compared to females across all geographic areas.
- Geographic variations in tobacco consumption suggest targeted awareness campaigns.
- Mean tobacco consumption has decreased over the last decade, with differences between genders.
- Future studies can explore age-related trends and variations within specific countries.

This EDA provides valuable insights into tobacco usage trends, aiding in the development of effective public health strategies to combat tobacco-related health issues.

For detailed analysis and code, please refer to the Jupyter Notebook provided in this repository.


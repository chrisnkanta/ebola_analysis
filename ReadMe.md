---
title: "Ebola Analysis"
output: prettydoc::html_pretty
date: "2026-10-17"
---

```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  janitor,
  inspectdf,
  knitr,
  flextable,
  here # new package we will use soon
)

ebola_sierra_leone <- read_csv(here("data/ebola_sierra_leone.csv"))
```

# Introduction


Ebola virus disease (EVD) is a severe infectious disease, and the 2014–2016 West African Ebola epidemic significantly affected Sierra Leone. The outbreak highlighted the importance of disease surveillance, reliable data collection, and effective public health interventions.

This project uses R to analyze Ebola data from Sierra Leone, focusing on age, sex, and case status. Using packages such as dplyr and ggplot2, the analysis applies descriptive statistics, frequency tables, histograms, and bar charts to explore the distribution of reported cases.

The main objective is to identify demographic patterns among recorded Ebola cases and demonstrate how data analysis can support public health research. The findings are interpreted with consideration of data quality and the limitations of case counts in measuring infection risk.

Sierra Leone Ebola dataset

```{r echo=FALSE}
# View the first six rows
kable(head(ebola_sierra_leone, 10))
```

# Abstract

The Ebola virus disease outbreak in Sierra Leone was a major public health emergency that formed part of the 2014–2016 West African Ebola epidemic. This data analysis project examines Ebola-related data from Sierra Leone to explore the demographic characteristics and distribution of reported cases. Using R programming, the analysis focuses on variables such as age, sex, and case status to identify patterns among individuals recorded in the dataset.

Data cleaning, descriptive statistics, frequency tables, and graphical visualizations are used to summarize the available information. Histograms help illustrate the distribution of age, while frequency tables and bar charts support comparisons of recorded cases by sex and case status. The analysis aims to provide a clear understanding of the demographic patterns present in the dataset and demonstrate how statistical programming can support the interpretation of public health data.

The findings are intended to contribute to a better understanding of the recorded Ebola cases in Sierra Leone and highlight the value of data analytics in epidemiological surveillance and public health planning. The conclusions are limited by the completeness, accuracy, and scope of the available dataset, and the results should not be interpreted as estimates of infection risk without appropriate population denominators.


# Data preparation and cleaning in R

To see the number of missing values in the key variables

```{r echo=FALSE}
# Count missing values in each column
ebola_sierra_leone |>
  summarise(
    missing_status = sum(is.na(status)),
    missing_age = sum(is.na(age)),
    missing_sex = sum(is.na(sex)),
    missing_district = sum(is.na(district))) |> 
  kable()
```

Checking for duplicate rows

```{r echo=FALSE}
# Count duplicate rows
kable(sum(duplicated(ebola_sierra_leone)))
```

Checking for distinct count

```{r echo=FALSE}
# Checking for distinct count
ebola_sierra_leone_clean <-
  ebola_sierra_leone |>
  distinct()
kable(head(ebola_sierra_leone_clean, 10))
```


# Demographic analysis

Determine the number of confirmed and suspected cases

```{r echo=FALSE}
# Determine the number of confirmed and suspected cases by sex
ebola_sierra_leone_status <-
  ebola_sierra_leone |> 
  select(status, age, sex) |> 
  count(status, name = "number")
kable(ebola_sierra_leone_status)
```

Determine the number of confirmed cases by sex

The analysis of the Sierra Leone Ebola dataset examined the number of people affected by Ebola according to their case status. The results indicated that more females were recorded as affected by Ebola than males.


```{r echo=FALSE}
# checking for the gender with the highest affected cases
ebola_sierra_leone_sex_status <- ebola_sierra_leone |>
  select(status, age, sex) |>
  mutate(
    sex    = tolower(sex),
    status = tolower(status)
  ) |>
  filter(status == "confirmed", sex %in% c("f", "m")) |>
  count(sex, name = "confirmed_cases") |>
  mutate(sex = recode(sex, f = "Female", m = "Male"))

kable(ebola_sierra_leone_sex_status)
```


# Summary of the age group

- Child (All ages less than 18)

- Young Adult (All ages greater than and equal to 18 but less than 30)

- Adult (All ages greater than and equal to 30)

```{r echo=FALSE}
# Summary of the age group
ebola_sierra_leone_age <-
  ebola_sierra_leone |>
  select(age) |> 
  mutate(age_group = case_when(age < 18 ~ "Child",
                               age >= 18 & age < 30 ~"Young Adult",
                               age >= 30 ~ "Adult",
                               is.na(age) ~ "No age")) |> 
  tabyl(age_group)
kable(ebola_sierra_leone_age)
```



Analyze the age distribution of reported Ebola cases using age-group classifications

```{r echo=FALSE}
# Report by age_group
ebola_sierra_leone_age_sex <- ebola_sierra_leone |>
  select(status, age, sex) |>
  mutate(
    sex    = tolower(sex),
    status = tolower(status),
    age_group = case_when(
      is.na(age)         ~ "Unknown",
      age < 18           ~ "Child",
      age >= 18 & age < 30 ~ "Young Adult",
      age >= 30          ~ "Adult"
    )
  ) |>
  filter(status == "confirmed", sex %in% c("f", "m")) |>
  count(age_group, sex, name = "confirmed_cases") |>
  mutate(sex = recode(sex, f = "Female", m = "Male"))
kable(ebola_sierra_leone_age_sex)
```

Analyze the age group  distribution of reported Ebola cases using histograms


```{r echo=FALSE}
# Age group distribution using histogram
ebola_sierra_leone_age_sex |>
  mutate(age_group = factor(age_group,
                            levels = c("Child", "Young Adult", "Adult", "Unknown"))) |>
  ggplot(aes(x = age_group, y = confirmed_cases, fill = sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = confirmed_cases),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Female" = "#E07A5F", "Male" = "#3D5A80")) +
  labs(
    title = "Confirmed Ebola Cases in Sierra Leone by Age Group and Sex",
    x = "Age group",
    y = "Confirmed cases",
    fill = "Sex"
  ) +
  theme_minimal()

```


# Cases by district

This table shows counts of cases by district:

```{r echo=FALSE}
# Cases by district
district_tab <- tabyl(ebola_sierra_leone, district)
flextable::flextable(district_tab)
```


Visualize district variables

```{r echo=FALSE}
# Visualize district variables
district_tab |>
  ggplot(aes(x = reorder(district, -n), y = n)) +
  geom_col(fill = "#3D5A80") +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  labs(
    title = "Ebola Cases in Sierra Leone by District",
    x = "District",
    y = "Number of cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


# Visualize categorical variables

```{r echo=FALSE}
categ_vars_plot<- show_plot(inspect_cat(ebola_sierra_leone))
categ_vars_plot
```



# Conclusion

The analysis of the Sierra Leone Ebola dataset provided insights into the demographic distribution of recorded Ebola cases by age, sex, and case status. Using R, the data was organized, summarized, and visualized to identify patterns among the individuals represented in the dataset. The results indicated that more females were recorded as affected by Ebola than males, highlighting a difference in the number of reported cases between the two groups. The analysis also examined the distribution of cases across different age groups and case classifications.

Overall, this project demonstrates the importance of data analysis and visualization in understanding public health data. Although the findings help describe the characteristics of recorded Ebola cases, further analysis using population data and additional epidemiological information would be needed to determine differences in infection risk. These insights emphasize the value of accurate data collection and analysis in supporting public health planning and disease surveillance.



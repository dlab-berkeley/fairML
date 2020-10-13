---
knitr: "bookdown::render_book"
title: "Fairness and Bias in Machine Learning Workshop"
author: ["Jae Yeon Kim", "Aniket Kesari", "Renata Barreto", "Avery Richard"]
date: "2020-10-12"
site: bookdown::bookdown_site
github-repo: dlab-berkeley/fairMLworkshop
output: bookdown::gitbook
documentclass: book
bibliography:
  - book.bib
  - packages.bib
biblio-style: apalike
link-citations: yes
colorlinks: yes 
description: "Fairness and Bias in Machine Learning Workshop"
---



# Redoing ProPublica Analysis of the COMPAS dataset

We have revised the ProPublica's Analysis of the COMPAS dataset to increase code readability and make changing data analysis and visualization easier.

1. Bias in the data 
- Logistic regression analysis 
  - Risk of Recidivism Data
  - Risk of Violent Recidivism Data

2. Bias in the algorithm 
- Survival analysis

For more information on the ProPublica's Machine Bias project, I encourage to check out [this article](https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing).

[Argument](https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing/) by Julia Angwin, Jeff Larson, Surya Mattu and Lauren Kirchner

[Counterargument](https://www.washingtonpost.com/news/monkey-cage/wp/2016/10/17/can-an-algorithm-be-racist-our-analysis-is-more-cautious-than-propublicas/) by Sam Corbett-Davies, Emma Pierson, Avi Feller and Sharad Goel

[Methodology](https://www.propublica.org/article/how-we-analyzed-the-compas-recidivism-algorithm/)

[Original Notebook in R](https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb)

<!--chapter:end:index.Rmd-->

# Risk of Recidivism Analysis

## Setup 


```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(
 tidyverse, # tidyverse packages 
 conflicted, # an alternative conflict resolution strategy 
 ggthemes, # other themes for ggplot2 
 patchwork, # arranging ggplots
 scales, # rescaling 
 survival, # survival analysis
 broom, # for modeling
 here, # reproducibility 
 glue # pasting strings and objects 
)

# To avoid conflicts 
conflict_prefer("filter", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::filter over any other package
```

```r
conflict_prefer("select", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::select over any other package
```

```r
# Set themes 
theme_set(ggthemes::theme_fivethirtyeight())
```

## Load data 

We select fields for severity of charge, number of priors, demographics, age, sex, COMPAS scores, and whether each person was accused of a crime within two years.


```r
two_years <- read_csv(here("data", "compas-scores-two-years.csv"))
```

```
## Warning: Duplicated column names deduplicated: 'decile_score' =>
## 'decile_score_1' [40], 'priors_count' => 'priors_count_1' [49]
```

```r
glue("N of observations (rows): {nrow(two_years)}
      N of variables (columns): {ncol(two_years)}")
```

```
## N of observations (rows): 7214
## N of variables (columns): 53
```

## Wrangling 

- Not all of the observations are useable for the first round of analysis.
- There are a number of reasons to remove rows because of missing data:
    - If the charge date of a defendants COMPAS scored crime was not within 30 days from when the person was arrested, we assume that because of data quality reasons, that we do not have the right offense.
    - We coded the recidivist flag -- is_recid -- to be -1 if we could not find a COMPAS case at all.
    - In a similar vein, ordinary traffic offenses -- those with a c_charge_degree of 'O' -- will not result in Jail time are removed (only two of them).
    - We filtered the underlying data from Broward county to include only those rows representing people who had either recidivated in two years, or had at least two years outside of a correctional facility.

### Create a function 


```r
wrangle_data <- function(data){

df <- data %>% 
    
    # Select variables 
    select(age, c_charge_degree, race, age_cat, score_text, sex, priors_count, days_b_screening_arrest, decile_score, is_recid, two_year_recid, 
         c_jail_in, c_jail_out) %>% 
    # Filter rows 
    filter(days_b_screening_arrest <= 30,
           days_b_screening_arrest >= -30, 
           is_recid != -1,
           c_charge_degree != "O",
           score_text != 'N/A') %>% 
    # Mutate variables 
    mutate(length_of_stay = as.numeric(as.Date(c_jail_out) - as.Date(c_jail_in)),
           c_charge_degree = factor(c_charge_degree),
           age_cat = factor(age_cat),
           race = factor(race, levels = c("Caucasian","African-American","Hispanic","Other","Asian","Native American")),
           sex = factor(sex, levels = c("Male","Female")),
           score_text = factor(score_text, levels = c("Low", "Medium", "High")),
           score = score_text,
# I added this new variable to test whether measuring the DV as a binary or continuous var makes a difference 
           score_num = as.numeric(score_text)) %>% 
    # Rename variables 
    rename(crime = c_charge_degree,
           gender = sex)
        
return(df)}
```

### Apply the function to the data 


```r
df <- wrangle_data(two_years)

names(df)
```

```
##  [1] "age"                     "crime"                  
##  [3] "race"                    "age_cat"                
##  [5] "score_text"              "gender"                 
##  [7] "priors_count"            "days_b_screening_arrest"
##  [9] "decile_score"            "is_recid"               
## [11] "two_year_recid"          "c_jail_in"              
## [13] "c_jail_out"              "length_of_stay"         
## [15] "score"                   "score_num"
```

```r
# Check whether the function works as expected
head(df, 5)
```

```
## # A tibble: 5 x 16
##     age crime race  age_cat score_text gender priors_count days_b_screenin…
##   <dbl> <fct> <fct> <fct>   <fct>      <fct>         <dbl>            <dbl>
## 1    69 F     Other Greate… Low        Male              0               -1
## 2    34 F     Afri… 25 - 45 Low        Male              0               -1
## 3    24 F     Afri… Less t… Low        Male              4               -1
## 4    44 M     Other 25 - 45 Low        Male              0                0
## 5    41 F     Cauc… 25 - 45 Medium     Male             14               -1
## # … with 8 more variables: decile_score <dbl>, is_recid <dbl>,
## #   two_year_recid <dbl>, c_jail_in <dttm>, c_jail_out <dttm>,
## #   length_of_stay <dbl>, score <fct>, score_num <dbl>
```

## Descriptive analysis 

- Higher COMPAS scores are slightly correlated with a longer length of stay.


```r
cor(df$length_of_stay, df$decile_score)
```

```
## [1] 0.2073297
```

```r
df %>%
  group_by(score) %>%
  count() %>%
  ggplot(aes(x = score, y = n)) +
    geom_col() +
    labs(x = "Score",
         y = "Count",
         title = "Score distribution")
```

<img src="01_risk_of_recidivism_analysis_files/figure-html/unnamed-chunk-5-1.png" width="672" />
Judges are often presented with two sets of scores from the COMPAS system -- one that classifies people into High, Medium and Low risk, and a corresponding decile score. There is a clear downward trend in the decile scores as those scores increase for white defendants.


```r
df %>%
  ggplot(aes(ordered(decile_score))) + 
          geom_bar() +
          facet_wrap(~race, nrow = 2) +
          labs(x = "Decile Score",
               y = "Count",
               Title = "Defendant's Decile Score")
```

<img src="01_risk_of_recidivism_analysis_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Modeling 

After filtering out bad rows, our first question is whether there is a significant difference in COMPAS scores between races. To do so we need to change some variables into factors, and run a logistic regression, comparing low scores to high scores.

### Model building 


```r
model_data <- function(data){

# Logistic regression model
lr_model <- glm(score ~ gender + age_cat + race + priors_count + crime + two_year_recid, 
             family = "binomial", data = data)

# OLS, DV = score_num
ols_model1 <- lm(score_num ~ gender + age_cat + race + priors_count + crime + two_year_recid, data = data)

# OLS, DV = decile_score 
ols_model2 <- lm(decile_score ~ gender + age_cat + race + priors_count + crime + two_year_recid, data = data)

# Extract model outcomes with confidence intervals 
lr_est <- lr_model %>% 
    tidy(conf.int = TRUE) 

ols_est1 <- ols_model1 %>%
    tidy(conf.int = TRUE) 

ols_est2 <- ols_model2 %>%
    tidy(conf.int = TRUE) 

# AIC scores 
lr_AIC <- AIC(lr_model)
ols_AIC1 <- AIC(ols_model1)
ols_AIC2 <- AIC(ols_model2)
    
list(lr_est, ols_est1, ols_est2, 
     lr_AIC, ols_AIC1, ols_AIC2)

}
```

### Model comparisons 


```r
glue("AIC score of logistic regression: {model_data(df)[4]} 
      AIC score of OLS regression (with categorical DV):  {model_data(df)[5]}
      AIC score of OLS regression (with continuous DV): {model_data(df)[6]}")
```

```
## AIC score of logistic regression: 6192.40169473357 
## AIC score of OLS regression (with categorical DV):  11772.1148541111
## AIC score of OLS regression (with continuous DV): 26779.9512226999
```

### Logistic regression model 


```r
lr_model <- model_data(df)[1] %>% data.frame()

lr_model %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("race|age_cat|gender|M","", term)) %>%
  ggplot(aes(x = fct_reorder(term, estimate), y = estimate, ymax = conf.high, ymin = conf.low)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = "Estimate", x = "",
      title = "Logistic regression") +
  geom_hline(yintercept = 0, linetype = "dashed")
```

<img src="01_risk_of_recidivism_analysis_files/figure-html/unnamed-chunk-9-1.png" width="672" />


```r
interpret_estimate <- function(model){
    
    # Control 
    intercept <- model$estimate[model$term == "(Intercept)"]
    control <- exp(intercept) / (1 + exp(intercept))
    
    # Likelihood 
    model <- model %>% filter(term != "(Intercept)")
    
    model$likelihood <- (exp(model$estimate) / (1 - control + (control * exp(model$estimate))))
    
    return(model)
}
```


```r
interpret_estimate(lr_model) %>%
    mutate(term = gsub("race|age_cat|gender","", term)) %>% 
    ggplot(aes(x = fct_reorder(term, likelihood), y = likelihood)) +
        geom_point(size = 3) +
        coord_flip() +
        labs(y = "Likelihood", x = "",
            title ="Logistic regression") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        geom_hline(yintercept = 1, linetype = "dashed")
```

<img src="01_risk_of_recidivism_analysis_files/figure-html/unnamed-chunk-11-1.png" width="672" />










<!--chapter:end:01_risk_of_recidivism_analysis.Rmd-->

# Risk of Violent Recidivism

## Setup 


```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(
 tidyverse, # tidyverse packages 
 conflicted, # an alternative conflict resolution strategy 
 ggthemes, # other themes for ggplot2 
 patchwork, # arranging ggplots
 scales, # rescaling 
 survival, # survival analysis
 broom, # for modeling
 here, # reproducibility 
 glue # pasting strings and objects 
)

# To avoid conflicts 
conflict_prefer("filter", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::filter over any other package
```

```r
conflict_prefer("select", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::select over any other package
```

```r
# Set themes 
theme_set(ggthemes::theme_fivethirtyeight())
```

## Load data 


```r
two_years_violent <- read_csv(here("data" ,"compas-scores-two-years-violent.csv"))
```

```
## Warning: Duplicated column names deduplicated: 'decile_score' =>
## 'decile_score_1' [40], 'priors_count' => 'priors_count_1' [49], 'two_year_recid'
## => 'two_year_recid_1' [54]
```

```
## 
## ── Column specification ──────────────────────────────
## cols(
##   .default = col_double(),
##   name = col_character(),
##   first = col_character(),
##   last = col_character(),
##   compas_screening_date = col_date(format = ""),
##   sex = col_character(),
##   dob = col_date(format = ""),
##   age_cat = col_character(),
##   race = col_character(),
##   c_jail_in = col_datetime(format = ""),
##   c_jail_out = col_datetime(format = ""),
##   c_case_number = col_character(),
##   c_offense_date = col_date(format = ""),
##   c_arrest_date = col_date(format = ""),
##   c_charge_degree = col_character(),
##   c_charge_desc = col_character(),
##   r_case_number = col_character(),
##   r_charge_degree = col_character(),
##   r_offense_date = col_date(format = ""),
##   r_charge_desc = col_character(),
##   r_jail_in = col_date(format = "")
##   # ... with 14 more columns
## )
## ℹ Use `spec()` for the full column specifications.
```

```r
glue("N of observations (rows): {nrow(two_years_violent)}
      N of variables (columns): {ncol(two_years_violent)}")
```

```
## N of observations (rows): 4743
## N of variables (columns): 54
```

## Wrangling

### Create a function 


```r
wrangle_data <- function(data){

df <- data %>% 
    
    # Select variables 
    select(age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count, 
         days_b_screening_arrest, v_decile_score, is_recid, two_year_recid) %>%            
    # Filter rows 
    filter(days_b_screening_arrest <= 30,
           days_b_screening_arrest >= -30, 
           is_recid != -1,
           c_charge_degree != "O",
           v_score_text != 'N/A') %>% 
    # Mutate variables 
    mutate(c_charge_degree = factor(c_charge_degree),
           age_cat = factor(age_cat),
           race = factor(race, levels = c("Caucasian","African-American","Hispanic","Other","Asian","Native American")),
           sex = factor(sex, levels = c("Male","Female")),
           v_score_text = factor(v_score_text, levels = c("Low", "Medium", "High")),
# I added this new variable to test whether measuring the DV as a binary or continuous var makes a difference 
           score_num = as.numeric(v_score_text)) %>%
    # Rename variables 
    rename(crime = c_charge_degree,
           gender = sex,
           score = v_score_text)
        
return(df)}
```

### Apply the function to the data 


```r
df <- wrangle_data(two_years_violent)

names(df)
```

```
##  [1] "age"                     "crime"                  
##  [3] "race"                    "age_cat"                
##  [5] "score"                   "gender"                 
##  [7] "priors_count"            "days_b_screening_arrest"
##  [9] "v_decile_score"          "is_recid"               
## [11] "two_year_recid"          "score_num"
```

```r
head(df, 5) # Check whether the function works as expected 
```

```
## # A tibble: 5 x 12
##     age crime race  age_cat score gender priors_count days_b_screenin…
##   <dbl> <fct> <fct> <fct>   <fct> <fct>         <dbl>            <dbl>
## 1    69 F     Other Greate… Low   Male              0               -1
## 2    34 F     Afri… 25 - 45 Low   Male              0               -1
## 3    44 M     Other 25 - 45 Low   Male              0                0
## 4    43 F     Other 25 - 45 Low   Male              3               -1
## 5    39 M     Cauc… 25 - 45 Low   Female            0               -1
## # … with 4 more variables: v_decile_score <dbl>, is_recid <dbl>,
## #   two_year_recid <dbl>, score_num <dbl>
```

### Descriptive analysis 

Score distribution 


```r
df %>%
  group_by(score) %>%
  count() %>%
  ggplot(aes(x = score, y = n)) +
    geom_col() +
    labs(x = "Score",
         y = "Count",
         title = "Score distribution")
```

<img src="02_risk_of_violent_recidivism_files/figure-html/unnamed-chunk-5-1.png" width="672" />

Score distribution by race


```r
df %>%
  ggplot(aes(ordered(v_decile_score))) + 
          geom_bar() +
          facet_wrap(~race, nrow = 2) +
          labs(x = "Decile Score",
               y = "Count",
               Title = "Defendant's Decile Score")
```

<img src="02_risk_of_violent_recidivism_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### Modeling 

After filtering out bad rows, our first question is whether there is a significant difference in COMPAS scores between races. To do so we need to change some variables into factors, and run a logistic regression, comparing low scores to high scores.


```r
model_data <- function(data){

# Logistic regression model
lr_model <- glm(score ~ gender + age_cat + race + priors_count + crime + two_year_recid, 
             family = "binomial", data = data)

# OLS
ols_model1 <- lm(score_num ~ gender + age_cat + race + priors_count + crime + two_year_recid, 
             data = data)

ols_model2 <- lm(v_decile_score ~ gender + age_cat + race + priors_count + crime + two_year_recid, 
             data = data)

# Extract model outcomes with confidence intervals 
lr_est <- lr_model %>% 
    tidy(conf.int = TRUE) 

ols_est1 <- ols_model1 %>%
    tidy(conf.int = TRUE) 

ols_est2 <- ols_model2 %>%
    tidy(conf.int = TRUE) 

# AIC scores 
lr_AIC <- AIC(lr_model)
ols_AIC1 <- AIC(ols_model1)
ols_AIC2 <- AIC(ols_model2)
    
list(lr_est, ols_est1, ols_est2, lr_AIC, ols_AIC1, ols_AIC2)
}
```

#### Model comparisons 


```r
glue("AIC score of logistic regression: {model_data(df)[4]} 
      AIC score of OLS regression (with categorical DV):  {model_data(df)[5]}
      AIC score of OLS regression (with continuous DV): {model_data(df)[6]}")
```

```
## AIC score of logistic regression: 3022.77943765996 
## AIC score of OLS regression (with categorical DV):  5414.49127581608
## AIC score of OLS regression (with continuous DV): 15458.3861723106
```

#### Logistic regression model 


```r
lr_model <- model_data(df)[1] %>% 
  data.frame()

lr_model %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("race|age_cat|gender","", term)) %>%
  ggplot(aes(x = fct_reorder(term, estimate), y = estimate, ymax = conf.high, ymin = conf.low)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = "Estimate", x = "",
      title = "Logistic regression") +
  geom_hline(yintercept = 0, linetype = "dashed")
```

<img src="02_risk_of_violent_recidivism_files/figure-html/unnamed-chunk-9-1.png" width="672" />


```r
interpret_estimate <- function(model){
    
    # Control 
    intercept <- model$estimate[model$term == "(Intercept)"]
    control <- exp(intercept) / (1 + exp(intercept))
    
    # Likelihood 
    model <- model %>% filter(term != "(Intercept)")
    
    model$likelihood <- (exp(model$estimate) / (1 - control + (control * exp(model$estimate))))
    
    return(model)
}

interpret_estimate(lr_model) %>%
    mutate(term = gsub("race|age_cat|gender","", term)) %>% 
    ggplot(aes(x = fct_reorder(term, likelihood), y = likelihood)) +
        geom_point(size = 3) +
        coord_flip() +
        labs(y = "Likelihood", x = "",
            title ="Logistic regression") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        geom_hline(yintercept = 1, linetype = "dashed")
```

<img src="02_risk_of_violent_recidivism_files/figure-html/unnamed-chunk-10-1.png" width="672" />










<!--chapter:end:02_risk_of_violent_recidivism.Rmd-->

# Risk of Violent Recidivism

- In order to test whether Compas scores do an accurate job of deciding whether an offender is Low, Medium or High risk, we ran a Cox Proportional Hazards model. Northpointe, the company that created COMPAS and markets it to Law Enforcement, also ran a Cox model in [their validation study](https://journals.sagepub.com/doi/abs/10.1177/0093854808326545).

- We used the counting model and removed people when they were incarcerated. Due to errors in the underlying jail data, we need to filter out 32 rows that have an end date more than the start date. Considering that there are 13,334 total rows in the data, such a small amount of errors will not affect the results.

## Setup 


```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(
 tidyverse, # tidyverse packages 
 conflicted, # an alternative conflict resolution strategy 
 ggthemes, # other themes for ggplot2 
 patchwork, # arranging ggplots
 scales, # rescaling 
 survival, # survival analysis
 broom, # for modeling
 here, # reproducibility 
 glue, # pasting strings and objects 
 reticulate # source python codes
)

# To avoid conflicts 
conflict_prefer("filter", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::filter over any other package
```

```r
conflict_prefer("select", "dplyr") 
```

```
## [conflicted] Will prefer dplyr::select over any other package
```

```r
# Set themes 
theme_set(ggthemes::theme_fivethirtyeight())
```


## Load data 


```r
cox_data <- read_csv(here("data" ,"cox-parsed.csv"))
```

```
## Warning: Duplicated column names deduplicated: 'decile_score' =>
## 'decile_score_1' [40], 'priors_count' => 'priors_count_1' [49]
```

```
## 
## ── Column specification ──────────────────────────────
## cols(
##   .default = col_character(),
##   id = col_double(),
##   compas_screening_date = col_date(format = ""),
##   dob = col_date(format = ""),
##   age = col_double(),
##   juv_fel_count = col_double(),
##   decile_score = col_double(),
##   juv_misd_count = col_double(),
##   juv_other_count = col_double(),
##   priors_count = col_double(),
##   days_b_screening_arrest = col_double(),
##   c_jail_in = col_datetime(format = ""),
##   c_jail_out = col_datetime(format = ""),
##   c_offense_date = col_date(format = ""),
##   c_arrest_date = col_date(format = ""),
##   c_days_from_compas = col_double(),
##   is_recid = col_double(),
##   r_days_from_arrest = col_double(),
##   r_offense_date = col_date(format = ""),
##   r_jail_in = col_date(format = ""),
##   r_jail_out = col_date(format = "")
##   # ... with 13 more columns
## )
## ℹ Use `spec()` for the full column specifications.
```

```r
glue("N of observations (rows): {nrow(cox_data)}
      N of variables (columns): {ncol(cox_data)}")
```

```
## N of observations (rows): 13419
## N of variables (columns): 52
```

## Wrangling


```r
df <- cox_data %>% 
    filter(score_text != "N/A") %>%
    filter(end > start) %>%
    mutate(c_charge_degree = factor(c_charge_degree),
           age_cat = factor(age_cat),
           race = factor(race, levels = c("Caucasian","African-American","Hispanic","Other","Asian","Native American")),
           sex = factor(sex, levels = c("Male","Female")),
           score_factor = factor(score_text, levels = c("Low", "Medium", "High")))

grp <- df[!duplicated(df$id),]
```

### Descriptive analysis 

Score distribution 


```r
grp %>% 
    group_by(score_factor) %>%
      count() %>%
      ggplot(aes(x = score_factor, y = n)) +
        geom_col() +
        labs(x = "Score",
             y = "Count",
             title = "Score distribution")
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Score distribution by race


```r
df %>%
  ggplot(aes(ordered(score_factor))) + 
          geom_bar() +
          facet_wrap(~race, nrow = 2) +
          labs(x = "Decile Score",
               y = "Count",
               Title = "Defendant's Decile Score")
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-5-1.png" width="672" />

### Modeling 


```r
f2 <- Surv(start, end, event, type="counting") ~ race + score_factor + race * score_factor

model <- coxph(f2, data = df)

model %>%
  broom::tidy(conf.int = TRUE) %>%
  mutate(term = gsub("race|score_factor","", term)) %>% 
  filter(term != "<chr>") %>%
  ggplot(aes(x = fct_reorder(term, estimate), y = estimate, ymax = conf.high, ymin = conf.low)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = "Estimate", x = "")
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-6-1.png" width="672" />

The interaction term shows a similar disparity as the logistic regression above.

High risk white defendants are 3.61 more likely than low risk white defendants, while High risk black defendants are 2.99 more likely than low.


```r
visualize_surv <- function(input){
  
f <- Surv(start, end, event, type="counting") ~ score_factor

fit <- survfit(f, data = input)

fit %>%
    tidy(conf.int = TRUE) %>%
    mutate(strata = gsub("score_factor=","", strata)) %>%
    mutate(strata = factor(strata, levels = c("High","Medium","Low"))) %>%
    ggplot(aes(x = time, y = estimate, ymax = conf.high, ymin = conf.low, group = strata, col = strata)) +
    geom_pointrange(alpha = 0.1) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    ylim(c(0, 1)) +
    labs(x = "Time", y = "Estimated survival rate", col = "Strata")}
```


```r
visualize_surv(df) + ggtitle("Overall")
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Black defendants do recidivate at higher rates according to race specific Kaplan Meier plots.


```r
(df %>% filter(race == "Caucasian") %>% visualize_surv() + ggtitle("Caucasian")) /
(df %>% filter(race == "African-American") %>% visualize_surv() + ggtitle("African-American")) 
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-9-1.png" width="672" />

In terms of underlying recidivism rates, we can look at gender specific Kaplan Meier estimates. There is a striking difference between women and men.


```r
(df %>% filter(sex == "Female") %>% visualize_surv() + ggtitle("Female")) /

(df %>% filter(sex == "Male") %>% visualize_surv() + ggtitle("Male"))
```

<img src="03_algorithm_accuracy_files/figure-html/unnamed-chunk-10-1.png" width="672" />

As these plots show, the COMPAS score treats a High risk women the same as a Medium risk man.

### Risk of Recidivism Accuracy 

The above analysis shows that the Compas algorithm does overpredict African-American defendant's future recidivism, but we haven't yet explored the direction of the bias. We can discover fine differences in overprediction and underprediction by comparing Compas scores across racial lines.


```r
# This directory location may be different depending on where you installed python
# Type which python in the command line 
use_python("/home/jae/anaconda3/bin/python")
```



```python

from truth_tables import PeekyReader, Person, table, is_race, count, vtable, hightable, vhightable
from csv import DictReader

people = []

with open("./data/cox-parsed.csv") as f:
    reader = PeekyReader(DictReader(f))
    try:
        while True:
            p = Person(reader)
            if p.valid:
                people.append(p)
    except StopIteration:
        pass

pop = list(filter(lambda i: ((i.recidivist == True and i.lifetime <= 730) or
                              i.lifetime > 730), list(filter(lambda x: x.score_valid, people))))

recid = list(filter(lambda i: i.recidivist == True and i.lifetime <= 730, pop))

recid
```

```
## [<truth_tables.Person object at 0x7fe4a3909390>, <truth_tables.Person object at 0x7fe4a39095f8>, <truth_tables.Person object at 0x7fe4a390e0f0>, <truth_tables.Person object at 0x7fe4a390ee10>, <truth_tables.Person object at 0x7fe4a3913588>, <truth_tables.Person object at 0x7fe4a3913ac8>, <truth_tables.Person object at 0x7fe4a3918080>, <truth_tables.Person object at 0x7fe4a3918b38>, <truth_tables.Person object at 0x7fe4a3918e10>, <truth_tables.Person object at 0x7fe4a3918f28>, <truth_tables.Person object at 0x7fe4a391e2e8>, <truth_tables.Person object at 0x7fe4a391e588>, <truth_tables.Person object at 0x7fe4a391ec88>, <truth_tables.Person object at 0x7fe4a391ecf8>, <truth_tables.Person object at 0x7fe4a38a90b8>, <truth_tables.Person object at 0x7fe4a38a9518>, <truth_tables.Person object at 0x7fe4a38a9d68>, <truth_tables.Person object at 0x7fe4a38ae048>, <truth_tables.Person object at 0x7fe4a38ae860>, <truth_tables.Person object at 0x7fe4a38b2470>, <truth_tables.Person object at 0x7fe4a38b2710>, <truth_tables.Person object at 0x7fe4a38b2898>, <truth_tables.Person object at 0x7fe4a38baba8>, <truth_tables.Person object at 0x7fe4a38bae80>, <truth_tables.Person object at 0x7fe4a38bacf8>, <truth_tables.Person object at 0x7fe4a38bad68>, <truth_tables.Person object at 0x7fe4a38beef0>, <truth_tables.Person object at 0x7fe4a38c6470>, <truth_tables.Person object at 0x7fe4a38c6c88>, <truth_tables.Person object at 0x7fe4a38c6eb8>, <truth_tables.Person object at 0x7fe4a38cbf98>, <truth_tables.Person object at 0x7fe4a38cf630>, <truth_tables.Person object at 0x7fe4a38cbda0>, <truth_tables.Person object at 0x7fe4a38cffd0>, <truth_tables.Person object at 0x7fe4a38cbe48>, <truth_tables.Person object at 0x7fe4a38cbcf8>, <truth_tables.Person object at 0x7fe4a38d6c18>, <truth_tables.Person object at 0x7fe4a38dc4e0>, <truth_tables.Person object at 0x7fe4a38dcf60>, <truth_tables.Person object at 0x7fe4a38dcc50>, <truth_tables.Person object at 0x7fe4a38e0da0>, <truth_tables.Person object at 0x7fe4a3865cf8>, <truth_tables.Person object at 0x7fe4a386b0b8>, <truth_tables.Person object at 0x7fe4a386b9e8>, <truth_tables.Person object at 0x7fe4a386be10>, <truth_tables.Person object at 0x7fe4a386b390>, <truth_tables.Person object at 0x7fe4a3871358>, <truth_tables.Person object at 0x7fe4a3871630>, <truth_tables.Person object at 0x7fe4a3871908>, <truth_tables.Person object at 0x7fe4a3871e48>, <truth_tables.Person object at 0x7fe4a3871da0>, <truth_tables.Person object at 0x7fe4a3877748>, <truth_tables.Person object at 0x7fe4a3877be0>, <truth_tables.Person object at 0x7fe4a3877ef0>, <truth_tables.Person object at 0x7fe4a387cc18>, <truth_tables.Person object at 0x7fe4a3882780>, <truth_tables.Person object at 0x7fe4a3882cf8>, <truth_tables.Person object at 0x7fe4a388d470>, <truth_tables.Person object at 0x7fe4a388dda0>, <truth_tables.Person object at 0x7fe4a388db70>, <truth_tables.Person object at 0x7fe4a388da90>, <truth_tables.Person object at 0x7fe4a3891710>, <truth_tables.Person object at 0x7fe4a38913c8>, <truth_tables.Person object at 0x7fe4a38972e8>, <truth_tables.Person object at 0x7fe4a3897470>, <truth_tables.Person object at 0x7fe4a3897898>, <truth_tables.Person object at 0x7fe4a38976d8>, <truth_tables.Person object at 0x7fe4a389cf28>, <truth_tables.Person object at 0x7fe4a389cb70>, <truth_tables.Person object at 0x7fe4a382b860>, <truth_tables.Person object at 0x7fe4a382b470>, <truth_tables.Person object at 0x7fe4a3830908>, <truth_tables.Person object at 0x7fe4a382bf60>, <truth_tables.Person object at 0x7fe4a3830198>, <truth_tables.Person object at 0x7fe4a3830dd8>, <truth_tables.Person object at 0x7fe4a3830e80>, <truth_tables.Person object at 0x7fe4a3830f28>, <truth_tables.Person object at 0x7fe4a383b358>, <truth_tables.Person object at 0x7fe4a383bba8>, <truth_tables.Person object at 0x7fe4a3842668>, <truth_tables.Person object at 0x7fe4a383be48>, <truth_tables.Person object at 0x7fe4a383bf98>, <truth_tables.Person object at 0x7fe4a3846588>, <truth_tables.Person object at 0x7fe4a3842128>, <truth_tables.Person object at 0x7fe4a3846cc0>, <truth_tables.Person object at 0x7fe4a384b6a0>, <truth_tables.Person object at 0x7fe4a384bf28>, <truth_tables.Person object at 0x7fe4a3850978>, <truth_tables.Person object at 0x7fe4a3856ba8>, <truth_tables.Person object at 0x7fe4a385ea58>, <truth_tables.Person object at 0x7fe4a37e26d8>, <truth_tables.Person object at 0x7fe4a37e7080>, <truth_tables.Person object at 0x7fe4a37e7400>, <truth_tables.Person object at 0x7fe4a37e7be0>, <truth_tables.Person object at 0x7fe4a37e7e80>, <truth_tables.Person object at 0x7fe4a37ed0f0>, <truth_tables.Person object at 0x7fe4a37ed5f8>, <truth_tables.Person object at 0x7fe4a37edc18>, <truth_tables.Person object at 0x7fe4a37edeb8>, <truth_tables.Person object at 0x7fe4a37f3400>, <truth_tables.Person object at 0x7fe4a37f3748>, <truth_tables.Person object at 0x7fe4a37f39e8>, <truth_tables.Person object at 0x7fe4a37f3cc0>, <truth_tables.Person object at 0x7fe4a37fa668>, <truth_tables.Person object at 0x7fe4a37f3eb8>, <truth_tables.Person object at 0x7fe4a37fe048>, <truth_tables.Person object at 0x7fe4a37fecf8>, <truth_tables.Person object at 0x7fe4a3803278>, <truth_tables.Person object at 0x7fe4a37fea20>, <truth_tables.Person object at 0x7fe4a38030f0>, <truth_tables.Person object at 0x7fe4a380d080>, <truth_tables.Person object at 0x7fe4a380d940>, <truth_tables.Person object at 0x7fe4a3813470>, <truth_tables.Person object at 0x7fe4a3813a90>, <truth_tables.Person object at 0x7fe4a3818160>, <truth_tables.Person object at 0x7fe4a38189b0>, <truth_tables.Person object at 0x7fe4a381e4e0>, <truth_tables.Person object at 0x7fe4a381e630>, <truth_tables.Person object at 0x7fe4a37a3128>, <truth_tables.Person object at 0x7fe4a37a3f60>, <truth_tables.Person object at 0x7fe4a37a82b0>, <truth_tables.Person object at 0x7fe4a37ae4a8>, <truth_tables.Person object at 0x7fe4a37aecf8>, <truth_tables.Person object at 0x7fe4a37b2518>, <truth_tables.Person object at 0x7fe4a37b2e10>, <truth_tables.Person object at 0x7fe4a37b81d0>, <truth_tables.Person object at 0x7fe4a37b8588>, <truth_tables.Person object at 0x7fe4a37b8898>, <truth_tables.Person object at 0x7fe4a37bd080>, <truth_tables.Person object at 0x7fe4a37b8dd8>, <truth_tables.Person object at 0x7fe4a37bd8d0>, <truth_tables.Person object at 0x7fe4a37bdba8>, <truth_tables.Person object at 0x7fe4a37c9550>, <truth_tables.Person object at 0x7fe4a37c9ba8>, <truth_tables.Person object at 0x7fe4a37d0588>, <truth_tables.Person object at 0x7fe4a37d0710>, <truth_tables.Person object at 0x7fe4a37d0978>, <truth_tables.Person object at 0x7fe4a37da128>, <truth_tables.Person object at 0x7fe4a37da8d0>, <truth_tables.Person object at 0x7fe4a37daeb8>, <truth_tables.Person object at 0x7fe4a37daef0>, <truth_tables.Person object at 0x7fe4a37df780>, <truth_tables.Person object at 0x7fe4a37dfa90>, <truth_tables.Person object at 0x7fe4a37655c0>, <truth_tables.Person object at 0x7fe4a3765860>, <truth_tables.Person object at 0x7fe4a37dffd0>, <truth_tables.Person object at 0x7fe4a3765da0>, <truth_tables.Person object at 0x7fe4a376b390>, <truth_tables.Person object at 0x7fe4a376b400>, <truth_tables.Person object at 0x7fe4a3770748>, <truth_tables.Person object at 0x7fe4a3770a20>, <truth_tables.Person object at 0x7fe4a37754a8>, <truth_tables.Person object at 0x7fe4a3775cf8>, <truth_tables.Person object at 0x7fe4a377b898>, <truth_tables.Person object at 0x7fe4a377b400>, <truth_tables.Person object at 0x7fe4a3781748>, <truth_tables.Person object at 0x7fe4a3781fd0>, <truth_tables.Person object at 0x7fe4a3787358>, <truth_tables.Person object at 0x7fe4a3787e10>, <truth_tables.Person object at 0x7fe4a378b940>, <truth_tables.Person object at 0x7fe4a378be80>, <truth_tables.Person object at 0x7fe4a37950f0>, <truth_tables.Person object at 0x7fe4a3795390>, <truth_tables.Person object at 0x7fe4a379c7b8>, <truth_tables.Person object at 0x7fe4a37232e8>, <truth_tables.Person object at 0x7fe4a37239b0>, <truth_tables.Person object at 0x7fe4a3727e10>, <truth_tables.Person object at 0x7fe4a372d0f0>, <truth_tables.Person object at 0x7fe4a3733b00>, <truth_tables.Person object at 0x7fe4a3733f98>, <truth_tables.Person object at 0x7fe4a373e4a8>, <truth_tables.Person object at 0x7fe4a373ec88>, <truth_tables.Person object at 0x7fe4a3737f60>, <truth_tables.Person object at 0x7fe4a3744710>, <truth_tables.Person object at 0x7fe4a3744978>, <truth_tables.Person object at 0x7fe4a373efd0>, <truth_tables.Person object at 0x7fe4a3749c88>, <truth_tables.Person object at 0x7fe4a3749978>, <truth_tables.Person object at 0x7fe4a37497f0>, <truth_tables.Person object at 0x7fe4a3753400>, <truth_tables.Person object at 0x7fe4a37530b8>, <truth_tables.Person object at 0x7fe4a37592e8>, <truth_tables.Person object at 0x7fe4a3759668>, <truth_tables.Person object at 0x7fe4a3759cc0>, <truth_tables.Person object at 0x7fe4a375fe48>, <truth_tables.Person object at 0x7fe4a36e5c88>, <truth_tables.Person object at 0x7fe4a36f0630>, <truth_tables.Person object at 0x7fe4a36f0898>, <truth_tables.Person object at 0x7fe4a36f5c50>, <truth_tables.Person object at 0x7fe4a36f53c8>, <truth_tables.Person object at 0x7fe4a36f5550>, <truth_tables.Person object at 0x7fe4a36fb278>, <truth_tables.Person object at 0x7fe4a36fb5c0>, <truth_tables.Person object at 0x7fe4a36fb828>, <truth_tables.Person object at 0x7fe4a3700630>, <truth_tables.Person object at 0x7fe4a3700b70>, <truth_tables.Person object at 0x7fe4a37065f8>, <truth_tables.Person object at 0x7fe4a3706da0>, <truth_tables.Person object at 0x7fe4a370c400>, <truth_tables.Person object at 0x7fe4a370cbe0>, <truth_tables.Person object at 0x7fe4a370ce10>, <truth_tables.Person object at 0x7fe4a3711828>, <truth_tables.Person object at 0x7fe4a370ce48>, <truth_tables.Person object at 0x7fe4a3717b38>, <truth_tables.Person object at 0x7fe4a3717eb8>, <truth_tables.Person object at 0x7fe4a371c240>, <truth_tables.Person object at 0x7fe4a371c390>, <truth_tables.Person object at 0x7fe4a371cba8>, <truth_tables.Person object at 0x7fe4a371ce48>, <truth_tables.Person object at 0x7fe4a36a2128>, <truth_tables.Person object at 0x7fe4a36a2438>, <truth_tables.Person object at 0x7fe4a36a2748>, <truth_tables.Person object at 0x7fe4a36a29b0>, <truth_tables.Person object at 0x7fe4a36a2c50>, <truth_tables.Person object at 0x7fe4a36a7e80>, <truth_tables.Person object at 0x7fe4a36ad668>, <truth_tables.Person object at 0x7fe4a36adb38>, <truth_tables.Person object at 0x7fe4a36ad7b8>, <truth_tables.Person object at 0x7fe4a36b3d30>, <truth_tables.Person object at 0x7fe4a36b8278>, <truth_tables.Person object at 0x7fe4a36bf0f0>, <truth_tables.Person object at 0x7fe4a36bf3c8>, <truth_tables.Person object at 0x7fe4a36bf6a0>, <truth_tables.Person object at 0x7fe4a36bff60>, <truth_tables.Person object at 0x7fe4a36c4438>, <truth_tables.Person object at 0x7fe4a36c4518>, <truth_tables.Person object at 0x7fe4a36ca748>, <truth_tables.Person object at 0x7fe4a36ca208>, <truth_tables.Person object at 0x7fe4a36cac50>, <truth_tables.Person object at 0x7fe4a36caf28>, <truth_tables.Person object at 0x7fe4a36d43c8>, <truth_tables.Person object at 0x7fe4a36d46a0>, <truth_tables.Person object at 0x7fe4a36d4978>, <truth_tables.Person object at 0x7fe4a36da2e8>, <truth_tables.Person object at 0x7fe4a3669438>, <truth_tables.Person object at 0x7fe4a36699e8>, <truth_tables.Person object at 0x7fe4a366e5c0>, <truth_tables.Person object at 0x7fe4a366e2e8>, <truth_tables.Person object at 0x7fe4a366e908>, <truth_tables.Person object at 0x7fe4a366eb70>, <truth_tables.Person object at 0x7fe4a3675b00>, <truth_tables.Person object at 0x7fe4a3675828>, <truth_tables.Person object at 0x7fe4a367a198>, <truth_tables.Person object at 0x7fe4a367a438>, <truth_tables.Person object at 0x7fe4a367f5c0>, <truth_tables.Person object at 0x7fe4a367a908>, <truth_tables.Person object at 0x7fe4a367fcc0>, <truth_tables.Person object at 0x7fe4a367f390>, <truth_tables.Person object at 0x7fe4a3684518>, <truth_tables.Person object at 0x7fe4a367a828>, <truth_tables.Person object at 0x7fe4a3684b38>, <truth_tables.Person object at 0x7fe4a3691518>, <truth_tables.Person object at 0x7fe4a3691240>, <truth_tables.Person object at 0x7fe4a3691cc0>, <truth_tables.Person object at 0x7fe4a3697898>, <truth_tables.Person object at 0x7fe4a369b630>, <truth_tables.Person object at 0x7fe4a36a0048>, <truth_tables.Person object at 0x7fe4a369bcf8>, <truth_tables.Person object at 0x7fe4a3628128>, <truth_tables.Person object at 0x7fe4a3628550>, <truth_tables.Person object at 0x7fe4a3628ba8>, <truth_tables.Person object at 0x7fe4a362e160>, <truth_tables.Person object at 0x7fe4a362e6d8>, <truth_tables.Person object at 0x7fe4a3633198>, <truth_tables.Person object at 0x7fe4a36332e8>, <truth_tables.Person object at 0x7fe4a3637278>, <truth_tables.Person object at 0x7fe4a363c0b8>, <truth_tables.Person object at 0x7fe4a363c908>, <truth_tables.Person object at 0x7fe4a363cac8>, <truth_tables.Person object at 0x7fe4a363ce80>, <truth_tables.Person object at 0x7fe4a36445c0>, <truth_tables.Person object at 0x7fe4a3644eb8>, <truth_tables.Person object at 0x7fe4a36493c8>, <truth_tables.Person object at 0x7fe4a364e5c0>, <truth_tables.Person object at 0x7fe4a364e9e8>, <truth_tables.Person object at 0x7fe4a3654748>, <truth_tables.Person object at 0x7fe4a3654978>, <truth_tables.Person object at 0x7fe4a364e898>, <truth_tables.Person object at 0x7fe4a36597f0>, <truth_tables.Person object at 0x7fe4a36600b8>, <truth_tables.Person object at 0x7fe4a3660b70>, <truth_tables.Person object at 0x7fe4a3660668>, <truth_tables.Person object at 0x7fe4a3660320>, <truth_tables.Person object at 0x7fe4a35e5a90>, <truth_tables.Person object at 0x7fe4a35ea240>, <truth_tables.Person object at 0x7fe4a35ea860>, <truth_tables.Person object at 0x7fe4a35ea6d8>, <truth_tables.Person object at 0x7fe4a35f0b00>, <truth_tables.Person object at 0x7fe4a35f0828>, <truth_tables.Person object at 0x7fe4a35f0b38>, <truth_tables.Person object at 0x7fe4a35f4710>, <truth_tables.Person object at 0x7fe4a35f4be0>, <truth_tables.Person object at 0x7fe4a35f49b0>, <truth_tables.Person object at 0x7fe4a3601780>, <truth_tables.Person object at 0x7fe4a3601400>, <truth_tables.Person object at 0x7fe4a35fcf98>, <truth_tables.Person object at 0x7fe4a3601b70>, <truth_tables.Person object at 0x7fe4a3601cc0>, <truth_tables.Person object at 0x7fe4a3607240>, <truth_tables.Person object at 0x7fe4a36073c8>, <truth_tables.Person object at 0x7fe4a3607e48>, <truth_tables.Person object at 0x7fe4a360c160>, <truth_tables.Person object at 0x7fe4a360c470>, <truth_tables.Person object at 0x7fe4a3611da0>, <truth_tables.Person object at 0x7fe4a361d390>, <truth_tables.Person object at 0x7fe4a361d4e0>, <truth_tables.Person object at 0x7fe4a361ddd8>, <truth_tables.Person object at 0x7fe4a35a2400>, <truth_tables.Person object at 0x7fe4a35a27b8>, <truth_tables.Person object at 0x7fe4a35a2668>, <truth_tables.Person object at 0x7fe4a35a7278>, <truth_tables.Person object at 0x7fe4a35a7400>, <truth_tables.Person object at 0x7fe4a35ad470>, <truth_tables.Person object at 0x7fe4a35b10f0>, <truth_tables.Person object at 0x7fe4a35b1860>, <truth_tables.Person object at 0x7fe4a35b1ac8>, <truth_tables.Person object at 0x7fe4a35b19b0>, <truth_tables.Person object at 0x7fe4a35b9940>, <truth_tables.Person object at 0x7fe4a35b9eb8>, <truth_tables.Person object at 0x7fe4a35b9b38>, <truth_tables.Person object at 0x7fe4a35c2908>, <truth_tables.Person object at 0x7fe4a35c2c18>, <truth_tables.Person object at 0x7fe4a35c26d8>, <truth_tables.Person object at 0x7fe4a35c2d68>, <truth_tables.Person object at 0x7fe4a35d0208>, <truth_tables.Person object at 0x7fe4a35d0f60>, <truth_tables.Person object at 0x7fe4a35d9160>, <truth_tables.Person object at 0x7fe4a35d4f60>, <truth_tables.Person object at 0x7fe4a35d9a20>, <truth_tables.Person object at 0x7fe4a35d9780>, <truth_tables.Person object at 0x7fe4a35d9f98>, <truth_tables.Person object at 0x7fe4a35def98>, <truth_tables.Person object at 0x7fe4a35639e8>, <truth_tables.Person object at 0x7fe4a3563cf8>, <truth_tables.Person object at 0x7fe4a356bb70>, <truth_tables.Person object at 0x7fe4a3575940>, <truth_tables.Person object at 0x7fe4a3575e10>, <truth_tables.Person object at 0x7fe4a357b160>, <truth_tables.Person object at 0x7fe4a357b630>, <truth_tables.Person object at 0x7fe4a357f080>, <truth_tables.Person object at 0x7fe4a357f668>, <truth_tables.Person object at 0x7fe4a357f7f0>, <truth_tables.Person object at 0x7fe4a358c0b8>, <truth_tables.Person object at 0x7fe4a3585cf8>, <truth_tables.Person object at 0x7fe4a358c3c8>, <truth_tables.Person object at 0x7fe4a3591a20>, <truth_tables.Person object at 0x7fe4a3597da0>, <truth_tables.Person object at 0x7fe4a3591fd0>, <truth_tables.Person object at 0x7fe4a359d128>, <truth_tables.Person object at 0x7fe4a3521160>, <truth_tables.Person object at 0x7fe4a3521630>, <truth_tables.Person object at 0x7fe4a3521c88>, <truth_tables.Person object at 0x7fe4a3527748>, <truth_tables.Person object at 0x7fe4a3527eb8>, <truth_tables.Person object at 0x7fe4a352c8d0>, <truth_tables.Person object at 0x7fe4a3533208>, <truth_tables.Person object at 0x7fe4a3533710>, <truth_tables.Person object at 0x7fe4a3533978>, <truth_tables.Person object at 0x7fe4a353e0b8>, <truth_tables.Person object at 0x7fe4a353e908>, <truth_tables.Person object at 0x7fe4a353eb38>, <truth_tables.Person object at 0x7fe4a3543828>, <truth_tables.Person object at 0x7fe4a3543b38>, <truth_tables.Person object at 0x7fe4a3547588>, <truth_tables.Person object at 0x7fe4a3547c50>, <truth_tables.Person object at 0x7fe4a3547ac8>, <truth_tables.Person object at 0x7fe4a354cbe0>, <truth_tables.Person object at 0x7fe4a354ccf8>, <truth_tables.Person object at 0x7fe4a35525f8>, <truth_tables.Person object at 0x7fe4a3552940>, <truth_tables.Person object at 0x7fe4a3552c88>, <truth_tables.Person object at 0x7fe4a35562b0>, <truth_tables.Person object at 0x7fe4a3556e80>, <truth_tables.Person object at 0x7fe4a355d400>, <truth_tables.Person object at 0x7fe4a3556f98>, <truth_tables.Person object at 0x7fe4a34e34a8>, <truth_tables.Person object at 0x7fe4a34e3a20>, <truth_tables.Person object at 0x7fe4a34e3780>, <truth_tables.Person object at 0x7fe4a34e3f60>, <truth_tables.Person object at 0x7fe4a34e3f98>, <truth_tables.Person object at 0x7fe4a34e3fd0>, <truth_tables.Person object at 0x7fe4a34e8a58>, <truth_tables.Person object at 0x7fe4a34f0da0>, <truth_tables.Person object at 0x7fe4a34f4d30>, <truth_tables.Person object at 0x7fe4a34f4f98>, <truth_tables.Person object at 0x7fe4a34f4d68>, <truth_tables.Person object at 0x7fe4a34f97b8>, <truth_tables.Person object at 0x7fe4a34f9e80>, <truth_tables.Person object at 0x7fe4a3504f28>, <truth_tables.Person object at 0x7fe4a350beb8>, <truth_tables.Person object at 0x7fe4a35103c8>, <truth_tables.Person object at 0x7fe4a35166a0>, <truth_tables.Person object at 0x7fe4a3516780>, <truth_tables.Person object at 0x7fe4a35169b0>, <truth_tables.Person object at 0x7fe4a3516e10>, <truth_tables.Person object at 0x7fe4a35203c8>, <truth_tables.Person object at 0x7fe4a351cfd0>, <truth_tables.Person object at 0x7fe4a35208d0>, <truth_tables.Person object at 0x7fe4a34a74e0>, <truth_tables.Person object at 0x7fe4a34a7240>, <truth_tables.Person object at 0x7fe4a34a7898>, <truth_tables.Person object at 0x7fe4a34a7da0>, <truth_tables.Person object at 0x7fe4a34a79b0>, <truth_tables.Person object at 0x7fe4a34b12b0>, <truth_tables.Person object at 0x7fe4a34b1ba8>, <truth_tables.Person object at 0x7fe4a34b1908>, <truth_tables.Person object at 0x7fe4a34b1e10>, <truth_tables.Person object at 0x7fe4a34b6198>, <truth_tables.Person object at 0x7fe4a34b6940>, <truth_tables.Person object at 0x7fe4a34b6d68>, <truth_tables.Person object at 0x7fe4a34b6710>, <truth_tables.Person object at 0x7fe4a34bb320>, <truth_tables.Person object at 0x7fe4a34bbdd8>, <truth_tables.Person object at 0x7fe4a34c2ba8>, <truth_tables.Person object at 0x7fe4a34c6080>, <truth_tables.Person object at 0x7fe4a34c2908>, <truth_tables.Person object at 0x7fe4a34cc518>, <truth_tables.Person object at 0x7fe4a34d1400>, <truth_tables.Person object at 0x7fe4a34d1a58>, <truth_tables.Person object at 0x7fe4a34d67f0>, <truth_tables.Person object at 0x7fe4a34d1d30>, <truth_tables.Person object at 0x7fe4a34d6d68>, <truth_tables.Person object at 0x7fe4a34dd2b0>, <truth_tables.Person object at 0x7fe4a34d69b0>, <truth_tables.Person object at 0x7fe4a34ddac8>, <truth_tables.Person object at 0x7fe4a34dd3c8>, <truth_tables.Person object at 0x7fe4a3463668>, <truth_tables.Person object at 0x7fe4a3463c88>, <truth_tables.Person object at 0x7fe4a3463ef0>, <truth_tables.Person object at 0x7fe4a3468198>, <truth_tables.Person object at 0x7fe4a3463fd0>, <truth_tables.Person object at 0x7fe4a3468eb8>, <truth_tables.Person object at 0x7fe4a346d908>, <truth_tables.Person object at 0x7fe4a3471908>, <truth_tables.Person object at 0x7fe4a3471fd0>, <truth_tables.Person object at 0x7fe4a3477f60>, <truth_tables.Person object at 0x7fe4a3477588>, <truth_tables.Person object at 0x7fe4a3477c88>, <truth_tables.Person object at 0x7fe4a347f780>, <truth_tables.Person object at 0x7fe4a347f9b0>, <truth_tables.Person object at 0x7fe4a347fbe0>, <truth_tables.Person object at 0x7fe4a3489470>, <truth_tables.Person object at 0x7fe4a3489a58>, <truth_tables.Person object at 0x7fe4a3489f98>, <truth_tables.Person object at 0x7fe4a348e278>, <truth_tables.Person object at 0x7fe4a3494f28>, <truth_tables.Person object at 0x7fe4a34982e8>, <truth_tables.Person object at 0x7fe4a3498cf8>, <truth_tables.Person object at 0x7fe4a349fe48>, <truth_tables.Person object at 0x7fe4a3424908>, <truth_tables.Person object at 0x7fe4a3424ef0>, <truth_tables.Person object at 0x7fe4a3424c18>, <truth_tables.Person object at 0x7fe4a3428fd0>, <truth_tables.Person object at 0x7fe4a342f5f8>, <truth_tables.Person object at 0x7fe4a342fc88>, <truth_tables.Person object at 0x7fe4a3434898>, <truth_tables.Person object at 0x7fe4a3434e80>, <truth_tables.Person object at 0x7fe4a3434710>, <truth_tables.Person object at 0x7fe4a343aeb8>, <truth_tables.Person object at 0x7fe4a343a550>, <truth_tables.Person object at 0x7fe4a3440f98>, <truth_tables.Person object at 0x7fe4a34448d0>, <truth_tables.Person object at 0x7fe4a3440588>, <truth_tables.Person object at 0x7fe4a3444ef0>, <truth_tables.Person object at 0x7fe4a344bc18>, <truth_tables.Person object at 0x7fe4a344f4e0>, <truth_tables.Person object at 0x7fe4a344fc50>, <truth_tables.Person object at 0x7fe4a344fda0>, <truth_tables.Person object at 0x7fe4a345b9b0>, <truth_tables.Person object at 0x7fe4a345bda0>, <truth_tables.Person object at 0x7fe4a345f2b0>, <truth_tables.Person object at 0x7fe4a33e70f0>, <truth_tables.Person object at 0x7fe4a33e7278>, <truth_tables.Person object at 0x7fe4a33e75f8>, <truth_tables.Person object at 0x7fe4a33ecac8>, <truth_tables.Person object at 0x7fe4a33ecfd0>, <truth_tables.Person object at 0x7fe4a33f8278>, <truth_tables.Person object at 0x7fe4a33f87f0>, <truth_tables.Person object at 0x7fe4a33fd470>, <truth_tables.Person object at 0x7fe4a33fd710>, <truth_tables.Person object at 0x7fe4a33fdf28>, <truth_tables.Person object at 0x7fe4a3403390>, <truth_tables.Person object at 0x7fe4a34038d0>, <truth_tables.Person object at 0x7fe4a340e128>, <truth_tables.Person object at 0x7fe4a340e860>, <truth_tables.Person object at 0x7fe4a340eba8>, <truth_tables.Person object at 0x7fe4a34130f0>, <truth_tables.Person object at 0x7fe4a340ee48>, <truth_tables.Person object at 0x7fe4a3417588>, <truth_tables.Person object at 0x7fe4a3417828>, <truth_tables.Person object at 0x7fe4a3417c50>, <truth_tables.Person object at 0x7fe4a33a40f0>, <truth_tables.Person object at 0x7fe4a341f6a0>, <truth_tables.Person object at 0x7fe4a341fb70>, <truth_tables.Person object at 0x7fe4a33a4e10>, <truth_tables.Person object at 0x7fe4a33a8ac8>, <truth_tables.Person object at 0x7fe4a33a8c50>, <truth_tables.Person object at 0x7fe4a33ad080>, <truth_tables.Person object at 0x7fe4a33ad2e8>, <truth_tables.Person object at 0x7fe4a33ad358>, <truth_tables.Person object at 0x7fe4a33adeb8>, <truth_tables.Person object at 0x7fe4a33b9c50>, <truth_tables.Person object at 0x7fe4a33b9eb8>, <truth_tables.Person object at 0x7fe4a33b9c88>, <truth_tables.Person object at 0x7fe4a33bdc50>, <truth_tables.Person object at 0x7fe4a33c3748>, <truth_tables.Person object at 0x7fe4a33c3d68>, <truth_tables.Person object at 0x7fe4a33c3be0>, <truth_tables.Person object at 0x7fe4a33cd588>, <truth_tables.Person object at 0x7fe4a33c9908>, <truth_tables.Person object at 0x7fe4a33cdd68>, <truth_tables.Person object at 0x7fe4a33d5710>, <truth_tables.Person object at 0x7fe4a33d8940>, <truth_tables.Person object at 0x7fe4a33de7b8>, <truth_tables.Person object at 0x7fe4a33deb00>, <truth_tables.Person object at 0x7fe4a33dee48>, <truth_tables.Person object at 0x7fe4a3365278>, <truth_tables.Person object at 0x7fe4a3365ef0>, <truth_tables.Person object at 0x7fe4a336a5c0>, <truth_tables.Person object at 0x7fe4a3365fd0>, <truth_tables.Person object at 0x7fe4a336ae10>, <truth_tables.Person object at 0x7fe4a3370b00>, <truth_tables.Person object at 0x7fe4a3375dd8>, <truth_tables.Person object at 0x7fe4a3380860>, <truth_tables.Person object at 0x7fe4a337bd68>, <truth_tables.Person object at 0x7fe4a33854a8>, <truth_tables.Person object at 0x7fe4a3385780>, <truth_tables.Person object at 0x7fe4a3385e10>, <truth_tables.Person object at 0x7fe4a338c438>, <truth_tables.Person object at 0x7fe4a338c898>, <truth_tables.Person object at 0x7fe4a338c9b0>, <truth_tables.Person object at 0x7fe4a33914a8>, <truth_tables.Person object at 0x7fe4a3391780>, <truth_tables.Person object at 0x7fe4a33959e8>, <truth_tables.Person object at 0x7fe4a339a550>, <truth_tables.Person object at 0x7fe4a339f2b0>, <truth_tables.Person object at 0x7fe4a339f400>, <truth_tables.Person object at 0x7fe4a339fe48>, <truth_tables.Person object at 0x7fe4a3327198>, <truth_tables.Person object at 0x7fe4a332d6a0>, <truth_tables.Person object at 0x7fe4a3327940>, <truth_tables.Person object at 0x7fe4a332db70>, <truth_tables.Person object at 0x7fe4a33315c0>, <truth_tables.Person object at 0x7fe4a3331cc0>, <truth_tables.Person object at 0x7fe4a3331e48>, <truth_tables.Person object at 0x7fe4a3336d30>, <truth_tables.Person object at 0x7fe4a333a828>, <truth_tables.Person object at 0x7fe4a333a3c8>, <truth_tables.Person object at 0x7fe4a33416a0>, <truth_tables.Person object at 0x7fe4a3346160>, <truth_tables.Person object at 0x7fe4a3341d30>, <truth_tables.Person object at 0x7fe4a3346ac8>, <truth_tables.Person object at 0x7fe4a3341e80>, <truth_tables.Person object at 0x7fe4a334b400>, <truth_tables.Person object at 0x7fe4a3351160>, <truth_tables.Person object at 0x7fe4a3351940>, <truth_tables.Person object at 0x7fe4a3351e10>, <truth_tables.Person object at 0x7fe4a335d470>, <truth_tables.Person object at 0x7fe4a335d9e8>, <truth_tables.Person object at 0x7fe4a335df60>, <truth_tables.Person object at 0x7fe4a32e23c8>, <truth_tables.Person object at 0x7fe4a32e2be0>, <truth_tables.Person object at 0x7fe4a32ec588>, <truth_tables.Person object at 0x7fe4a32ecfd0>, <truth_tables.Person object at 0x7fe4a32ef4e0>, <truth_tables.Person object at 0x7fe4a32ef710>, <truth_tables.Person object at 0x7fe4a32efd30>, <truth_tables.Person object at 0x7fe4a32efb38>, <truth_tables.Person object at 0x7fe4a32f9470>, <truth_tables.Person object at 0x7fe4a32f9c18>, <truth_tables.Person object at 0x7fe4a32f9da0>, <truth_tables.Person object at 0x7fe4a32fe588>, <truth_tables.Person object at 0x7fe4a32fe940>, <truth_tables.Person object at 0x7fe4a3302e80>, <truth_tables.Person object at 0x7fe4a33081d0>, <truth_tables.Person object at 0x7fe4a3308e80>, <truth_tables.Person object at 0x7fe4a330c320>, <truth_tables.Person object at 0x7fe4a3312828>, <truth_tables.Person object at 0x7fe4a3312978>, <truth_tables.Person object at 0x7fe4a3312dd8>, <truth_tables.Person object at 0x7fe4a33172e8>, <truth_tables.Person object at 0x7fe4a331c1d0>, <truth_tables.Person object at 0x7fe4a331c5f8>, <truth_tables.Person object at 0x7fe4a331cb38>, <truth_tables.Person object at 0x7fe4a331cdd8>, <truth_tables.Person object at 0x7fe4a32a2588>, <truth_tables.Person object at 0x7fe4a32a2c18>, <truth_tables.Person object at 0x7fe4a32a8c50>, <truth_tables.Person object at 0x7fe4a32ae8d0>, <truth_tables.Person object at 0x7fe4a32aee48>, <truth_tables.Person object at 0x7fe4a32b4b38>, <truth_tables.Person object at 0x7fe4a32b4828>, <truth_tables.Person object at 0x7fe4a32b9e48>, <truth_tables.Person object at 0x7fe4a32bd9b0>, <truth_tables.Person object at 0x7fe4a32bde48>, <truth_tables.Person object at 0x7fe4a32c3d68>, <truth_tables.Person object at 0x7fe4a32c9ba8>, <truth_tables.Person object at 0x7fe4a32d07b8>, <truth_tables.Person object at 0x7fe4a32c9da0>, <truth_tables.Person object at 0x7fe4a32d0e10>, <truth_tables.Person object at 0x7fe4a32d4630>, <truth_tables.Person object at 0x7fe4a32d4f28>, <truth_tables.Person object at 0x7fe4a32da470>, <truth_tables.Person object at 0x7fe4a32da6d8>, <truth_tables.Person object at 0x7fe4a32dac18>, <truth_tables.Person object at 0x7fe4a32daef0>, <truth_tables.Person object at 0x7fe4a3262ac8>, <truth_tables.Person object at 0x7fe4a3262dd8>, <truth_tables.Person object at 0x7fe4a3262ba8>, <truth_tables.Person object at 0x7fe4a3267ac8>, <truth_tables.Person object at 0x7fe4a3267e48>, <truth_tables.Person object at 0x7fe4a326c0f0>, <truth_tables.Person object at 0x7fe4a326c668>, <truth_tables.Person object at 0x7fe4a326ce48>, <truth_tables.Person object at 0x7fe4a3270390>, <truth_tables.Person object at 0x7fe4a32753c8>, <truth_tables.Person object at 0x7fe4a3275668>, <truth_tables.Person object at 0x7fe4a327d940>, <truth_tables.Person object at 0x7fe4a327def0>, <truth_tables.Person object at 0x7fe4a3283278>, <truth_tables.Person object at 0x7fe4a3283780>, <truth_tables.Person object at 0x7fe4a3283ef0>, <truth_tables.Person object at 0x7fe4a3287470>, <truth_tables.Person object at 0x7fe4a3287cc0>, <truth_tables.Person object at 0x7fe4a328c320>, <truth_tables.Person object at 0x7fe4a328c828>, <truth_tables.Person object at 0x7fe4a328cb38>, <truth_tables.Person object at 0x7fe4a3290128>, <truth_tables.Person object at 0x7fe4a3298080>, <truth_tables.Person object at 0x7fe4a3298390>, <truth_tables.Person object at 0x7fe4a3298668>, <truth_tables.Person object at 0x7fe4a3298e48>, <truth_tables.Person object at 0x7fe4a3298908>, <truth_tables.Person object at 0x7fe4a3298b70>, <truth_tables.Person object at 0x7fe4a329dd30>, <truth_tables.Person object at 0x7fe4a329dc18>, <truth_tables.Person object at 0x7fe4a32292e8>, <truth_tables.Person object at 0x7fe4a3229860>, <truth_tables.Person object at 0x7fe4a3229588>, <truth_tables.Person object at 0x7fe4a3229da0>, <truth_tables.Person object at 0x7fe4a322c320>, <truth_tables.Person object at 0x7fe4a3234048>, <truth_tables.Person object at 0x7fe4a32346d8>, <truth_tables.Person object at 0x7fe4a3234f60>, <truth_tables.Person object at 0x7fe4a3239780>, <truth_tables.Person object at 0x7fe4a323e9e8>, <truth_tables.Person object at 0x7fe4a323e828>, <truth_tables.Person object at 0x7fe4a324be48>, <truth_tables.Person object at 0x7fe4a32512b0>, <truth_tables.Person object at 0x7fe4a32518d0>, <truth_tables.Person object at 0x7fe4a3251b70>, <truth_tables.Person object at 0x7fe4a3255160>, <truth_tables.Person object at 0x7fe4a3255438>, <truth_tables.Person object at 0x7fe4a3255828>, <truth_tables.Person object at 0x7fe4a325a9e8>, <truth_tables.Person object at 0x7fe4a325f240>, <truth_tables.Person object at 0x7fe4a325ae48>, <truth_tables.Person object at 0x7fe4a31e3828>, <truth_tables.Person object at 0x7fe4a31e3cf8>, <truth_tables.Person object at 0x7fe4a31e3be0>, <truth_tables.Person object at 0x7fe4a31ea0b8>, <truth_tables.Person object at 0x7fe4a31ee160>, <truth_tables.Person object at 0x7fe4a31ee4a8>, <truth_tables.Person object at 0x7fe4a31ee9b0>, <truth_tables.Person object at 0x7fe4a31f5438>, <truth_tables.Person object at 0x7fe4a31f5908>, <truth_tables.Person object at 0x7fe4a31f56a0>, <truth_tables.Person object at 0x7fe4a31f9a20>, <truth_tables.Person object at 0x7fe4a31f97b8>, <truth_tables.Person object at 0x7fe4a31fe0f0>, <truth_tables.Person object at 0x7fe4a31feb38>, <truth_tables.Person object at 0x7fe4a31fed30>, <truth_tables.Person object at 0x7fe4a31fe390>, <truth_tables.Person object at 0x7fe4a31fecf8>, <truth_tables.Person object at 0x7fe4a3206dd8>, <truth_tables.Person object at 0x7fe4a320b2b0>, <truth_tables.Person object at 0x7fe4a320bc50>, <truth_tables.Person object at 0x7fe4a3210550>, <truth_tables.Person object at 0x7fe4a3210748>, <truth_tables.Person object at 0x7fe4a3210ef0>, <truth_tables.Person object at 0x7fe4a3210e80>, <truth_tables.Person object at 0x7fe4a31a10b8>, <truth_tables.Person object at 0x7fe4a321bcf8>, <truth_tables.Person object at 0x7fe4a321be10>, <truth_tables.Person object at 0x7fe4a31a1898>, <truth_tables.Person object at 0x7fe4a31a1f60>, <truth_tables.Person object at 0x7fe4a31a9278>, <truth_tables.Person object at 0x7fe4a31a9940>, <truth_tables.Person object at 0x7fe4a31ac1d0>, <truth_tables.Person object at 0x7fe4a31ac5f8>, <truth_tables.Person object at 0x7fe4a31ac860>, <truth_tables.Person object at 0x7fe4a31b16d8>, <truth_tables.Person object at 0x7fe4a31b6748>, <truth_tables.Person object at 0x7fe4a31bc8d0>, <truth_tables.Person object at 0x7fe4a31c50f0>, <truth_tables.Person object at 0x7fe4a31c5588>, <truth_tables.Person object at 0x7fe4a31bccf8>, <truth_tables.Person object at 0x7fe4a31bccc0>, <truth_tables.Person object at 0x7fe4a31c9908>, <truth_tables.Person object at 0x7fe4a31ce860>, <truth_tables.Person object at 0x7fe4a31d3438>, <truth_tables.Person object at 0x7fe4a31d3978>, <truth_tables.Person object at 0x7fe4a31d3e80>, <truth_tables.Person object at 0x7fe4a31d9588>, <truth_tables.Person object at 0x7fe4a31d9fd0>, <truth_tables.Person object at 0x7fe4a3163f60>, <truth_tables.Person object at 0x7fe4a3163b38>, <truth_tables.Person object at 0x7fe4a31687b8>, <truth_tables.Person object at 0x7fe4a3168cf8>, <truth_tables.Person object at 0x7fe4a31741d0>, <truth_tables.Person object at 0x7fe4a3174438>, <truth_tables.Person object at 0x7fe4a3174be0>, <truth_tables.Person object at 0x7fe4a3174e80>, <truth_tables.Person object at 0x7fe4a3174fd0>, <truth_tables.Person object at 0x7fe4a3180128>, <truth_tables.Person object at 0x7fe4a31808d0>, <truth_tables.Person object at 0x7fe4a3180b70>, <truth_tables.Person object at 0x7fe4a3185320>, <truth_tables.Person object at 0x7fe4a3180e80>, <truth_tables.Person object at 0x7fe4a3185ac8>, <truth_tables.Person object at 0x7fe4a3185dd8>, <truth_tables.Person object at 0x7fe4a318ae80>, <truth_tables.Person object at 0x7fe4a3190390>, <truth_tables.Person object at 0x7fe4a3195a58>, <truth_tables.Person object at 0x7fe4a3195d30>, <truth_tables.Person object at 0x7fe4a3195fd0>, <truth_tables.Person object at 0x7fe4a319b278>, <truth_tables.Person object at 0x7fe4a319b908>, <truth_tables.Person object at 0x7fe4a319f4a8>, <truth_tables.Person object at 0x7fe4a319fbe0>, <truth_tables.Person object at 0x7fe4a3124390>, <truth_tables.Person object at 0x7fe4a3124d30>, <truth_tables.Person object at 0x7fe4a312ccf8>, <truth_tables.Person object at 0x7fe4a312ce80>, <truth_tables.Person object at 0x7fe4a3132940>, <truth_tables.Person object at 0x7fe4a3132b38>, <truth_tables.Person object at 0x7fe4a3137fd0>, <truth_tables.Person object at 0x7fe4a3137a90>, <truth_tables.Person object at 0x7fe4a3137cf8>, <truth_tables.Person object at 0x7fe4a313b5c0>, <truth_tables.Person object at 0x7fe4a313b898>, <truth_tables.Person object at 0x7fe4a313bcf8>, <truth_tables.Person object at 0x7fe4a31428d0>, <truth_tables.Person object at 0x7fe4a314da58>, <truth_tables.Person object at 0x7fe4a3148da0>, <truth_tables.Person object at 0x7fe4a3152710>, <truth_tables.Person object at 0x7fe4a3158668>, <truth_tables.Person object at 0x7fe4a30e4518>, <truth_tables.Person object at 0x7fe4a30e4940>, <truth_tables.Person object at 0x7fe4a30e4cc0>, <truth_tables.Person object at 0x7fe4a30e4f28>, <truth_tables.Person object at 0x7fe4a30e9dd8>, <truth_tables.Person object at 0x7fe4a30ee940>, <truth_tables.Person object at 0x7fe4a30eef28>, <truth_tables.Person object at 0x7fe4a30f9160>, <truth_tables.Person object at 0x7fe4a31054e0>, <truth_tables.Person object at 0x7fe4a3109048>, <truth_tables.Person object at 0x7fe4a3109f28>, <truth_tables.Person object at 0x7fe4a310f588>, <truth_tables.Person object at 0x7fe4a3109c18>, <truth_tables.Person object at 0x7fe4a310ff28>, <truth_tables.Person object at 0x7fe4a3119eb8>, <truth_tables.Person object at 0x7fe4a311f470>, <truth_tables.Person object at 0x7fe4a311f940>, <truth_tables.Person object at 0x7fe4a30a42b0>, <truth_tables.Person object at 0x7fe4a30a4710>, <truth_tables.Person object at 0x7fe4a30b1470>, <truth_tables.Person object at 0x7fe4a30b1ef0>, <truth_tables.Person object at 0x7fe4a30b1978>, <truth_tables.Person object at 0x7fe4a30b1710>, <truth_tables.Person object at 0x7fe4a30b7160>, <truth_tables.Person object at 0x7fe4a30b7908>, <truth_tables.Person object at 0x7fe4a30b7ba8>, <truth_tables.Person object at 0x7fe4a30bc438>, <truth_tables.Person object at 0x7fe4a30bc9b0>, <truth_tables.Person object at 0x7fe4a30c1080>, <truth_tables.Person object at 0x7fe4a30bceb8>, <truth_tables.Person object at 0x7fe4a30c13c8>, <truth_tables.Person object at 0x7fe4a30c1da0>, <truth_tables.Person object at 0x7fe4a30c1ac8>, <truth_tables.Person object at 0x7fe4a30c55c0>, <truth_tables.Person object at 0x7fe4a30c57b8>, <truth_tables.Person object at 0x7fe4a30d1f28>, <truth_tables.Person object at 0x7fe4a30d7e10>, <truth_tables.Person object at 0x7fe4a30dc8d0>, <truth_tables.Person object at 0x7fe4a30dcb38>, <truth_tables.Person object at 0x7fe4a3061240>, <truth_tables.Person object at 0x7fe4a30610f0>, <truth_tables.Person object at 0x7fe4a3061c18>, <truth_tables.Person object at 0x7fe4a30673c8>, <truth_tables.Person object at 0x7fe4a3061fd0>, <truth_tables.Person object at 0x7fe4a306c2b0>, <truth_tables.Person object at 0x7fe4a306cef0>, <truth_tables.Person object at 0x7fe4a306cfd0>, <truth_tables.Person object at 0x7fe4a3070208>, <truth_tables.Person object at 0x7fe4a3076390>, <truth_tables.Person object at 0x7fe4a3076fd0>, <truth_tables.Person object at 0x7fe4a3076e80>, <truth_tables.Person object at 0x7fe4a307bc18>, <truth_tables.Person object at 0x7fe4a30818d0>, <truth_tables.Person object at 0x7fe4a3081ba8>, <truth_tables.Person object at 0x7fe4a3081940>, <truth_tables.Person object at 0x7fe4a3087160>, <truth_tables.Person object at 0x7fe4a3087320>, <truth_tables.Person object at 0x7fe4a308b9b0>, <truth_tables.Person object at 0x7fe4a308bac8>, <truth_tables.Person object at 0x7fe4a30914a8>, <truth_tables.Person object at 0x7fe4a3091940>, <truth_tables.Person object at 0x7fe4a3091e80>, <truth_tables.Person object at 0x7fe4a3097ac8>, <truth_tables.Person object at 0x7fe4a3097940>, <truth_tables.Person object at 0x7fe4a309ce48>, <truth_tables.Person object at 0x7fe4a30234a8>, <truth_tables.Person object at 0x7fe4a309ccc0>, <truth_tables.Person object at 0x7fe4a3023a20>, <truth_tables.Person object at 0x7fe4a3023cc0>, <truth_tables.Person object at 0x7fe4a3028358>, <truth_tables.Person object at 0x7fe4a3028c88>, <truth_tables.Person object at 0x7fe4a302e0f0>, <truth_tables.Person object at 0x7fe4a3028f28>, <truth_tables.Person object at 0x7fe4a3028dd8>, <truth_tables.Person object at 0x7fe4a3032208>, <truth_tables.Person object at 0x7fe4a3032940>, <truth_tables.Person object at 0x7fe4a30327f0>, <truth_tables.Person object at 0x7fe4a30396a0>, <truth_tables.Person object at 0x7fe4a3039518>, <truth_tables.Person object at 0x7fe4a3039e10>, <truth_tables.Person object at 0x7fe4a303e320>, <truth_tables.Person object at 0x7fe4a303ed30>, <truth_tables.Person object at 0x7fe4a303ef60>, <truth_tables.Person object at 0x7fe4a3043518>, <truth_tables.Person object at 0x7fe4a3043be0>, <truth_tables.Person object at 0x7fe4a30481d0>, <truth_tables.Person object at 0x7fe4a3048470>, <truth_tables.Person object at 0x7fe4a3048dd8>, <truth_tables.Person object at 0x7fe4a304d438>, <truth_tables.Person object at 0x7fe4a304df98>, <truth_tables.Person object at 0x7fe4a304dda0>, <truth_tables.Person object at 0x7fe4a3059400>, <truth_tables.Person object at 0x7fe4a305d5c0>, <truth_tables.Person object at 0x7fe4a2fe3390>, <truth_tables.Person object at 0x7fe4a2fe36a0>, <truth_tables.Person object at 0x7fe4a2fe3dd8>, <truth_tables.Person object at 0x7fe4a2fe8860>, <truth_tables.Person object at 0x7fe4a2ff39e8>, <truth_tables.Person object at 0x7fe4a2ff8438>, <truth_tables.Person object at 0x7fe4a2fff978>, <truth_tables.Person object at 0x7fe4a2fff2b0>, <truth_tables.Person object at 0x7fe4a2ffff28>, <truth_tables.Person object at 0x7fe4a30047f0>, <truth_tables.Person object at 0x7fe4a3004860>, <truth_tables.Person object at 0x7fe4a300fb38>, <truth_tables.Person object at 0x7fe4a300ff98>, <truth_tables.Person object at 0x7fe4a3013a58>, <truth_tables.Person object at 0x7fe4a3013e48>, <truth_tables.Person object at 0x7fe4a301ad30>, <truth_tables.Person object at 0x7fe4a301f9e8>, <truth_tables.Person object at 0x7fe4a301feb8>, <truth_tables.Person object at 0x7fe4a2fa63c8>, <truth_tables.Person object at 0x7fe4a2fa6198>, <truth_tables.Person object at 0x7fe4a2fab240>, <truth_tables.Person object at 0x7fe4a2fab860>, <truth_tables.Person object at 0x7fe4a2fb05f8>, <truth_tables.Person object at 0x7fe4a2fabba8>, <truth_tables.Person object at 0x7fe4a2fb0c18>, <truth_tables.Person object at 0x7fe4a2fbaf28>, <truth_tables.Person object at 0x7fe4a2fc35c0>, <truth_tables.Person object at 0x7fe4a2fcc7f0>, <truth_tables.Person object at 0x7fe4a2fd1198>, <truth_tables.Person object at 0x7fe4a2fd6470>, <truth_tables.Person object at 0x7fe4a2fdcb38>, <truth_tables.Person object at 0x7fe4a2fdcf98>, <truth_tables.Person object at 0x7fe4a2fe0d68>, <truth_tables.Person object at 0x7fe4a2f66b00>, <truth_tables.Person object at 0x7fe4a2f6b4e0>, <truth_tables.Person object at 0x7fe4a2f700b8>, <truth_tables.Person object at 0x7fe4a2f707b8>, <truth_tables.Person object at 0x7fe4a2f70c88>, <truth_tables.Person object at 0x7fe4a2f76908>, <truth_tables.Person object at 0x7fe4a2f76f98>, <truth_tables.Person object at 0x7fe4a2f7d4e0>, <truth_tables.Person object at 0x7fe4a2f7db00>, <truth_tables.Person object at 0x7fe4a2f80eb8>, <truth_tables.Person object at 0x7fe4a2f85ac8>, <truth_tables.Person object at 0x7fe4a2f85898>, <truth_tables.Person object at 0x7fe4a2f85668>, <truth_tables.Person object at 0x7fe4a2f8b898>, <truth_tables.Person object at 0x7fe4a2f91ba8>, <truth_tables.Person object at 0x7fe4a2f95f98>, <truth_tables.Person object at 0x7fe4a2f9c588>, <truth_tables.Person object at 0x7fe4a2f9c9e8>, <truth_tables.Person object at 0x7fe4a2f9ce10>, <truth_tables.Person object at 0x7fe4a2fa05f8>, <truth_tables.Person object at 0x7fe4a2fa09e8>, <truth_tables.Person object at 0x7fe4a2f25240>, <truth_tables.Person object at 0x7fe4a2f253c8>, <truth_tables.Person object at 0x7fe4a2f25ac8>, <truth_tables.Person object at 0x7fe4a2f25588>, <truth_tables.Person object at 0x7fe4a2f25fd0>, <truth_tables.Person object at 0x7fe4a2f2d278>, <truth_tables.Person object at 0x7fe4a2f2d320>, <truth_tables.Person object at 0x7fe4a2f2dd30>, <truth_tables.Person object at 0x7fe4a2f3f400>, <truth_tables.Person object at 0x7fe4a2f43f28>, <truth_tables.Person object at 0x7fe4a2f4a4a8>, <truth_tables.Person object at 0x7fe4a2f4a9e8>, <truth_tables.Person object at 0x7fe4a2f4ea58>, <truth_tables.Person object at 0x7fe4a2f53160>, <truth_tables.Person object at 0x7fe4a2f59278>, <truth_tables.Person object at 0x7fe4a2f539b0>, <truth_tables.Person object at 0x7fe4a2f5d8d0>, <truth_tables.Person object at 0x7fe4a2f5da20>, <truth_tables.Person object at 0x7fe4a2ee38d0>, <truth_tables.Person object at 0x7fe4a2ee3d30>, <truth_tables.Person object at 0x7fe4a2ee9668>, <truth_tables.Person object at 0x7fe4a2ee98d0>, <truth_tables.Person object at 0x7fe4a2ee9e48>, <truth_tables.Person object at 0x7fe4a2ee9f98>, <truth_tables.Person object at 0x7fe4a2eee7b8>, <truth_tables.Person object at 0x7fe4a2eeee48>, <truth_tables.Person object at 0x7fe4a2ef36a0>, <truth_tables.Person object at 0x7fe4a2eeee80>, <truth_tables.Person object at 0x7fe4a2eeecf8>, <truth_tables.Person object at 0x7fe4a2ef35f8>, <truth_tables.Person object at 0x7fe4a2efa2e8>, <truth_tables.Person object at 0x7fe4a2efa5c0>, <truth_tables.Person object at 0x7fe4a2efa748>, <truth_tables.Person object at 0x7fe4a2eff320>, <truth_tables.Person object at 0x7fe4a2effb38>, <truth_tables.Person object at 0x7fe4a2effdd8>, <truth_tables.Person object at 0x7fe4a2effeb8>, <truth_tables.Person object at 0x7fe4a2f05c50>, <truth_tables.Person object at 0x7fe4a2f05cf8>, <truth_tables.Person object at 0x7fe4a2f0f550>, <truth_tables.Person object at 0x7fe4a2f15908>, <truth_tables.Person object at 0x7fe4a2f0fc88>, <truth_tables.Person object at 0x7fe4a2f1ab38>, <truth_tables.Person object at 0x7fe4a2f1a828>, <truth_tables.Person object at 0x7fe4a2f1af28>, <truth_tables.Person object at 0x7fe4a2ea1710>, <truth_tables.Person object at 0x7fe4a2ea1a20>, <truth_tables.Person object at 0x7fe4a2ea6128>, <truth_tables.Person object at 0x7fe4a2ea6940>, <truth_tables.Person object at 0x7fe4a2eab048>, <truth_tables.Person object at 0x7fe4a2eab358>, <truth_tables.Person object at 0x7fe4a2eab630>, <truth_tables.Person object at 0x7fe4a2eabe48>, <truth_tables.Person object at 0x7fe4a2eabba8>, <truth_tables.Person object at 0x7fe4a2eb11d0>, <truth_tables.Person object at 0x7fe4a2eb1780>, <truth_tables.Person object at 0x7fe4a2eb1c88>, <truth_tables.Person object at 0x7fe4a2eb7358>, <truth_tables.Person object at 0x7fe4a2eb77b8>, <truth_tables.Person object at 0x7fe4a2eb7b70>, <truth_tables.Person object at 0x7fe4a2ebc080>, <truth_tables.Person object at 0x7fe4a2ebc588>, <truth_tables.Person object at 0x7fe4a2ebc320>, <truth_tables.Person object at 0x7fe4a2ebcdd8>, <truth_tables.Person object at 0x7fe4a2ec3240>, <truth_tables.Person object at 0x7fe4a2ec73c8>, <truth_tables.Person object at 0x7fe4a2ec7dd8>, <truth_tables.Person object at 0x7fe4a2ecb080>, <truth_tables.Person object at 0x7fe4a2ecb358>, <truth_tables.Person object at 0x7fe4a2ecbb38>, <truth_tables.Person object at 0x7fe4a2ecbf60>, <truth_tables.Person object at 0x7fe4a2ed32b0>, <truth_tables.Person object at 0x7fe4a2ed3b70>, <truth_tables.Person object at 0x7fe4a2ed7898>, <truth_tables.Person object at 0x7fe4a2edd1d0>, <truth_tables.Person object at 0x7fe4a2edd438>, <truth_tables.Person object at 0x7fe4a2edd908>, <truth_tables.Person object at 0x7fe4a2e63f60>, <truth_tables.Person object at 0x7fe4a2e69208>, <truth_tables.Person object at 0x7fe4a2e69470>, <truth_tables.Person object at 0x7fe4a2e696a0>, <truth_tables.Person object at 0x7fe4a2e6f780>, <truth_tables.Person object at 0x7fe4a2e6f128>, <truth_tables.Person object at 0x7fe4a2e6f278>, <truth_tables.Person object at 0x7fe4a2e73748>, <truth_tables.Person object at 0x7fe4a2e73a58>, <truth_tables.Person object at 0x7fe4a2e73ba8>, <truth_tables.Person object at 0x7fe4a2e79668>, <truth_tables.Person object at 0x7fe4a2e79ba8>, <truth_tables.Person object at 0x7fe4a2e79f60>, <truth_tables.Person object at 0x7fe4a2e7f630>, <truth_tables.Person object at 0x7fe4a2e7f978>, <truth_tables.Person object at 0x7fe4a2e84048>, <truth_tables.Person object at 0x7fe4a2e7fe80>, <truth_tables.Person object at 0x7fe4a2e84a58>, <truth_tables.Person object at 0x7fe4a2e89978>, <truth_tables.Person object at 0x7fe4a2e8e940>, <truth_tables.Person object at 0x7fe4a2e8ecf8>, <truth_tables.Person object at 0x7fe4a2e934e0>, <truth_tables.Person object at 0x7fe4a2e93978>, <truth_tables.Person object at 0x7fe4a2e93fd0>, <truth_tables.Person object at 0x7fe4a2e93be0>, <truth_tables.Person object at 0x7fe4a2e981d0>, <truth_tables.Person object at 0x7fe4a2e988d0>, <truth_tables.Person object at 0x7fe4a2e98a20>, <truth_tables.Person object at 0x7fe4a2e98be0>, <truth_tables.Person object at 0x7fe4a2e9dfd0>, <truth_tables.Person object at 0x7fe4a2e2a9e8>, <truth_tables.Person object at 0x7fe4a2e2ae48>, <truth_tables.Person object at 0x7fe4a2e30630>, <truth_tables.Person object at 0x7fe4a2e30a58>, <truth_tables.Person object at 0x7fe4a2e35eb8>, <truth_tables.Person object at 0x7fe4a2e39358>, <truth_tables.Person object at 0x7fe4a2e395c0>, <truth_tables.Person object at 0x7fe4a2e400f0>, <truth_tables.Person object at 0x7fe4a2e40630>, <truth_tables.Person object at 0x7fe4a2e40278>, <truth_tables.Person object at 0x7fe4a2e40ac8>, <truth_tables.Person object at 0x7fe4a2e49c18>, <truth_tables.Person object at 0x7fe4a2e50b70>, <truth_tables.Person object at 0x7fe4a2e50e48>, <truth_tables.Person object at 0x7fe4a2e56198>, <truth_tables.Person object at 0x7fe4a2e568d0>, <truth_tables.Person object at 0x7fe4a2e56a58>, <truth_tables.Person object at 0x7fe4a2e56c50>, <truth_tables.Person object at 0x7fe4a2e56d68>, <truth_tables.Person object at 0x7fe4a2e60048>, <truth_tables.Person object at 0x7fe4a2de59e8>, <truth_tables.Person object at 0x7fe4a2e60eb8>, <truth_tables.Person object at 0x7fe4a2de5d68>, <truth_tables.Person object at 0x7fe4a2deaa20>, <truth_tables.Person object at 0x7fe4a2deae10>, <truth_tables.Person object at 0x7fe4a2deacc0>, <truth_tables.Person object at 0x7fe4a2deaf98>, <truth_tables.Person object at 0x7fe4a2df14a8>, <truth_tables.Person object at 0x7fe4a2df7588>, <truth_tables.Person object at 0x7fe4a2df7d30>, <truth_tables.Person object at 0x7fe4a2dfb160>, <truth_tables.Person object at 0x7fe4a2e00470>, <truth_tables.Person object at 0x7fe4a2e00a20>, <truth_tables.Person object at 0x7fe4a2e00b00>, <truth_tables.Person object at 0x7fe4a2e046d8>, <truth_tables.Person object at 0x7fe4a2e11cc0>, <truth_tables.Person object at 0x7fe4a2e11898>, <truth_tables.Person object at 0x7fe4a2e164e0>, <truth_tables.Person object at 0x7fe4a2e1c5f8>, <truth_tables.Person object at 0x7fe4a2e1cb00>, <truth_tables.Person object at 0x7fe4a2da12b0>, <truth_tables.Person object at 0x7fe4a2da17b8>, <truth_tables.Person object at 0x7fe4a2da1c50>, <truth_tables.Person object at 0x7fe4a2da1ef0>, <truth_tables.Person object at 0x7fe4a2dad400>, <truth_tables.Person object at 0x7fe4a2dade48>, <truth_tables.Person object at 0x7fe4a2db7048>, <truth_tables.Person object at 0x7fe4a2db7748>, <truth_tables.Person object at 0x7fe4a2dbe668>, <truth_tables.Person object at 0x7fe4a2db7c88>, <truth_tables.Person object at 0x7fe4a2dbeac8>, <truth_tables.Person object at 0x7fe4a2dbe978>, <truth_tables.Person object at 0x7fe4a2dbe5c0>, <truth_tables.Person object at 0x7fe4a2dc96a0>, <truth_tables.Person object at 0x7fe4a2dc9be0>, <truth_tables.Person object at 0x7fe4a2dd4048>, <truth_tables.Person object at 0x7fe4a2dd4320>, <truth_tables.Person object at 0x7fe4a2dd4898>, <truth_tables.Person object at 0x7fe4a2dd4fd0>, <truth_tables.Person object at 0x7fe4a2dd9470>, <truth_tables.Person object at 0x7fe4a2dd9e48>, <truth_tables.Person object at 0x7fe4a2ddf3c8>, <truth_tables.Person object at 0x7fe4a2d6aa20>, <truth_tables.Person object at 0x7fe4a2d6f6a0>, <truth_tables.Person object at 0x7fe4a2d6fba8>, <truth_tables.Person object at 0x7fe4a2d74860>, <truth_tables.Person object at 0x7fe4a2d74128>, <truth_tables.Person object at 0x7fe4a2d6f978>, <truth_tables.Person object at 0x7fe4a2d7f400>, <truth_tables.Person object at 0x7fe4a2d7be10>, <truth_tables.Person object at 0x7fe4a2d7f9e8>, <truth_tables.Person object at 0x7fe4a2d7f780>, <truth_tables.Person object at 0x7fe4a2d7bcf8>, <truth_tables.Person object at 0x7fe4a2d8a080>, <truth_tables.Person object at 0x7fe4a2d86d30>, <truth_tables.Person object at 0x7fe4a2d8aa90>, <truth_tables.Person object at 0x7fe4a2d907b8>, <truth_tables.Person object at 0x7fe4a2d97908>, <truth_tables.Person object at 0x7fe4a2d9c5c0>, <truth_tables.Person object at 0x7fe4a2d220b8>, <truth_tables.Person object at 0x7fe4a2d227f0>, <truth_tables.Person object at 0x7fe4a2d260f0>, <truth_tables.Person object at 0x7fe4a2d266d8>, <truth_tables.Person object at 0x7fe4a2d26a20>, <truth_tables.Person object at 0x7fe4a2d26f98>, <truth_tables.Person object at 0x7fe4a2d2d278>, <truth_tables.Person object at 0x7fe4a2d26c88>, <truth_tables.Person object at 0x7fe4a2d2d160>, <truth_tables.Person object at 0x7fe4a2d26fd0>, <truth_tables.Person object at 0x7fe4a2d33780>, <truth_tables.Person object at 0x7fe4a2d38240>, <truth_tables.Person object at 0x7fe4a2d38b00>, <truth_tables.Person object at 0x7fe4a2d33f98>, <truth_tables.Person object at 0x7fe4a2d3d940>, <truth_tables.Person object at 0x7fe4a2d42668>, <truth_tables.Person object at 0x7fe4a2d427f0>, <truth_tables.Person object at 0x7fe4a2d42c18>, <truth_tables.Person object at 0x7fe4a2d42f98>, <truth_tables.Person object at 0x7fe4a2d47da0>, <truth_tables.Person object at 0x7fe4a2d4d080>, <truth_tables.Person object at 0x7fe4a2d52390>, <truth_tables.Person object at 0x7fe4a2d582b0>, <truth_tables.Person object at 0x7fe4a2d588d0>, <truth_tables.Person object at 0x7fe4a2d58fd0>, <truth_tables.Person object at 0x7fe4a2d58cf8>, <truth_tables.Person object at 0x7fe4a2d58b38>, <truth_tables.Person object at 0x7fe4a2d5eeb8>, <truth_tables.Person object at 0x7fe4a2d58cc0>, <truth_tables.Person object at 0x7fe4a2ce4780>, <truth_tables.Person object at 0x7fe4a2ce4e10>, <truth_tables.Person object at 0x7fe4a2ce97f0>, <truth_tables.Person object at 0x7fe4a2ced390>, <truth_tables.Person object at 0x7fe4a2ce9400>, <truth_tables.Person object at 0x7fe4a2cf37f0>, <truth_tables.Person object at 0x7fe4a2cf8320>, <truth_tables.Person object at 0x7fe4a2cf8748>, <truth_tables.Person object at 0x7fe4a2cf8668>, <truth_tables.Person object at 0x7fe4a2cffba8>, <truth_tables.Person object at 0x7fe4a2cffe80>, <truth_tables.Person object at 0x7fe4a2d042b0>, <truth_tables.Person object at 0x7fe4a2d040b8>, <truth_tables.Person object at 0x7fe4a2d09198>, <truth_tables.Person object at 0x7fe4a2d09438>, <truth_tables.Person object at 0x7fe4a2d09be0>, <truth_tables.Person object at 0x7fe4a2d09fd0>, <truth_tables.Person object at 0x7fe4a2d0f0f0>, <truth_tables.Person object at 0x7fe4a2d0fda0>, <truth_tables.Person object at 0x7fe4a2d1c2b0>, <truth_tables.Person object at 0x7fe4a2d1ccf8>, <truth_tables.Person object at 0x7fe4a2d1cfd0>, <truth_tables.Person object at 0x7fe4a2ca1550>, <truth_tables.Person object at 0x7fe4a2ca7a90>, <truth_tables.Person object at 0x7fe4a2ca1438>, <truth_tables.Person object at 0x7fe4a2ca1f98>, <truth_tables.Person object at 0x7fe4a2ca1e10>, <truth_tables.Person object at 0x7fe4a2ca77b8>, <truth_tables.Person object at 0x7fe4a2cab5f8>, <truth_tables.Person object at 0x7fe4a2cabb38>, <truth_tables.Person object at 0x7fe4a2cab898>, <truth_tables.Person object at 0x7fe4a2cb2160>, <truth_tables.Person object at 0x7fe4a2cb2978>, <truth_tables.Person object at 0x7fe4a2cb7cf8>, <truth_tables.Person object at 0x7fe4a2cbcf60>, <truth_tables.Person object at 0x7fe4a2cc2438>, <truth_tables.Person object at 0x7fe4a2cc6668>, <truth_tables.Person object at 0x7fe4a2cc6e48>, <truth_tables.Person object at 0x7fe4a2ccc3c8>, <truth_tables.Person object at 0x7fe4a2cc6e80>, <truth_tables.Person object at 0x7fe4a2cd2a20>, <truth_tables.Person object at 0x7fe4a2cd2b70>, <truth_tables.Person object at 0x7fe4a2cd6b00>, <truth_tables.Person object at 0x7fe4a2cd6400>, <truth_tables.Person object at 0x7fe4a2cdc2e8>, <truth_tables.Person object at 0x7fe4a2cdc588>, <truth_tables.Person object at 0x7fe4a2c647f0>, <truth_tables.Person object at 0x7fe4a2c68080>, <truth_tables.Person object at 0x7fe4a2c68b00>, <truth_tables.Person object at 0x7fe4a2c68630>, <truth_tables.Person object at 0x7fe4a2c73400>, <truth_tables.Person object at 0x7fe4a2c73940>, <truth_tables.Person object at 0x7fe4a2c79ba8>, <truth_tables.Person object at 0x7fe4a2c7fc50>, <truth_tables.Person object at 0x7fe4a2c7f6d8>, <truth_tables.Person object at 0x7fe4a2c836a0>, <truth_tables.Person object at 0x7fe4a2c8b048>, <truth_tables.Person object at 0x7fe4a2c83d68>, <truth_tables.Person object at 0x7fe4a2c8b438>, <truth_tables.Person object at 0x7fe4a2c8b898>, <truth_tables.Person object at 0x7fe4a2c8f630>, <truth_tables.Person object at 0x7fe4a2c8f748>, <truth_tables.Person object at 0x7fe4a2c8fef0>, <truth_tables.Person object at 0x7fe4a2c94c18>, <truth_tables.Person object at 0x7fe4a2c9b2b0>, <truth_tables.Person object at 0x7fe4a2c9b550>, <truth_tables.Person object at 0x7fe4a2c9ba20>, <truth_tables.Person object at 0x7fe4a2c9bcc0>, <truth_tables.Person object at 0x7fe4a2c9f2b0>, <truth_tables.Person object at 0x7fe4a2c9f438>, <truth_tables.Person object at 0x7fe4a2c9fcc0>, <truth_tables.Person object at 0x7fe4a2c27668>, <truth_tables.Person object at 0x7fe4a2c27e80>, <truth_tables.Person object at 0x7fe4a2c2b048>, <truth_tables.Person object at 0x7fe4a2c2b588>, <truth_tables.Person object at 0x7fe4a2c30198>, <truth_tables.Person object at 0x7fe4a2c30710>, <truth_tables.Person object at 0x7fe4a2c30a58>, <truth_tables.Person object at 0x7fe4a2c30ba8>, <truth_tables.Person object at 0x7fe4a2c30fd0>, <truth_tables.Person object at 0x7fe4a2c3a5c0>, <truth_tables.Person object at 0x7fe4a2c3ab70>, <truth_tables.Person object at 0x7fe4a2c3ada0>, <truth_tables.Person object at 0x7fe4a2c407b8>, <truth_tables.Person object at 0x7fe4a2c40b00>, <truth_tables.Person object at 0x7fe4a2c45278>, <truth_tables.Person object at 0x7fe4a2c45a58>, <truth_tables.Person object at 0x7fe4a2c45ef0>, <truth_tables.Person object at 0x7fe4a2c4b550>, <truth_tables.Person object at 0x7fe4a2c4b3c8>, <truth_tables.Person object at 0x7fe4a2c4bc18>, <truth_tables.Person object at 0x7fe4a2c56668>, <truth_tables.Person object at 0x7fe4a2c56cc0>, <truth_tables.Person object at 0x7fe4a2c5c400>, <truth_tables.Person object at 0x7fe4a2c5cc18>, <truth_tables.Person object at 0x7fe4a2c5cda0>, <truth_tables.Person object at 0x7fe4a2c60588>, <truth_tables.Person object at 0x7fe4a2c60c50>, <truth_tables.Person object at 0x7fe4a2c60ef0>, <truth_tables.Person object at 0x7fe4a2c60f28>, <truth_tables.Person object at 0x7fe4a2be6588>, <truth_tables.Person object at 0x7fe4a2be66d8>, <truth_tables.Person object at 0x7fe4a2bea240>, <truth_tables.Person object at 0x7fe4a2bea4e0>, <truth_tables.Person object at 0x7fe4a2bea780>, <truth_tables.Person object at 0x7fe4a2bf0080>, <truth_tables.Person object at 0x7fe4a2bf0390>, <truth_tables.Person object at 0x7fe4a2bf05f8>, <truth_tables.Person object at 0x7fe4a2bf0b70>, <truth_tables.Person object at 0x7fe4a2bf50f0>, <truth_tables.Person object at 0x7fe4a2bf55f8>, <truth_tables.Person object at 0x7fe4a2bf5748>, <truth_tables.Person object at 0x7fe4a2c00208>, <truth_tables.Person object at 0x7fe4a2c005c0>, <truth_tables.Person object at 0x7fe4a2c063c8>, <truth_tables.Person object at 0x7fe4a2c06320>, <truth_tables.Person object at 0x7fe4a2c06908>, <truth_tables.Person object at 0x7fe4a2c06f98>, <truth_tables.Person object at 0x7fe4a2c0b048>, <truth_tables.Person object at 0x7fe4a2c122b0>, <truth_tables.Person object at 0x7fe4a2c12588>, <truth_tables.Person object at 0x7fe4a2c12668>, <truth_tables.Person object at 0x7fe4a2c12e80>, <truth_tables.Person object at 0x7fe4a2c1d358>, <truth_tables.Person object at 0x7fe4a2ba1080>, <truth_tables.Person object at 0x7fe4a2ba1518>, <truth_tables.Person object at 0x7fe4a2ba1780>, <truth_tables.Person object at 0x7fe4a2ba1a20>, <truth_tables.Person object at 0x7fe4a2ba1c88>, <truth_tables.Person object at 0x7fe4a2ba6b00>, <truth_tables.Person object at 0x7fe4a2bacf98>, <truth_tables.Person object at 0x7fe4a2bacb00>, <truth_tables.Person object at 0x7fe4a2bb1320>, <truth_tables.Person object at 0x7fe4a2bb9358>, <truth_tables.Person object at 0x7fe4a2bb9b38>, <truth_tables.Person object at 0x7fe4a2bbde80>, <truth_tables.Person object at 0x7fe4a2bc3390>, <truth_tables.Person object at 0x7fe4a2bc36a0>, <truth_tables.Person object at 0x7fe4a2bc3940>, <truth_tables.Person object at 0x7fe4a2bc7668>, <truth_tables.Person object at 0x7fe4a2bc73c8>, <truth_tables.Person object at 0x7fe4a2bc7978>, <truth_tables.Person object at 0x7fe4a2bc79b0>, <truth_tables.Person object at 0x7fe4a2bc7fd0>, <truth_tables.Person object at 0x7fe4a2bd36d8>, <truth_tables.Person object at 0x7fe4a2bc7c50>, <truth_tables.Person object at 0x7fe4a2bd3978>, <truth_tables.Person object at 0x7fe4a2bd3c18>, <truth_tables.Person object at 0x7fe4a2bd92b0>, <truth_tables.Person object at 0x7fe4a2bd9550>, <truth_tables.Person object at 0x7fe4a2bd9a58>, <truth_tables.Person object at 0x7fe4a2bd9940>, <truth_tables.Person object at 0x7fe4a2bdf2e8>, <truth_tables.Person object at 0x7fe4a2bdfe80>, <truth_tables.Person object at 0x7fe4a2bdf5c0>, <truth_tables.Person object at 0x7fe4a2b65e48>, <truth_tables.Person object at 0x7fe4a2b69208>, <truth_tables.Person object at 0x7fe4a2b65940>, <truth_tables.Person object at 0x7fe4a2b65b70>, <truth_tables.Person object at 0x7fe4a2b6f208>, <truth_tables.Person object at 0x7fe4a2b6f518>, <truth_tables.Person object at 0x7fe4a2b74048>, <truth_tables.Person object at 0x7fe4a2b74390>, <truth_tables.Person object at 0x7fe4a2b7ba20>, <truth_tables.Person object at 0x7fe4a2b819e8>, <truth_tables.Person object at 0x7fe4a2b7b7f0>, <truth_tables.Person object at 0x7fe4a2b87358>, <truth_tables.Person object at 0x7fe4a2b87d68>, <truth_tables.Person object at 0x7fe4a2b8bef0>, <truth_tables.Person object at 0x7fe4a2b975f8>, <truth_tables.Person object at 0x7fe4a2b97d30>, <truth_tables.Person object at 0x7fe4a2b9d278>, <truth_tables.Person object at 0x7fe4a2b9dfd0>, <truth_tables.Person object at 0x7fe4a2b9d7b8>, <truth_tables.Person object at 0x7fe4a2b22ac8>, <truth_tables.Person object at 0x7fe4a2b27908>, <truth_tables.Person object at 0x7fe4a2b2b9e8>, <truth_tables.Person object at 0x7fe4a2b2b780>, <truth_tables.Person object at 0x7fe4a2b31a58>, <truth_tables.Person object at 0x7fe4a2b37320>, <truth_tables.Person object at 0x7fe4a2b31da0>, <truth_tables.Person object at 0x7fe4a2b31d68>, <truth_tables.Person object at 0x7fe4a2b37ef0>, <truth_tables.Person object at 0x7fe4a2b37c50>, <truth_tables.Person object at 0x7fe4a2b43828>, <truth_tables.Person object at 0x7fe4a2b436d8>, <truth_tables.Person object at 0x7fe4a2b48240>, <truth_tables.Person object at 0x7fe4a2b487f0>, <truth_tables.Person object at 0x7fe4a2b4e630>, <truth_tables.Person object at 0x7fe4a2b54710>, <truth_tables.Person object at 0x7fe4a2b54a58>, <truth_tables.Person object at 0x7fe4a2b592b0>, <truth_tables.Person object at 0x7fe4a2b54d68>, <truth_tables.Person object at 0x7fe4a2b59cc0>, <truth_tables.Person object at 0x7fe4a2b59f28>, <truth_tables.Person object at 0x7fe4a2b5f550>, <truth_tables.Person object at 0x7fe4a2b5fd68>, <truth_tables.Person object at 0x7fe4a2ae9390>, <truth_tables.Person object at 0x7fe4a2ae9e48>, <truth_tables.Person object at 0x7fe4a2ae9780>, <truth_tables.Person object at 0x7fe4a2af34a8>, <truth_tables.Person object at 0x7fe4a2af3cc0>, <truth_tables.Person object at 0x7fe4a2af3e48>, <truth_tables.Person object at 0x7fe4a2af9748>, <truth_tables.Person object at 0x7fe4a2aff4e0>, <truth_tables.Person object at 0x7fe4a2aff240>, <truth_tables.Person object at 0x7fe4a2aff550>, <truth_tables.Person object at 0x7fe4a2b064e0>, <truth_tables.Person object at 0x7fe4a2b06630>, <truth_tables.Person object at 0x7fe4a2b06e80>, <truth_tables.Person object at 0x7fe4a2b0b668>, <truth_tables.Person object at 0x7fe4a2b06ef0>, <truth_tables.Person object at 0x7fe4a2b10240>, <truth_tables.Person object at 0x7fe4a2b0bf28>, <truth_tables.Person object at 0x7fe4a2b0bd30>, <truth_tables.Person object at 0x7fe4a2b15278>, <truth_tables.Person object at 0x7fe4a2b15390>, <truth_tables.Person object at 0x7fe4a2b15b70>, <truth_tables.Person object at 0x7fe4a2b1a0f0>, <truth_tables.Person object at 0x7fe4a2b1acc0>, <truth_tables.Person object at 0x7fe4a2b1afd0>, <truth_tables.Person object at 0x7fe4a2b20e48>, <truth_tables.Person object at 0x7fe4a2aab198>, <truth_tables.Person object at 0x7fe4a2ab01d0>, <truth_tables.Person object at 0x7fe4a2ab0358>, <truth_tables.Person object at 0x7fe4a2ab0b38>, <truth_tables.Person object at 0x7fe4a2ab0f60>, <truth_tables.Person object at 0x7fe4a2ab0d68>, <truth_tables.Person object at 0x7fe4a2ab4358>, <truth_tables.Person object at 0x7fe4a2ab06d8>, <truth_tables.Person object at 0x7fe4a2ab4d30>, <truth_tables.Person object at 0x7fe4a2abd9b0>, <truth_tables.Person object at 0x7fe4a2ac2d68>, <truth_tables.Person object at 0x7fe4a2ac6d30>, <truth_tables.Person object at 0x7fe4a2acba20>, <truth_tables.Person object at 0x7fe4a2ac6da0>, <truth_tables.Person object at 0x7fe4a2ac6eb8>, <truth_tables.Person object at 0x7fe4a2ad0588>, <truth_tables.Person object at 0x7fe4a2ad0860>, <truth_tables.Person object at 0x7fe4a2ad7748>, <truth_tables.Person object at 0x7fe4a2ad7f60>, <truth_tables.Person object at 0x7fe4a2adc4a8>, <truth_tables.Person object at 0x7fe4a2a61128>, <truth_tables.Person object at 0x7fe4a2a67668>, <truth_tables.Person object at 0x7fe4a2a6b6d8>, <truth_tables.Person object at 0x7fe4a2a67c50>, <truth_tables.Person object at 0x7fe4a2a67f28>, <truth_tables.Person object at 0x7fe4a2a6b2e8>, <truth_tables.Person object at 0x7fe4a2a6bdd8>, <truth_tables.Person object at 0x7fe4a2a716a0>, <truth_tables.Person object at 0x7fe4a2a71d68>, <truth_tables.Person object at 0x7fe4a2a78ba8>, <truth_tables.Person object at 0x7fe4a2a7c358>, <truth_tables.Person object at 0x7fe4a2a816d8>, <truth_tables.Person object at 0x7fe4a2a7cfd0>, <truth_tables.Person object at 0x7fe4a2a88400>, <truth_tables.Person object at 0x7fe4a2a8ddd8>, <truth_tables.Person object at 0x7fe4a2a946d8>, <truth_tables.Person object at 0x7fe4a2a94b00>, <truth_tables.Person object at 0x7fe4a2a949b0>, <truth_tables.Person object at 0x7fe4a2a98710>, <truth_tables.Person object at 0x7fe4a2a9db00>, <truth_tables.Person object at 0x7fe4a2a9dd68>, <truth_tables.Person object at 0x7fe4a2a22668>, <truth_tables.Person object at 0x7fe4a2a9dd30>, <truth_tables.Person object at 0x7fe4a2a28160>, <truth_tables.Person object at 0x7fe4a2a22898>, <truth_tables.Person object at 0x7fe4a2a286d8>, <truth_tables.Person object at 0x7fe4a2a2eeb8>, <truth_tables.Person object at 0x7fe4a2a371d0>, <truth_tables.Person object at 0x7fe4a2a37550>, <truth_tables.Person object at 0x7fe4a2a37438>, <truth_tables.Person object at 0x7fe4a2a446d8>, <truth_tables.Person object at 0x7fe4a2a3da90>, <truth_tables.Person object at 0x7fe4a2a44208>, <truth_tables.Person object at 0x7fe4a2a3d6a0>, <truth_tables.Person object at 0x7fe4a2a3d9b0>, <truth_tables.Person object at 0x7fe4a2a4d1d0>, <truth_tables.Person object at 0x7fe4a2a56630>, <truth_tables.Person object at 0x7fe4a2a56e80>, <truth_tables.Person object at 0x7fe4a2a5ccc0>, <truth_tables.Person object at 0x7fe4a2a5cfd0>, <truth_tables.Person object at 0x7fe4a29e1a58>, <truth_tables.Person object at 0x7fe4a29e13c8>, <truth_tables.Person object at 0x7fe4a29e1390>, <truth_tables.Person object at 0x7fe4a29e64e0>, <truth_tables.Person object at 0x7fe4a29e6630>, <truth_tables.Person object at 0x7fe4a29ec9e8>, <truth_tables.Person object at 0x7fe4a29ecd30>, <truth_tables.Person object at 0x7fe4a29ecd68>, <truth_tables.Person object at 0x7fe4a29f1b00>, <truth_tables.Person object at 0x7fe4a29f6710>, <truth_tables.Person object at 0x7fe4a29f17b8>, <truth_tables.Person object at 0x7fe4a2a02048>, <truth_tables.Person object at 0x7fe4a29fcc88>, <truth_tables.Person object at 0x7fe4a2a07940>, <truth_tables.Person object at 0x7fe4a29fcb00>, <truth_tables.Person object at 0x7fe4a2a075f8>, <truth_tables.Person object at 0x7fe4a2a0c710>, <truth_tables.Person object at 0x7fe4a2a0cef0>, <truth_tables.Person object at 0x7fe4a2a136a0>, <truth_tables.Person object at 0x7fe4a2a18828>, <truth_tables.Person object at 0x7fe4a2a18550>, <truth_tables.Person object at 0x7fe4a2a1d710>, <truth_tables.Person object at 0x7fe4a29a31d0>, <truth_tables.Person object at 0x7fe4a29a3358>, <truth_tables.Person object at 0x7fe4a29a3f28>, <truth_tables.Person object at 0x7fe4a29a3a20>, <truth_tables.Person object at 0x7fe4a29a3c50>, <truth_tables.Person object at 0x7fe4a29a74a8>, <truth_tables.Person object at 0x7fe4a29a7dd8>, <truth_tables.Person object at 0x7fe4a29ad0f0>, <truth_tables.Person object at 0x7fe4a29ad3c8>, <truth_tables.Person object at 0x7fe4a29adac8>, <truth_tables.Person object at 0x7fe4a29adbe0>, <truth_tables.Person object at 0x7fe4a29b4128>, <truth_tables.Person object at 0x7fe4a29b46d8>, <truth_tables.Person object at 0x7fe4a29b9128>, <truth_tables.Person object at 0x7fe4a29b9550>, <truth_tables.Person object at 0x7fe4a29bf1d0>, <truth_tables.Person object at 0x7fe4a29bf978>, <truth_tables.Person object at 0x7fe4a29bfe80>, <truth_tables.Person object at 0x7fe4a29c3518>, <truth_tables.Person object at 0x7fe4a29c3a90>, <truth_tables.Person object at 0x7fe4a29c8518>, <truth_tables.Person object at 0x7fe4a29c3978>, <truth_tables.Person object at 0x7fe4a29c8470>, <truth_tables.Person object at 0x7fe4a29cf080>, <truth_tables.Person object at 0x7fe4a29cf828>, <truth_tables.Person object at 0x7fe4a29d42e8>, <truth_tables.Person object at 0x7fe4a29cff60>, <truth_tables.Person object at 0x7fe4a29cfd30>, <truth_tables.Person object at 0x7fe4a29d4a58>, <truth_tables.Person object at 0x7fe4a29d4e80>, <truth_tables.Person object at 0x7fe4a29d9ba8>, <truth_tables.Person object at 0x7fe4a29d9978>, <truth_tables.Person object at 0x7fe4a29dd6d8>, <truth_tables.Person object at 0x7fe4a29ddc50>, <truth_tables.Person object at 0x7fe4a29ddac8>, <truth_tables.Person object at 0x7fe4a29ddbe0>, <truth_tables.Person object at 0x7fe4a2963d30>, <truth_tables.Person object at 0x7fe4a29ddc18>, <truth_tables.Person object at 0x7fe4a2963dd8>, <truth_tables.Person object at 0x7fe4a296fcc0>, <truth_tables.Person object at 0x7fe4a29769b0>, <truth_tables.Person object at 0x7fe4a2976ef0>, <truth_tables.Person object at 0x7fe4a297acc0>, <truth_tables.Person object at 0x7fe4a2976dd8>, <truth_tables.Person object at 0x7fe4a297a9b0>, <truth_tables.Person object at 0x7fe4a297afd0>, <truth_tables.Person object at 0x7fe4a2981320>, <truth_tables.Person object at 0x7fe4a2981898>, <truth_tables.Person object at 0x7fe4a2981b70>, <truth_tables.Person object at 0x7fe4a2981f28>, <truth_tables.Person object at 0x7fe4a2987358>, <truth_tables.Person object at 0x7fe4a2987a58>, <truth_tables.Person object at 0x7fe4a298b630>, <truth_tables.Person object at 0x7fe4a2990160>, <truth_tables.Person object at 0x7fe4a2990da0>, <truth_tables.Person object at 0x7fe4a2990fd0>, <truth_tables.Person object at 0x7fe4a2995908>, <truth_tables.Person object at 0x7fe4a2995b38>, <truth_tables.Person object at 0x7fe4a2995cf8>, <truth_tables.Person object at 0x7fe4a299b710>, <truth_tables.Person object at 0x7fe4a299b4e0>, <truth_tables.Person object at 0x7fe4a29220b8>, <truth_tables.Person object at 0x7fe4a29225f8>, <truth_tables.Person object at 0x7fe4a2922470>, <truth_tables.Person object at 0x7fe4a2922ba8>, <truth_tables.Person object at 0x7fe4a2922cf8>, <truth_tables.Person object at 0x7fe4a29266d8>, <truth_tables.Person object at 0x7fe4a2926a20>, <truth_tables.Person object at 0x7fe4a292b748>, <truth_tables.Person object at 0x7fe4a292bef0>, <truth_tables.Person object at 0x7fe4a29310b8>, <truth_tables.Person object at 0x7fe4a2931860>, <truth_tables.Person object at 0x7fe4a29313c8>, <truth_tables.Person object at 0x7fe4a2931358>, <truth_tables.Person object at 0x7fe4a2931d30>, <truth_tables.Person object at 0x7fe4a2931fd0>, <truth_tables.Person object at 0x7fe4a2936c50>, <truth_tables.Person object at 0x7fe4a293d1d0>, <truth_tables.Person object at 0x7fe4a2936978>, <truth_tables.Person object at 0x7fe4a2936780>, <truth_tables.Person object at 0x7fe4a293d860>, <truth_tables.Person object at 0x7fe4a293dba8>, <truth_tables.Person object at 0x7fe4a29421d0>, <truth_tables.Person object at 0x7fe4a2942828>, <truth_tables.Person object at 0x7fe4a2946b70>, <truth_tables.Person object at 0x7fe4a294b4a8>, <truth_tables.Person object at 0x7fe4a29502b0>, <truth_tables.Person object at 0x7fe4a2950e48>, <truth_tables.Person object at 0x7fe4a2956390>, <truth_tables.Person object at 0x7fe4a2956d68>, <truth_tables.Person object at 0x7fe4a28e1b00>, <truth_tables.Person object at 0x7fe4a28e70f0>, <truth_tables.Person object at 0x7fe4a28e7400>, <truth_tables.Person object at 0x7fe4a28e76d8>, <truth_tables.Person object at 0x7fe4a28e7a20>, <truth_tables.Person object at 0x7fe4a28ec278>, <truth_tables.Person object at 0x7fe4a28ec908>, <truth_tables.Person object at 0x7fe4a28ece80>, <truth_tables.Person object at 0x7fe4a28f3ef0>, <truth_tables.Person object at 0x7fe4a28f3c50>, <truth_tables.Person object at 0x7fe4a28f7470>, <truth_tables.Person object at 0x7fe4a28f77f0>, <truth_tables.Person object at 0x7fe4a28fd320>, <truth_tables.Person object at 0x7fe4a28fdef0>, <truth_tables.Person object at 0x7fe4a28fdc50>, <truth_tables.Person object at 0x7fe4a28fda58>, <truth_tables.Person object at 0x7fe4a29026a0>, <truth_tables.Person object at 0x7fe4a2902e80>, <truth_tables.Person object at 0x7fe4a2902eb8>, <truth_tables.Person object at 0x7fe4a2902f98>, <truth_tables.Person object at 0x7fe4a29089b0>, <truth_tables.Person object at 0x7fe4a290e550>, <truth_tables.Person object at 0x7fe4a29124e0>, <truth_tables.Person object at 0x7fe4a2912d68>, <truth_tables.Person object at 0x7fe4a2918160>, <truth_tables.Person object at 0x7fe4a291e9e8>, <truth_tables.Person object at 0x7fe4a291ef28>, <truth_tables.Person object at 0x7fe4a28a8198>, <truth_tables.Person object at 0x7fe4a28a85c0>, <truth_tables.Person object at 0x7fe4a28a2c50>, <truth_tables.Person object at 0x7fe4a28a8c50>, <truth_tables.Person object at 0x7fe4a28ae128>, <truth_tables.Person object at 0x7fe4a28ae3c8>, <truth_tables.Person object at 0x7fe4a28ae828>, <truth_tables.Person object at 0x7fe4a28b46a0>, <truth_tables.Person object at 0x7fe4a28b4b00>, <truth_tables.Person object at 0x7fe4a28b8ac8>, <truth_tables.Person object at 0x7fe4a28bdc18>, <truth_tables.Person object at 0x7fe4a28bdeb8>, <truth_tables.Person object at 0x7fe4a28c4668>, <truth_tables.Person object at 0x7fe4a28c4dd8>, <truth_tables.Person object at 0x7fe4a28c4b00>, <truth_tables.Person object at 0x7fe4a28ca908>, <truth_tables.Person object at 0x7fe4a28d0240>, <truth_tables.Person object at 0x7fe4a28caef0>, <truth_tables.Person object at 0x7fe4a28d0748>, <truth_tables.Person object at 0x7fe4a28d4908>, <truth_tables.Person object at 0x7fe4a28d4be0>, <truth_tables.Person object at 0x7fe4a28d4eb8>, <truth_tables.Person object at 0x7fe4a28d9c88>, <truth_tables.Person object at 0x7fe4a28d9da0>, <truth_tables.Person object at 0x7fe4a28e00f0>, <truth_tables.Person object at 0x7fe4a28e0a20>, <truth_tables.Person object at 0x7fe4a28e0c50>, <truth_tables.Person object at 0x7fe4a28e0f28>, <truth_tables.Person object at 0x7fe4a2866fd0>, <truth_tables.Person object at 0x7fe4a286c470>, <truth_tables.Person object at 0x7fe4a2871128>, <truth_tables.Person object at 0x7fe4a2871e48>, <truth_tables.Person object at 0x7fe4a28773c8>, <truth_tables.Person object at 0x7fe4a287c7b8>, <truth_tables.Person object at 0x7fe4a287ca58>, <truth_tables.Person object at 0x7fe4a2881128>, <truth_tables.Person object at 0x7fe4a2881898>, <truth_tables.Person object at 0x7fe4a2886320>, <truth_tables.Person object at 0x7fe4a288d6a0>, <truth_tables.Person object at 0x7fe4a288d550>, <truth_tables.Person object at 0x7fe4a28926a0>, <truth_tables.Person object at 0x7fe4a2892f98>, <truth_tables.Person object at 0x7fe4a2898fd0>, <truth_tables.Person object at 0x7fe4a2898630>, <truth_tables.Person object at 0x7fe4a28214a8>, <truth_tables.Person object at 0x7fe4a2821898>, <truth_tables.Person object at 0x7fe4a2821cf8>, <truth_tables.Person object at 0x7fe4a2821fd0>, <truth_tables.Person object at 0x7fe4a2826320>, <truth_tables.Person object at 0x7fe4a2826550>, <truth_tables.Person object at 0x7fe4a28267f0>, <truth_tables.Person object at 0x7fe4a2826da0>, <truth_tables.Person object at 0x7fe4a282c198>, <truth_tables.Person object at 0x7fe4a282c940>, <truth_tables.Person object at 0x7fe4a282cbe0>, <truth_tables.Person object at 0x7fe4a282cf60>, <truth_tables.Person object at 0x7fe4a2834a20>, <truth_tables.Person object at 0x7fe4a2834ba8>, <truth_tables.Person object at 0x7fe4a2839400>, <truth_tables.Person object at 0x7fe4a283d320>, <truth_tables.Person object at 0x7fe4a2839eb8>, <truth_tables.Person object at 0x7fe4a283da58>, <truth_tables.Person object at 0x7fe4a2843978>, <truth_tables.Person object at 0x7fe4a28433c8>, <truth_tables.Person object at 0x7fe4a2843e48>, <truth_tables.Person object at 0x7fe4a2843be0>, <truth_tables.Person object at 0x7fe4a28434e0>, <truth_tables.Person object at 0x7fe4a284f0f0>, <truth_tables.Person object at 0x7fe4a284f518>, <truth_tables.Person object at 0x7fe4a28543c8>, <truth_tables.Person object at 0x7fe4a2858160>, <truth_tables.Person object at 0x7fe4a28584e0>, <truth_tables.Person object at 0x7fe4a28602b0>, <truth_tables.Person object at 0x7fe4a2860198>, <truth_tables.Person object at 0x7fe4a2860748>, <truth_tables.Person object at 0x7fe4a27ebdd8>, <truth_tables.Person object at 0x7fe4a27f06d8>, <truth_tables.Person object at 0x7fe4a27f5470>, <truth_tables.Person object at 0x7fe4a27f56d8>, <truth_tables.Person object at 0x7fe4a27f5f60>, <truth_tables.Person object at 0x7fe4a27fb710>, <truth_tables.Person object at 0x7fe4a27fba58>, <truth_tables.Person object at 0x7fe4a2800080>, <truth_tables.Person object at 0x7fe4a2800f28>, <truth_tables.Person object at 0x7fe4a28007b8>, <truth_tables.Person object at 0x7fe4a28050b8>, <truth_tables.Person object at 0x7fe4a2805c18>, <truth_tables.Person object at 0x7fe4a2805ac8>, <truth_tables.Person object at 0x7fe4a280f278>, <truth_tables.Person object at 0x7fe4a280a668>, <truth_tables.Person object at 0x7fe4a2815358>, <truth_tables.Person object at 0x7fe4a2815668>, <truth_tables.Person object at 0x7fe4a28157f0>, <truth_tables.Person object at 0x7fe4a28194e0>, <truth_tables.Person object at 0x7fe4a2819198>, <truth_tables.Person object at 0x7fe4a2820470>, <truth_tables.Person object at 0x7fe4a28206d8>, <truth_tables.Person object at 0x7fe4a27a4208>, <truth_tables.Person object at 0x7fe4a27a4518>, <truth_tables.Person object at 0x7fe4a27a4630>, <truth_tables.Person object at 0x7fe4a27aaa90>, <truth_tables.Person object at 0x7fe4a27af438>, <truth_tables.Person object at 0x7fe4a27afc88>, <truth_tables.Person object at 0x7fe4a27b55c0>, <truth_tables.Person object at 0x7fe4a27b5b00>, <truth_tables.Person object at 0x7fe4a27b5e48>, <truth_tables.Person object at 0x7fe4a27bcb00>, <truth_tables.Person object at 0x7fe4a27bf1d0>, <truth_tables.Person object at 0x7fe4a27bc208>, <truth_tables.Person object at 0x7fe4a27bf588>, <truth_tables.Person object at 0x7fe4a27c57f0>, <truth_tables.Person object at 0x7fe4a27c5eb8>, <truth_tables.Person object at 0x7fe4a27c5d68>, <truth_tables.Person object at 0x7fe4a27ca5c0>, <truth_tables.Person object at 0x7fe4a27cae48>, <truth_tables.Person object at 0x7fe4a27cfc50>, <truth_tables.Person object at 0x7fe4a27cad68>, <truth_tables.Person object at 0x7fe4a27db358>, <truth_tables.Person object at 0x7fe4a27db780>, <truth_tables.Person object at 0x7fe4a27dbba8>, <truth_tables.Person object at 0x7fe4a2762160>, <truth_tables.Person object at 0x7fe4a2762898>, <truth_tables.Person object at 0x7fe4a2762ef0>, <truth_tables.Person object at 0x7fe4a2762b38>, <truth_tables.Person object at 0x7fe4a27666a0>, <truth_tables.Person object at 0x7fe4a276c080>, <truth_tables.Person object at 0x7fe4a2766b38>, <truth_tables.Person object at 0x7fe4a276cbe0>, <truth_tables.Person object at 0x7fe4a27727b8>, <truth_tables.Person object at 0x7fe4a2772940>, <truth_tables.Person object at 0x7fe4a2772d30>, <truth_tables.Person object at 0x7fe4a2777a90>, <truth_tables.Person object at 0x7fe4a2777d30>, <truth_tables.Person object at 0x7fe4a277cf98>, <truth_tables.Person object at 0x7fe4a277cc18>, <truth_tables.Person object at 0x7fe4a277c7b8>, <truth_tables.Person object at 0x7fe4a27882e8>, <truth_tables.Person object at 0x7fe4a2788c88>, <truth_tables.Person object at 0x7fe4a2788f28>, <truth_tables.Person object at 0x7fe4a278d828>, <truth_tables.Person object at 0x7fe4a278d9b0>, <truth_tables.Person object at 0x7fe4a278dcc0>, <truth_tables.Person object at 0x7fe4a2790160>, <truth_tables.Person object at 0x7fe4a2790828>, <truth_tables.Person object at 0x7fe4a2790b38>, <truth_tables.Person object at 0x7fe4a2798128>, <truth_tables.Person object at 0x7fe4a2798b38>, <truth_tables.Person object at 0x7fe4a2798a58>, <truth_tables.Person object at 0x7fe4a279c710>, <truth_tables.Person object at 0x7fe4a27290b8>, <truth_tables.Person object at 0x7fe4a2729860>, <truth_tables.Person object at 0x7fe4a27332e8>, <truth_tables.Person object at 0x7fe4a272e320>, <truth_tables.Person object at 0x7fe4a2733668>, <truth_tables.Person object at 0x7fe4a2733f60>, <truth_tables.Person object at 0x7fe4a273e208>, <truth_tables.Person object at 0x7fe4a2737860>, <truth_tables.Person object at 0x7fe4a27444a8>, <truth_tables.Person object at 0x7fe4a273e978>, <truth_tables.Person object at 0x7fe4a273e9b0>, <truth_tables.Person object at 0x7fe4a27447f0>, <truth_tables.Person object at 0x7fe4a27491d0>, <truth_tables.Person object at 0x7fe4a2744860>, <truth_tables.Person object at 0x7fe4a2749a58>, <truth_tables.Person object at 0x7fe4a274e2e8>, <truth_tables.Person object at 0x7fe4a274e668>, <truth_tables.Person object at 0x7fe4a274ec18>, <truth_tables.Person object at 0x7fe4a274e940>, <truth_tables.Person object at 0x7fe4a2753358>, <truth_tables.Person object at 0x7fe4a2753b70>, <truth_tables.Person object at 0x7fe4a2759358>, <truth_tables.Person object at 0x7fe4a2759630>, <truth_tables.Person object at 0x7fe4a275eb38>, <truth_tables.Person object at 0x7fe4a26e5160>, <truth_tables.Person object at 0x7fe4a26e5668>, <truth_tables.Person object at 0x7fe4a26e5e48>, <truth_tables.Person object at 0x7fe4a26ee3c8>, <truth_tables.Person object at 0x7fe4a26ee518>, <truth_tables.Person object at 0x7fe4a26eeba8>, <truth_tables.Person object at 0x7fe4a26f4828>, <truth_tables.Person object at 0x7fe4a26f41d0>, <truth_tables.Person object at 0x7fe4a26f44a8>, <truth_tables.Person object at 0x7fe4a26f4f28>, <truth_tables.Person object at 0x7fe4a26fa2b0>, <truth_tables.Person object at 0x7fe4a26fa710>, <truth_tables.Person object at 0x7fe4a2700390>, <truth_tables.Person object at 0x7fe4a26fa588>, <truth_tables.Person object at 0x7fe4a2700dd8>, <truth_tables.Person object at 0x7fe4a2706080>, <truth_tables.Person object at 0x7fe4a2706390>, <truth_tables.Person object at 0x7fe4a2706898>, <truth_tables.Person object at 0x7fe4a2706f60>, <truth_tables.Person object at 0x7fe4a2706ac8>, <truth_tables.Person object at 0x7fe4a270a320>, <truth_tables.Person object at 0x7fe4a2711828>, <truth_tables.Person object at 0x7fe4a2711ac8>, <truth_tables.Person object at 0x7fe4a2716048>, <truth_tables.Person object at 0x7fe4a2711d68>, <truth_tables.Person object at 0x7fe4a2716dd8>, <truth_tables.Person object at 0x7fe4a271a400>, <truth_tables.Person object at 0x7fe4a271a588>, <truth_tables.Person object at 0x7fe4a2720198>, <truth_tables.Person object at 0x7fe4a27204e0>, <truth_tables.Person object at 0x7fe4a2720ac8>, <truth_tables.Person object at 0x7fe4a2720d68>, <truth_tables.Person object at 0x7fe4a2720b00>, <truth_tables.Person object at 0x7fe4a26a6a58>, <truth_tables.Person object at 0x7fe4a26aa080>, <truth_tables.Person object at 0x7fe4a26aa668>, <truth_tables.Person object at 0x7fe4a26aabe0>, <truth_tables.Person object at 0x7fe4a26aae80>, <truth_tables.Person object at 0x7fe4a26b1780>, <truth_tables.Person object at 0x7fe4a26aaeb8>, <truth_tables.Person object at 0x7fe4a26b1240>, <truth_tables.Person object at 0x7fe4a26bbac8>, <truth_tables.Person object at 0x7fe4a26bb6d8>, <truth_tables.Person object at 0x7fe4a26bbe10>, <truth_tables.Person object at 0x7fe4a26c2668>, <truth_tables.Person object at 0x7fe4a26c2be0>, <truth_tables.Person object at 0x7fe4a26c2a58>, <truth_tables.Person object at 0x7fe4a26c7160>, <truth_tables.Person object at 0x7fe4a26cd0f0>, <truth_tables.Person object at 0x7fe4a26cd8d0>, <truth_tables.Person object at 0x7fe4a26cda20>, <truth_tables.Person object at 0x7fe4a26d36a0>, <truth_tables.Person object at 0x7fe4a26d3be0>, <truth_tables.Person object at 0x7fe4a26d8588>, <truth_tables.Person object at 0x7fe4a26d8b70>, <truth_tables.Person object at 0x7fe4a26d8be0>, <truth_tables.Person object at 0x7fe4a26de780>, <truth_tables.Person object at 0x7fe4a26dedd8>, <truth_tables.Person object at 0x7fe4a26dee10>, <truth_tables.Person object at 0x7fe4a2668fd0>, <truth_tables.Person object at 0x7fe4a266d128>, <truth_tables.Person object at 0x7fe4a266de80>, <truth_tables.Person object at 0x7fe4a266d9b0>, <truth_tables.Person object at 0x7fe4a26739b0>, <truth_tables.Person object at 0x7fe4a26782b0>, <truth_tables.Person object at 0x7fe4a2673ac8>, <truth_tables.Person object at 0x7fe4a267f898>, <truth_tables.Person object at 0x7fe4a267f7b8>, <truth_tables.Person object at 0x7fe4a2684358>, <truth_tables.Person object at 0x7fe4a2684f98>, <truth_tables.Person object at 0x7fe4a2684fd0>, <truth_tables.Person object at 0x7fe4a2688b00>, <truth_tables.Person object at 0x7fe4a268ef28>, <truth_tables.Person object at 0x7fe4a268ef60>, <truth_tables.Person object at 0x7fe4a2693390>, <truth_tables.Person object at 0x7fe4a2699eb8>, <truth_tables.Person object at 0x7fe4a269e2e8>, <truth_tables.Person object at 0x7fe4a269e748>, <truth_tables.Person object at 0x7fe4a2624b00>, <truth_tables.Person object at 0x7fe4a269ef98>, <truth_tables.Person object at 0x7fe4a2624e10>, <truth_tables.Person object at 0x7fe4a2624e80>, <truth_tables.Person object at 0x7fe4a262a4a8>, <truth_tables.Person object at 0x7fe4a262a7f0>, <truth_tables.Person object at 0x7fe4a262a978>, <truth_tables.Person object at 0x7fe4a262ae48>, <truth_tables.Person object at 0x7fe4a262f400>, <truth_tables.Person object at 0x7fe4a2635630>, <truth_tables.Person object at 0x7fe4a263c198>, <truth_tables.Person object at 0x7fe4a263c4a8>, <truth_tables.Person object at 0x7fe4a263cc18>, <truth_tables.Person object at 0x7fe4a263c978>, <truth_tables.Person object at 0x7fe4a2646908>, <truth_tables.Person object at 0x7fe4a264bbe0>, <truth_tables.Person object at 0x7fe4a264bc50>, <truth_tables.Person object at 0x7fe4a2651518>, <truth_tables.Person object at 0x7fe4a2651fd0>, <truth_tables.Person object at 0x7fe4a2660128>, <truth_tables.Person object at 0x7fe4a26603c8>, <truth_tables.Person object at 0x7fe4a2660518>, <truth_tables.Person object at 0x7fe4a2660cf8>, <truth_tables.Person object at 0x7fe4a26609e8>, <truth_tables.Person object at 0x7fe4a25e52e8>, <truth_tables.Person object at 0x7fe4a25eb5c0>, <truth_tables.Person object at 0x7fe4a25eb8d0>, <truth_tables.Person object at 0x7fe4a25eb940>, <truth_tables.Person object at 0x7fe4a25f76d8>, <truth_tables.Person object at 0x7fe4a25f0d68>, <truth_tables.Person object at 0x7fe4a25f73c8>, <truth_tables.Person object at 0x7fe4a25fba90>, <truth_tables.Person object at 0x7fe4a26019e8>, <truth_tables.Person object at 0x7fe4a2601320>, <truth_tables.Person object at 0x7fe4a26014e0>, <truth_tables.Person object at 0x7fe4a2601f60>, <truth_tables.Person object at 0x7fe4a2607128>, <truth_tables.Person object at 0x7fe4a260ce80>, <truth_tables.Person object at 0x7fe4a260cba8>, <truth_tables.Person object at 0x7fe4a2612400>, <truth_tables.Person object at 0x7fe4a261c5f8>, <truth_tables.Person object at 0x7fe4a25a30f0>, <truth_tables.Person object at 0x7fe4a25a3550>, <truth_tables.Person object at 0x7fe4a25a3828>, <truth_tables.Person object at 0x7fe4a25a3ef0>, <truth_tables.Person object at 0x7fe4a25a3c88>, <truth_tables.Person object at 0x7fe4a25a3fd0>, <truth_tables.Person object at 0x7fe4a25a79e8>, <truth_tables.Person object at 0x7fe4a25ade10>, <truth_tables.Person object at 0x7fe4a25b4b00>, <truth_tables.Person object at 0x7fe4a25b4da0>, <truth_tables.Person object at 0x7fe4a25b8630>, <truth_tables.Person object at 0x7fe4a25b8be0>, <truth_tables.Person object at 0x7fe4a25bf198>, <truth_tables.Person object at 0x7fe4a25bfe80>, <truth_tables.Person object at 0x7fe4a25c35c0>, <truth_tables.Person object at 0x7fe4a25c3588>, <truth_tables.Person object at 0x7fe4a25c3898>, <truth_tables.Person object at 0x7fe4a25c9828>, <truth_tables.Person object at 0x7fe4a25c94a8>, <truth_tables.Person object at 0x7fe4a25c9390>, <truth_tables.Person object at 0x7fe4a25ce048>, <truth_tables.Person object at 0x7fe4a25ce2e8>, <truth_tables.Person object at 0x7fe4a25ceb00>, <truth_tables.Person object at 0x7fe4a25d32e8>, <truth_tables.Person object at 0x7fe4a25ced68>, <truth_tables.Person object at 0x7fe4a25d3e48>, <truth_tables.Person object at 0x7fe4a25d3f98>, <truth_tables.Person object at 0x7fe4a25d9b00>, <truth_tables.Person object at 0x7fe4a25df940>, <truth_tables.Person object at 0x7fe4a25dfe80>, <truth_tables.Person object at 0x7fe4a25dffd0>, <truth_tables.Person object at 0x7fe4a2563828>, <truth_tables.Person object at 0x7fe4a2563438>, <truth_tables.Person object at 0x7fe4a2563f28>, <truth_tables.Person object at 0x7fe4a2563ef0>, <truth_tables.Person object at 0x7fe4a256a128>, <truth_tables.Person object at 0x7fe4a256afd0>, <truth_tables.Person object at 0x7fe4a2570198>, <truth_tables.Person object at 0x7fe4a25705c0>, <truth_tables.Person object at 0x7fe4a2570a90>, <truth_tables.Person object at 0x7fe4a2570c18>, <truth_tables.Person object at 0x7fe4a25756a0>, <truth_tables.Person object at 0x7fe4a257b9b0>, <truth_tables.Person object at 0x7fe4a257bc18>, <truth_tables.Person object at 0x7fe4a257fa20>, <truth_tables.Person object at 0x7fe4a2585198>, <truth_tables.Person object at 0x7fe4a2585438>, <truth_tables.Person object at 0x7fe4a25857f0>, <truth_tables.Person object at 0x7fe4a2585cc0>, <truth_tables.Person object at 0x7fe4a258bcf8>, <truth_tables.Person object at 0x7fe4a2590e48>, <truth_tables.Person object at 0x7fe4a25978d0>, <truth_tables.Person object at 0x7fe4a259d3c8>, <truth_tables.Person object at 0x7fe4a259db70>, <truth_tables.Person object at 0x7fe4a259df98>, <truth_tables.Person object at 0x7fe4a2523cf8>, <truth_tables.Person object at 0x7fe4a2528e10>, <truth_tables.Person object at 0x7fe4a25284a8>, <truth_tables.Person object at 0x7fe4a252d8d0>, <truth_tables.Person object at 0x7fe4a252de10>, <truth_tables.Person object at 0x7fe4a25352b0>, <truth_tables.Person object at 0x7fe4a2535828>, <truth_tables.Person object at 0x7fe4a25380b8>, <truth_tables.Person object at 0x7fe4a2535b00>, <truth_tables.Person object at 0x7fe4a2535ef0>, <truth_tables.Person object at 0x7fe4a253e748>, <truth_tables.Person object at 0x7fe4a253e9b0>, <truth_tables.Person object at 0x7fe4a253eac8>, <truth_tables.Person object at 0x7fe4a25451d0>, <truth_tables.Person object at 0x7fe4a25456a0>, <truth_tables.Person object at 0x7fe4a253ef60>, <truth_tables.Person object at 0x7fe4a2545d68>, <truth_tables.Person object at 0x7fe4a254a390>, <truth_tables.Person object at 0x7fe4a254f278>, <truth_tables.Person object at 0x7fe4a254f588>, <truth_tables.Person object at 0x7fe4a254f908>, <truth_tables.Person object at 0x7fe4a2554518>, <truth_tables.Person object at 0x7fe4a255a320>, <truth_tables.Person object at 0x7fe4a255a5c0>, <truth_tables.Person object at 0x7fe4a255a710>, <truth_tables.Person object at 0x7fe4a255f358>, <truth_tables.Person object at 0x7fe4a255afd0>, <truth_tables.Person object at 0x7fe4a255f780>, <truth_tables.Person object at 0x7fe4a24e4eb8>, <truth_tables.Person object at 0x7fe4a24e4c88>, <truth_tables.Person object at 0x7fe4a24e4cc0>, <truth_tables.Person object at 0x7fe4a24ee940>, <truth_tables.Person object at 0x7fe4a24eec18>, <truth_tables.Person object at 0x7fe4a24f5160>, <truth_tables.Person object at 0x7fe4a24fb710>, <truth_tables.Person object at 0x7fe4a24fbc88>, <truth_tables.Person object at 0x7fe4a2500710>, <truth_tables.Person object at 0x7fe4a2500438>, <truth_tables.Person object at 0x7fe4a25064e0>, <truth_tables.Person object at 0x7fe4a2506a58>, <truth_tables.Person object at 0x7fe4a2506ba8>, <truth_tables.Person object at 0x7fe4a250a278>, <truth_tables.Person object at 0x7fe4a250aef0>, <truth_tables.Person object at 0x7fe4a250acc0>, <truth_tables.Person object at 0x7fe4a25183c8>, <truth_tables.Person object at 0x7fe4a2511e48>, <truth_tables.Person object at 0x7fe4a251c278>, <truth_tables.Person object at 0x7fe4a251c4e0>, <truth_tables.Person object at 0x7fe4a251c828>, <truth_tables.Person object at 0x7fe4a24a6278>, <truth_tables.Person object at 0x7fe4a24a6518>, <truth_tables.Person object at 0x7fe4a24a6cf8>, <truth_tables.Person object at 0x7fe4a24ad208>, <truth_tables.Person object at 0x7fe4a24b3080>, <truth_tables.Person object at 0x7fe4a24b3208>, <truth_tables.Person object at 0x7fe4a24b3668>, <truth_tables.Person object at 0x7fe4a24b3ef0>, <truth_tables.Person object at 0x7fe4a24b83c8>, <truth_tables.Person object at 0x7fe4a24b8f60>, <truth_tables.Person object at 0x7fe4a24bec50>, <truth_tables.Person object at 0x7fe4a24c2ba8>, <truth_tables.Person object at 0x7fe4a24c2e48>, <truth_tables.Person object at 0x7fe4a24c8198>, <truth_tables.Person object at 0x7fe4a24cf160>, <truth_tables.Person object at 0x7fe4a24cf668>, <truth_tables.Person object at 0x7fe4a24cfda0>, <truth_tables.Person object at 0x7fe4a24d36d8>, <truth_tables.Person object at 0x7fe4a24d3fd0>, <truth_tables.Person object at 0x7fe4a24d3940>, <truth_tables.Person object at 0x7fe4a24d9cc0>, <truth_tables.Person object at 0x7fe4a24d99b0>, <truth_tables.Person object at 0x7fe4a24dd6a0>, <truth_tables.Person object at 0x7fe4a24ddbe0>, <truth_tables.Person object at 0x7fe4a2463048>, <truth_tables.Person object at 0x7fe4a24634e0>, <truth_tables.Person object at 0x7fe4a24639b0>, <truth_tables.Person object at 0x7fe4a24693c8>, <truth_tables.Person object at 0x7fe4a246fe10>, <truth_tables.Person object at 0x7fe4a24740f0>, <truth_tables.Person object at 0x7fe4a2474240>, <truth_tables.Person object at 0x7fe4a2474ac8>, <truth_tables.Person object at 0x7fe4a24745c0>, <truth_tables.Person object at 0x7fe4a24782e8>, <truth_tables.Person object at 0x7fe4a24785f8>, <truth_tables.Person object at 0x7fe4a2478780>, <truth_tables.Person object at 0x7fe4a2478f60>, <truth_tables.Person object at 0x7fe4a2481630>, <truth_tables.Person object at 0x7fe4a248d588>, <truth_tables.Person object at 0x7fe4a24981d0>, <truth_tables.Person object at 0x7fe4a2492be0>, <truth_tables.Person object at 0x7fe4a2498e48>, <truth_tables.Person object at 0x7fe4a249ce80>, <truth_tables.Person object at 0x7fe4a2422198>, <truth_tables.Person object at 0x7fe4a249c898>, <truth_tables.Person object at 0x7fe4a2422780>, <truth_tables.Person object at 0x7fe4a24225f8>, <truth_tables.Person object at 0x7fe4a242e400>, <truth_tables.Person object at 0x7fe4a2429cf8>, <truth_tables.Person object at 0x7fe4a242e550>, <truth_tables.Person object at 0x7fe4a242e908>, <truth_tables.Person object at 0x7fe4a242e780>, <truth_tables.Person object at 0x7fe4a2433ac8>, <truth_tables.Person object at 0x7fe4a2433390>, <truth_tables.Person object at 0x7fe4a2437358>, <truth_tables.Person object at 0x7fe4a243eac8>, <truth_tables.Person object at 0x7fe4a2445630>, <truth_tables.Person object at 0x7fe4a2449748>, <truth_tables.Person object at 0x7fe4a2449f60>, <truth_tables.Person object at 0x7fe4a244e240>, <truth_tables.Person object at 0x7fe4a244e518>, <truth_tables.Person object at 0x7fe4a2453080>, <truth_tables.Person object at 0x7fe4a2453898>, <truth_tables.Person object at 0x7fe4a2453ba8>, <truth_tables.Person object at 0x7fe4a2459438>, <truth_tables.Person object at 0x7fe4a2459278>, <truth_tables.Person object at 0x7fe4a2459f60>, <truth_tables.Person object at 0x7fe4a23e1828>, <truth_tables.Person object at 0x7fe4a23e1cc0>, <truth_tables.Person object at 0x7fe4a23e60b8>, <truth_tables.Person object at 0x7fe4a23e6fd0>, <truth_tables.Person object at 0x7fe4a23e6d30>, <truth_tables.Person object at 0x7fe4a23ea390>, <truth_tables.Person object at 0x7fe4a23eaba8>, <truth_tables.Person object at 0x7fe4a23ea630>, <truth_tables.Person object at 0x7fe4a23f1198>, <truth_tables.Person object at 0x7fe4a23eae10>, <truth_tables.Person object at 0x7fe4a23f14e0>, <truth_tables.Person object at 0x7fe4a23f1a20>, <truth_tables.Person object at 0x7fe4a23f15c0>, <truth_tables.Person object at 0x7fe4a23f6240>, <truth_tables.Person object at 0x7fe4a23f6390>, <truth_tables.Person object at 0x7fe4a23fbc18>, <truth_tables.Person object at 0x7fe4a2401320>, <truth_tables.Person object at 0x7fe4a2406198>, <truth_tables.Person object at 0x7fe4a24062e8>, <truth_tables.Person object at 0x7fe4a2406710>, <truth_tables.Person object at 0x7fe4a2406c50>, <truth_tables.Person object at 0x7fe4a240d5c0>, <truth_tables.Person object at 0x7fe4a240def0>, <truth_tables.Person object at 0x7fe4a2411a90>, <truth_tables.Person object at 0x7fe4a2411908>, <truth_tables.Person object at 0x7fe4a24116d8>, <truth_tables.Person object at 0x7fe4a24183c8>, <truth_tables.Person object at 0x7fe4a2418ef0>, <truth_tables.Person object at 0x7fe4a241ec18>, <truth_tables.Person object at 0x7fe4a23a2240>, <truth_tables.Person object at 0x7fe4a241eef0>, <truth_tables.Person object at 0x7fe4a241ec88>, <truth_tables.Person object at 0x7fe4a23a8358>, <truth_tables.Person object at 0x7fe4a23a8c18>, <truth_tables.Person object at 0x7fe4a23aefd0>, <truth_tables.Person object at 0x7fe4a23ae0f0>, <truth_tables.Person object at 0x7fe4a23b3588>, <truth_tables.Person object at 0x7fe4a23b3080>, <truth_tables.Person object at 0x7fe4a23b3a90>, <truth_tables.Person object at 0x7fe4a23b94e0>, <truth_tables.Person object at 0x7fe4a23b9f28>, <truth_tables.Person object at 0x7fe4a23be278>, <truth_tables.Person object at 0x7fe4a23be4e0>, <truth_tables.Person object at 0x7fe4a23bea20>, <truth_tables.Person object at 0x7fe4a23becc0>, <truth_tables.Person object at 0x7fe4a23c3be0>, <truth_tables.Person object at 0x7fe4a23c3eb8>, <truth_tables.Person object at 0x7fe4a23c9748>, <truth_tables.Person object at 0x7fe4a23c99e8>, <truth_tables.Person object at 0x7fe4a23c9f28>, <truth_tables.Person object at 0x7fe4a23cecc0>, <truth_tables.Person object at 0x7fe4a23d4278>, <truth_tables.Person object at 0x7fe4a23d4400>, <truth_tables.Person object at 0x7fe4a23d4898>, <truth_tables.Person object at 0x7fe4a23d9978>, <truth_tables.Person object at 0x7fe4a23d9ef0>, <truth_tables.Person object at 0x7fe4a23df5c0>, <truth_tables.Person object at 0x7fe4a23dfcf8>, <truth_tables.Person object at 0x7fe4a23dff28>, <truth_tables.Person object at 0x7fe4a23659b0>, <truth_tables.Person object at 0x7fe4a236a400>, <truth_tables.Person object at 0x7fe4a236a978>, <truth_tables.Person object at 0x7fe4a236aeb8>, <truth_tables.Person object at 0x7fe4a236f1d0>, <truth_tables.Person object at 0x7fe4a236f358>, <truth_tables.Person object at 0x7fe4a23742b0>, <truth_tables.Person object at 0x7fe4a23747b8>, <truth_tables.Person object at 0x7fe4a2374550>, <truth_tables.Person object at 0x7fe4a237b0b8>, <truth_tables.Person object at 0x7fe4a23744e0>, <truth_tables.Person object at 0x7fe4a2374cc0>, <truth_tables.Person object at 0x7fe4a237ba90>, <truth_tables.Person object at 0x7fe4a237bcf8>, <truth_tables.Person object at 0x7fe4a2387780>, <truth_tables.Person object at 0x7fe4a2387a20>, <truth_tables.Person object at 0x7fe4a238d320>, <truth_tables.Person object at 0x7fe4a238d5c0>, <truth_tables.Person object at 0x7fe4a238d8d0>, <truth_tables.Person object at 0x7fe4a238db38>, <truth_tables.Person object at 0x7fe4a23926d8>, <truth_tables.Person object at 0x7fe4a238d898>, <truth_tables.Person object at 0x7fe4a238d9e8>, <truth_tables.Person object at 0x7fe4a2397ef0>, <truth_tables.Person object at 0x7fe4a2321a58>, <truth_tables.Person object at 0x7fe4a2321e80>, <truth_tables.Person object at 0x7fe4a23259e8>, <truth_tables.Person object at 0x7fe4a23258d0>, <truth_tables.Person object at 0x7fe4a232c4e0>, <truth_tables.Person object at 0x7fe4a2325898>, <truth_tables.Person object at 0x7fe4a232cf28>, <truth_tables.Person object at 0x7fe4a2332080>, <truth_tables.Person object at 0x7fe4a2332518>, <truth_tables.Person object at 0x7fe4a2332cf8>, <truth_tables.Person object at 0x7fe4a2337940>, <truth_tables.Person object at 0x7fe4a23375c0>, <truth_tables.Person object at 0x7fe4a233b7f0>, <truth_tables.Person object at 0x7fe4a233ba90>, <truth_tables.Person object at 0x7fe4a2340518>, <truth_tables.Person object at 0x7fe4a2340320>, <truth_tables.Person object at 0x7fe4a2346fd0>, <truth_tables.Person object at 0x7fe4a234ba20>, <truth_tables.Person object at 0x7fe4a234bd30>, <truth_tables.Person object at 0x7fe4a2352da0>, <truth_tables.Person object at 0x7fe4a2357518>, <truth_tables.Person object at 0x7fe4a2357cc0>, <truth_tables.Person object at 0x7fe4a2357a20>, <truth_tables.Person object at 0x7fe4a22e62b0>, <truth_tables.Person object at 0x7fe4a22e6550>, <truth_tables.Person object at 0x7fe4a22ee198>, <truth_tables.Person object at 0x7fe4a22eeb38>, <truth_tables.Person object at 0x7fe4a22f3080>, <truth_tables.Person object at 0x7fe4a22f32b0>, <truth_tables.Person object at 0x7fe4a22f70f0>, <truth_tables.Person object at 0x7fe4a22fd160>, <truth_tables.Person object at 0x7fe4a22fd978>, <truth_tables.Person object at 0x7fe4a22fdda0>, <truth_tables.Person object at 0x7fe4a22fdfd0>, <truth_tables.Person object at 0x7fe4a2302ac8>, <truth_tables.Person object at 0x7fe4a23087b8>, <truth_tables.Person object at 0x7fe4a2308d30>, <truth_tables.Person object at 0x7fe4a230e048>, <truth_tables.Person object at 0x7fe4a230e5c0>, <truth_tables.Person object at 0x7fe4a230eb00>, <truth_tables.Person object at 0x7fe4a230ee80>, <truth_tables.Person object at 0x7fe4a23195c0>, <truth_tables.Person object at 0x7fe4a2319b00>, <truth_tables.Person object at 0x7fe4a231f358>, <truth_tables.Person object at 0x7fe4a231fdd8>, <truth_tables.Person object at 0x7fe4a231f3c8>, <truth_tables.Person object at 0x7fe4a22a3588>, <truth_tables.Person object at 0x7fe4a231f898>, <truth_tables.Person object at 0x7fe4a22a3dd8>, <truth_tables.Person object at 0x7fe4a22a90b8>, <truth_tables.Person object at 0x7fe4a22a98d0>, <truth_tables.Person object at 0x7fe4a22a9390>, <truth_tables.Person object at 0x7fe4a22a9d30>, <truth_tables.Person object at 0x7fe4a22af2e8>, <truth_tables.Person object at 0x7fe4a22ba898>, <truth_tables.Person object at 0x7fe4a22b4668>, <truth_tables.Person object at 0x7fe4a22ba208>, <truth_tables.Person object at 0x7fe4a22c5160>, <truth_tables.Person object at 0x7fe4a22ca080>, <truth_tables.Person object at 0x7fe4a22c5630>, <truth_tables.Person object at 0x7fe4a22ca240>, <truth_tables.Person object at 0x7fe4a22ca898>, <truth_tables.Person object at 0x7fe4a22ce390>, <truth_tables.Person object at 0x7fe4a22ce518>, <truth_tables.Person object at 0x7fe4a22d4048>, <truth_tables.Person object at 0x7fe4a22d45c0>, <truth_tables.Person object at 0x7fe4a22d9048>, <truth_tables.Person object at 0x7fe4a22d9898>, <truth_tables.Person object at 0x7fe4a22df2e8>, <truth_tables.Person object at 0x7fe4a22df7f0>, <truth_tables.Person object at 0x7fe4a2264048>, <truth_tables.Person object at 0x7fe4a22641d0>, <truth_tables.Person object at 0x7fe4a2264f28>, <truth_tables.Person object at 0x7fe4a226add8>, <truth_tables.Person object at 0x7fe4a226ab38>, <truth_tables.Person object at 0x7fe4a2272160>, <truth_tables.Person object at 0x7fe4a22722b0>, <truth_tables.Person object at 0x7fe4a2272cf8>, <truth_tables.Person object at 0x7fe4a2272fd0>, <truth_tables.Person object at 0x7fe4a22772b0>, <truth_tables.Person object at 0x7fe4a2277908>, <truth_tables.Person object at 0x7fe4a227c1d0>, <truth_tables.Person object at 0x7fe4a227c4a8>, <truth_tables.Person object at 0x7fe4a2280048>, <truth_tables.Person object at 0x7fe4a2280278>, <truth_tables.Person object at 0x7fe4a22864e0>, <truth_tables.Person object at 0x7fe4a228c208>, <truth_tables.Person object at 0x7fe4a228ccc0>, <truth_tables.Person object at 0x7fe4a22918d0>, <truth_tables.Person object at 0x7fe4a2291e10>, <truth_tables.Person object at 0x7fe4a2291b70>, <truth_tables.Person object at 0x7fe4a22975c0>, <truth_tables.Person object at 0x7fe4a2297860>, <truth_tables.Person object at 0x7fe4a2297d30>, <truth_tables.Person object at 0x7fe4a2297fd0>, <truth_tables.Person object at 0x7fe4a229dac8>, <truth_tables.Person object at 0x7fe4a229deb8>, <truth_tables.Person object at 0x7fe4a22235c0>, <truth_tables.Person object at 0x7fe4a2227518>, <truth_tables.Person object at 0x7fe4a2223cf8>, <truth_tables.Person object at 0x7fe4a2227ef0>, <truth_tables.Person object at 0x7fe4a2227dd8>, <truth_tables.Person object at 0x7fe4a222cbe0>, <truth_tables.Person object at 0x7fe4a22322b0>, <truth_tables.Person object at 0x7fe4a2232550>, <truth_tables.Person object at 0x7fe4a22329e8>, <truth_tables.Person object at 0x7fe4a2237940>, <truth_tables.Person object at 0x7fe4a223d898>, <truth_tables.Person object at 0x7fe4a2243470>, <truth_tables.Person object at 0x7fe4a2243cc0>, <truth_tables.Person object at 0x7fe4a2248588>, <truth_tables.Person object at 0x7fe4a2248da0>, <truth_tables.Person object at 0x7fe4a224d908>, <truth_tables.Person object at 0x7fe4a224dc18>, <truth_tables.Person object at 0x7fe4a22522b0>, <truth_tables.Person object at 0x7fe4a2252e48>, <truth_tables.Person object at 0x7fe4a2252780>, <truth_tables.Person object at 0x7fe4a22571d0>, <truth_tables.Person object at 0x7fe4a2257978>, <truth_tables.Person object at 0x7fe4a22576d8>, <truth_tables.Person object at 0x7fe4a225f400>, <truth_tables.Person object at 0x7fe4a225f908>, <truth_tables.Person object at 0x7fe4a21e4ba8>, <truth_tables.Person object at 0x7fe4a21ea630>, <truth_tables.Person object at 0x7fe4a21ea940>, <truth_tables.Person object at 0x7fe4a21eac50>, <truth_tables.Person object at 0x7fe4a21eaf98>, <truth_tables.Person object at 0x7fe4a21eafd0>, <truth_tables.Person object at 0x7fe4a21f0c88>, <truth_tables.Person object at 0x7fe4a21f5438>, <truth_tables.Person object at 0x7fe4a21fb898>, <truth_tables.Person object at 0x7fe4a22002b0>, <truth_tables.Person object at 0x7fe4a2200780>, <truth_tables.Person object at 0x7fe4a2200f60>, <truth_tables.Person object at 0x7fe4a22052e8>, <truth_tables.Person object at 0x7fe4a220b080>, <truth_tables.Person object at 0x7fe4a22055f8>, <truth_tables.Person object at 0x7fe4a220b860>, <truth_tables.Person object at 0x7fe4a220bb38>, <truth_tables.Person object at 0x7fe4a220be48>, <truth_tables.Person object at 0x7fe4a2211320>, <truth_tables.Person object at 0x7fe4a2211ac8>, <truth_tables.Person object at 0x7fe4a2217358>, <truth_tables.Person object at 0x7fe4a22176a0>, <truth_tables.Person object at 0x7fe4a2217ac8>, <truth_tables.Person object at 0x7fe4a221cf60>, <truth_tables.Person object at 0x7fe4a21a1278>, <truth_tables.Person object at 0x7fe4a21a1eb8>, <truth_tables.Person object at 0x7fe4a21ae9b0>, <truth_tables.Person object at 0x7fe4a21aeb00>, <truth_tables.Person object at 0x7fe4a21b3668>, <truth_tables.Person object at 0x7fe4a21b3f98>, <truth_tables.Person object at 0x7fe4a21b3cf8>, <truth_tables.Person object at 0x7fe4a21b8908>, <truth_tables.Person object at 0x7fe4a21bc0b8>, <truth_tables.Person object at 0x7fe4a21bc5c0>, <truth_tables.Person object at 0x7fe4a21bcac8>, <truth_tables.Person object at 0x7fe4a21bc668>, <truth_tables.Person object at 0x7fe4a21c1c50>, <truth_tables.Person object at 0x7fe4a21c8b38>, <truth_tables.Person object at 0x7fe4a21c8cc0>, <truth_tables.Person object at 0x7fe4a21ce6a0>, <truth_tables.Person object at 0x7fe4a21ce400>, <truth_tables.Person object at 0x7fe4a21d3828>, <truth_tables.Person object at 0x7fe4a21d3198>, <truth_tables.Person object at 0x7fe4a21cec88>, <truth_tables.Person object at 0x7fe4a21d3f98>, <truth_tables.Person object at 0x7fe4a21de748>, <truth_tables.Person object at 0x7fe4a21dec88>, <truth_tables.Person object at 0x7fe4a2164780>, <truth_tables.Person object at 0x7fe4a216a278>, <truth_tables.Person object at 0x7fe4a216aac8>, <truth_tables.Person object at 0x7fe4a216f080>, <truth_tables.Person object at 0x7fe4a216f860>, <truth_tables.Person object at 0x7fe4a2175908>, <truth_tables.Person object at 0x7fe4a21792b0>, <truth_tables.Person object at 0x7fe4a2180a20>, <truth_tables.Person object at 0x7fe4a2180cc0>, <truth_tables.Person object at 0x7fe4a2185048>, <truth_tables.Person object at 0x7fe4a2185588>, <truth_tables.Person object at 0x7fe4a2185ba8>, <truth_tables.Person object at 0x7fe4a218b908>, <truth_tables.Person object at 0x7fe4a2191470>, <truth_tables.Person object at 0x7fe4a21915c0>, <truth_tables.Person object at 0x7fe4a2191f60>, <truth_tables.Person object at 0x7fe4a21957b8>, <truth_tables.Person object at 0x7fe4a21955c0>, <truth_tables.Person object at 0x7fe4a21959b0>, <truth_tables.Person object at 0x7fe4a219b358>, <truth_tables.Person object at 0x7fe4a21214e0>, <truth_tables.Person object at 0x7fe4a2121748>, <truth_tables.Person object at 0x7fe4a2126208>, <truth_tables.Person object at 0x7fe4a2126668>, <truth_tables.Person object at 0x7fe4a2126390>, <truth_tables.Person object at 0x7fe4a2121978>, <truth_tables.Person object at 0x7fe4a2126c88>, <truth_tables.Person object at 0x7fe4a212cbe0>, <truth_tables.Person object at 0x7fe4a212c710>, <truth_tables.Person object at 0x7fe4a2130080>, <truth_tables.Person object at 0x7fe4a212ccc0>, <truth_tables.Person object at 0x7fe4a21308d0>, <truth_tables.Person object at 0x7fe4a2138668>, <truth_tables.Person object at 0x7fe4a2138be0>, <truth_tables.Person object at 0x7fe4a213d1d0>, <truth_tables.Person object at 0x7fe4a213d710>, <truth_tables.Person object at 0x7fe4a21423c8>, <truth_tables.Person object at 0x7fe4a2142da0>, <truth_tables.Person object at 0x7fe4a21480b8>, <truth_tables.Person object at 0x7fe4a21482e8>, <truth_tables.Person object at 0x7fe4a2148400>, <truth_tables.Person object at 0x7fe4a2148da0>, <truth_tables.Person object at 0x7fe4a214e198>, <truth_tables.Person object at 0x7fe4a214e978>, <truth_tables.Person object at 0x7fe4a2153048>, <truth_tables.Person object at 0x7fe4a214ebe0>, <truth_tables.Person object at 0x7fe4a2153e10>, <truth_tables.Person object at 0x7fe4a2153b70>, <truth_tables.Person object at 0x7fe4a215ac88>, <truth_tables.Person object at 0x7fe4a215eb38>, <truth_tables.Person object at 0x7fe4a215ee80>, <truth_tables.Person object at 0x7fe4a215e860>, <truth_tables.Person object at 0x7fe4a20e3438>, <truth_tables.Person object at 0x7fe4a20e3eb8>, <truth_tables.Person object at 0x7fe4a20e3908>, <truth_tables.Person object at 0x7fe4a20e93c8>, <truth_tables.Person object at 0x7fe4a20e3748>, <truth_tables.Person object at 0x7fe4a20ef588>, <truth_tables.Person object at 0x7fe4a20e3a20>, <truth_tables.Person object at 0x7fe4a20f5160>, <truth_tables.Person object at 0x7fe4a20f53c8>, <truth_tables.Person object at 0x7fe4a20f55f8>, <truth_tables.Person object at 0x7fe4a20f99e8>, <truth_tables.Person object at 0x7fe4a20f9f60>, <truth_tables.Person object at 0x7fe4a20ff2b0>, <truth_tables.Person object at 0x7fe4a20ffa20>, <truth_tables.Person object at 0x7fe4a21051d0>, <truth_tables.Person object at 0x7fe4a20ffc88>, <truth_tables.Person object at 0x7fe4a2105f98>, <truth_tables.Person object at 0x7fe4a20ffeb8>, <truth_tables.Person object at 0x7fe4a2105fd0>, <truth_tables.Person object at 0x7fe4a2110ba8>, <truth_tables.Person object at 0x7fe4a2114390>, <truth_tables.Person object at 0x7fe4a2114630>, <truth_tables.Person object at 0x7fe4a2114c50>, <truth_tables.Person object at 0x7fe4a211a198>, <truth_tables.Person object at 0x7fe4a2114eb8>, <truth_tables.Person object at 0x7fe4a211ad68>, <truth_tables.Person object at 0x7fe4a211af98>, <truth_tables.Person object at 0x7fe4a2120ba8>, <truth_tables.Person object at 0x7fe4a20a6198>, <truth_tables.Person object at 0x7fe4a20a6320>, <truth_tables.Person object at 0x7fe4a20a6a20>, <truth_tables.Person object at 0x7fe4a20ab828>, <truth_tables.Person object at 0x7fe4a20abd68>, <truth_tables.Person object at 0x7fe4a20b04e0>, <truth_tables.Person object at 0x7fe4a20b0cf8>, <truth_tables.Person object at 0x7fe4a20b0f98>, <truth_tables.Person object at 0x7fe4a20b5358>, <truth_tables.Person object at 0x7fe4a20b5b00>, <truth_tables.Person object at 0x7fe4a20bf048>, <truth_tables.Person object at 0x7fe4a20bf550>, <truth_tables.Person object at 0x7fe4a20bff60>, <truth_tables.Person object at 0x7fe4a20ca550>, <truth_tables.Person object at 0x7fe4a20ca898>, <truth_tables.Person object at 0x7fe4a20d06d8>, <truth_tables.Person object at 0x7fe4a20d0dd8>, <truth_tables.Person object at 0x7fe4a20d6898>, <truth_tables.Person object at 0x7fe4a20dd748>, <truth_tables.Person object at 0x7fe4a2062400>, <truth_tables.Person object at 0x7fe4a2068668>, <truth_tables.Person object at 0x7fe4a2068400>, <truth_tables.Person object at 0x7fe4a206e080>, <truth_tables.Person object at 0x7fe4a206e6a0>, <truth_tables.Person object at 0x7fe4a206eb00>, <truth_tables.Person object at 0x7fe4a20732b0>, <truth_tables.Person object at 0x7fe4a2073940>, <truth_tables.Person object at 0x7fe4a20735c0>, <truth_tables.Person object at 0x7fe4a2078be0>, <truth_tables.Person object at 0x7fe4a2078ef0>, <truth_tables.Person object at 0x7fe4a207e5c0>, <truth_tables.Person object at 0x7fe4a207ecc0>, <truth_tables.Person object at 0x7fe4a20824e0>, <truth_tables.Person object at 0x7fe4a20827b8>, <truth_tables.Person object at 0x7fe4a2082a58>, <truth_tables.Person object at 0x7fe4a2082cf8>, <truth_tables.Person object at 0x7fe4a20878d0>, <truth_tables.Person object at 0x7fe4a208c668>, <truth_tables.Person object at 0x7fe4a208cbe0>, <truth_tables.Person object at 0x7fe4a2092b00>, <truth_tables.Person object at 0x7fe4a208ce80>, <truth_tables.Person object at 0x7fe4a20922e8>, <truth_tables.Person object at 0x7fe4a20995f8>, <truth_tables.Person object at 0x7fe4a2099b70>, <truth_tables.Person object at 0x7fe4a2099898>, <truth_tables.Person object at 0x7fe4a20997b8>, <truth_tables.Person object at 0x7fe4a209e438>, <truth_tables.Person object at 0x7fe4a2099ef0>, <truth_tables.Person object at 0x7fe4a209e9e8>, <truth_tables.Person object at 0x7fe4a20237b8>, <truth_tables.Person object at 0x7fe4a2023c50>, <truth_tables.Person object at 0x7fe4a2023c88>, <truth_tables.Person object at 0x7fe4a202e048>, <truth_tables.Person object at 0x7fe4a2028cf8>, <truth_tables.Person object at 0x7fe4a2034198>, <truth_tables.Person object at 0x7fe4a20390b8>, <truth_tables.Person object at 0x7fe4a2034b00>, <truth_tables.Person object at 0x7fe4a20395f8>, <truth_tables.Person object at 0x7fe4a203d940>, <truth_tables.Person object at 0x7fe4a20399b0>, <truth_tables.Person object at 0x7fe4a203dd68>, <truth_tables.Person object at 0x7fe4a203dc88>, <truth_tables.Person object at 0x7fe4a203dfd0>, <truth_tables.Person object at 0x7fe4a2043ba8>, <truth_tables.Person object at 0x7fe4a204c0f0>, <truth_tables.Person object at 0x7fe4a204cc50>, <truth_tables.Person object at 0x7fe4a204c940>, <truth_tables.Person object at 0x7fe4a204c6d8>, <truth_tables.Person object at 0x7fe4a2051358>, <truth_tables.Person object at 0x7fe4a2056588>, <truth_tables.Person object at 0x7fe4a2056748>, <truth_tables.Person object at 0x7fe4a205a0b8>, <truth_tables.Person object at 0x7fe4a205a898>, <truth_tables.Person object at 0x7fe4a1fe6198>, <truth_tables.Person object at 0x7fe4a1fe64a8>, <truth_tables.Person object at 0x7fe4a1fe6c88>, <truth_tables.Person object at 0x7fe4a1fe6d68>, <truth_tables.Person object at 0x7fe4a1fec0b8>, <truth_tables.Person object at 0x7fe4a1feca58>, <truth_tables.Person object at 0x7fe4a1ff12e8>, <truth_tables.Person object at 0x7fe4a1ff6240>, <truth_tables.Person object at 0x7fe4a1ff1e10>, <truth_tables.Person object at 0x7fe4a1ff6cf8>, <truth_tables.Person object at 0x7fe4a1ff69b0>, <truth_tables.Person object at 0x7fe4a20069e8>, <truth_tables.Person object at 0x7fe4a2006e10>, <truth_tables.Person object at 0x7fe4a2006e48>, <truth_tables.Person object at 0x7fe4a200c908>, <truth_tables.Person object at 0x7fe4a2012828>, <truth_tables.Person object at 0x7fe4a20129e8>, <truth_tables.Person object at 0x7fe4a2012898>, <truth_tables.Person object at 0x7fe4a2017400>, <truth_tables.Person object at 0x7fe4a20177f0>, <truth_tables.Person object at 0x7fe4a201d5c0>, <truth_tables.Person object at 0x7fe4a201dd30>, <truth_tables.Person object at 0x7fe4a201dfd0>, <truth_tables.Person object at 0x7fe4a201dda0>, <truth_tables.Person object at 0x7fe4a1fa1d30>, <truth_tables.Person object at 0x7fe4a1fa6320>, <truth_tables.Person object at 0x7fe4a1fa6470>, <truth_tables.Person object at 0x7fe4a1fa6da0>, <truth_tables.Person object at 0x7fe4a1fad940>, <truth_tables.Person object at 0x7fe4a1fb15c0>, <truth_tables.Person object at 0x7fe4a1fb80b8>, <truth_tables.Person object at 0x7fe4a1fb87b8>, <truth_tables.Person object at 0x7fe4a1fbd0f0>, <truth_tables.Person object at 0x7fe4a1fbd4a8>, <truth_tables.Person object at 0x7fe4a1fc3cf8>, <truth_tables.Person object at 0x7fe4a1fc9278>, <truth_tables.Person object at 0x7fe4a1fc9390>, <truth_tables.Person object at 0x7fe4a1fc9e80>, <truth_tables.Person object at 0x7fe4a1fd4048>, <truth_tables.Person object at 0x7fe4a1fd45f8>, <truth_tables.Person object at 0x7fe4a1fd4dd8>, <truth_tables.Person object at 0x7fe4a1fda630>, <truth_tables.Person object at 0x7fe4a1fde278>, <truth_tables.Person object at 0x7fe4a1fde630>, <truth_tables.Person object at 0x7fe4a1fdeb38>, <truth_tables.Person object at 0x7fe4a1f639b0>, <truth_tables.Person object at 0x7fe4a1f63cc0>, <truth_tables.Person object at 0x7fe4a1f63e48>, <truth_tables.Person object at 0x7fe4a1f68390>, <truth_tables.Person object at 0x7fe4a1f68978>, <truth_tables.Person object at 0x7fe4a1f68e10>, <truth_tables.Person object at 0x7fe4a1f6d400>, <truth_tables.Person object at 0x7fe4a1f7af28>, <truth_tables.Person object at 0x7fe4a1f7ada0>, <truth_tables.Person object at 0x7fe4a1f7ea90>, <truth_tables.Person object at 0x7fe4a1f83ba8>, <truth_tables.Person object at 0x7fe4a1f83eb8>, <truth_tables.Person object at 0x7fe4a1f89630>, <truth_tables.Person object at 0x7fe4a1f89978>, <truth_tables.Person object at 0x7fe4a1f89ac8>, <truth_tables.Person object at 0x7fe4a1f89e48>, <truth_tables.Person object at 0x7fe4a1f8d278>, <truth_tables.Person object at 0x7fe4a1f8d518>, <truth_tables.Person object at 0x7fe4a1f8da20>, <truth_tables.Person object at 0x7fe4a1f92550>, <truth_tables.Person object at 0x7fe4a1f8d3c8>, <truth_tables.Person object at 0x7fe4a1f987f0>, <truth_tables.Person object at 0x7fe4a1f98a90>, <truth_tables.Person object at 0x7fe4a1f98d30>, <truth_tables.Person object at 0x7fe4a1f9d390>, <truth_tables.Person object at 0x7fe4a1f9d630>, <truth_tables.Person object at 0x7fe4a1f9dda0>, <truth_tables.Person object at 0x7fe4a1f25160>, <truth_tables.Person object at 0x7fe4a1f25f98>, <truth_tables.Person object at 0x7fe4a1f29898>, <truth_tables.Person object at 0x7fe4a1f2e748>, <truth_tables.Person object at 0x7fe4a1f29c18>, <truth_tables.Person object at 0x7fe4a1f2e278>, <truth_tables.Person object at 0x7fe4a1f33128>, <truth_tables.Person object at 0x7fe4a1f33390>, <truth_tables.Person object at 0x7fe4a1f38a90>, <truth_tables.Person object at 0x7fe4a1f38358>, <truth_tables.Person object at 0x7fe4a1f384a8>, <truth_tables.Person object at 0x7fe4a1f38240>, <truth_tables.Person object at 0x7fe4a1f38128>, <truth_tables.Person object at 0x7fe4a1f3f278>, <truth_tables.Person object at 0x7fe4a1f454a8>, <truth_tables.Person object at 0x7fe4a1f457b8>, <truth_tables.Person object at 0x7fe4a1f45ac8>, <truth_tables.Person object at 0x7fe4a1f4a048>, <truth_tables.Person object at 0x7fe4a1f4a1d0>, <truth_tables.Person object at 0x7fe4a1f4a940>, <truth_tables.Person object at 0x7fe4a1f4ac88>, <truth_tables.Person object at 0x7fe4a1f54080>, <truth_tables.Person object at 0x7fe4a1f54358>, <truth_tables.Person object at 0x7fe4a1f54710>, <truth_tables.Person object at 0x7fe4a1f5b198>, <truth_tables.Person object at 0x7fe4a1f5b7f0>, <truth_tables.Person object at 0x7fe4a1f5bef0>, <truth_tables.Person object at 0x7fe4a1f60470>, <truth_tables.Person object at 0x7fe4a1f60b38>, <truth_tables.Person object at 0x7fe4a1ee6a20>, <truth_tables.Person object at 0x7fe4a1ee6f98>, <truth_tables.Person object at 0x7fe4a1eecba8>, <truth_tables.Person object at 0x7fe4a1eece48>, <truth_tables.Person object at 0x7fe4a1eece80>, <truth_tables.Person object at 0x7fe4a1ef6400>, <truth_tables.Person object at 0x7fe4a1efb080>, <truth_tables.Person object at 0x7fe4a1ef69b0>, <truth_tables.Person object at 0x7fe4a1efb400>, <truth_tables.Person object at 0x7fe4a1ef6cc0>, <truth_tables.Person object at 0x7fe4a1efb9b0>, <truth_tables.Person object at 0x7fe4a1f06278>, <truth_tables.Person object at 0x7fe4a1f06898>, <truth_tables.Person object at 0x7fe4a1f06cc0>, <truth_tables.Person object at 0x7fe4a1f06f28>, <truth_tables.Person object at 0x7fe4a1f0ac50>, <truth_tables.Person object at 0x7fe4a1f0a908>, <truth_tables.Person object at 0x7fe4a1f0aa20>, <truth_tables.Person object at 0x7fe4a1f0a9e8>, <truth_tables.Person object at 0x7fe4a1f11940>, <truth_tables.Person object at 0x7fe4a1f144e0>, <truth_tables.Person object at 0x7fe4a1f14ef0>, <truth_tables.Person object at 0x7fe4a1f1b7f0>, <truth_tables.Person object at 0x7fe4a1f1bd30>, <truth_tables.Person object at 0x7fe4a1f1ba20>, <truth_tables.Person object at 0x7fe4a1ea1588>, <truth_tables.Person object at 0x7fe4a1ea1160>, <truth_tables.Person object at 0x7fe4a1ea1dd8>, <truth_tables.Person object at 0x7fe4a1eac5f8>, <truth_tables.Person object at 0x7fe4a1eaca58>, <truth_tables.Person object at 0x7fe4a1eacb70>, <truth_tables.Person object at 0x7fe4a1eacbe0>, <truth_tables.Person object at 0x7fe4a1eb26a0>, <truth_tables.Person object at 0x7fe4a1eb7da0>, <truth_tables.Person object at 0x7fe4a1ebc2b0>, <truth_tables.Person object at 0x7fe4a1ebc438>, <truth_tables.Person object at 0x7fe4a1ebcb00>, <truth_tables.Person object at 0x7fe4a1ec2710>, <truth_tables.Person object at 0x7fe4a1ec2550>, <truth_tables.Person object at 0x7fe4a1ec2828>, <truth_tables.Person object at 0x7fe4a1ec2cf8>, <truth_tables.Person object at 0x7fe4a1ec8a58>, <truth_tables.Person object at 0x7fe4a1ecf6a0>, <truth_tables.Person object at 0x7fe4a1ed4470>, <truth_tables.Person object at 0x7fe4a1ed4978>, <truth_tables.Person object at 0x7fe4a1ed96d8>, <truth_tables.Person object at 0x7fe4a1edd3c8>, <truth_tables.Person object at 0x7fe4a1eddba8>, <truth_tables.Person object at 0x7fe4a1edde80>, <truth_tables.Person object at 0x7fe4a1e648d0>, <truth_tables.Person object at 0x7fe4a1e64c18>, <truth_tables.Person object at 0x7fe4a1e69e10>, <truth_tables.Person object at 0x7fe4a1e6f080>, <truth_tables.Person object at 0x7fe4a1e6fef0>, <truth_tables.Person object at 0x7fe4a1e74ba8>, <truth_tables.Person object at 0x7fe4a1e74eb8>, <truth_tables.Person object at 0x7fe4a1e74fd0>, <truth_tables.Person object at 0x7fe4a1e7f748>, <truth_tables.Person object at 0x7fe4a1e7fcc0>, <truth_tables.Person object at 0x7fe4a1e89358>, <truth_tables.Person object at 0x7fe4a1e89828>, <truth_tables.Person object at 0x7fe4a1e901d0>, <truth_tables.Person object at 0x7fe4a1e907b8>, <truth_tables.Person object at 0x7fe4a1e90a20>, <truth_tables.Person object at 0x7fe4a1e964a8>, <truth_tables.Person object at 0x7fe4a1e96da0>, <truth_tables.Person object at 0x7fe4a1e96f28>, <truth_tables.Person object at 0x7fe4a1ea07b8>, <truth_tables.Person object at 0x7fe4a1e2f400>, <truth_tables.Person object at 0x7fe4a1e29f60>, <truth_tables.Person object at 0x7fe4a1e2fda0>, <truth_tables.Person object at 0x7fe4a1e35908>, <truth_tables.Person object at 0x7fe4a1e35438>, <truth_tables.Person object at 0x7fe4a1e3bc18>, <truth_tables.Person object at 0x7fe4a1e40b70>, <truth_tables.Person object at 0x7fe4a1e408d0>, <truth_tables.Person object at 0x7fe4a1e4b198>, <truth_tables.Person object at 0x7fe4a1e502e8>, <truth_tables.Person object at 0x7fe4a1e508d0>, <truth_tables.Person object at 0x7fe4a1e50c18>, <truth_tables.Person object at 0x7fe4a1e50e80>, <truth_tables.Person object at 0x7fe4a1e56940>, <truth_tables.Person object at 0x7fe4a1e56eb8>, <truth_tables.Person object at 0x7fe4a1e5a198>, <truth_tables.Person object at 0x7fe4a1e60550>, <truth_tables.Person object at 0x7fe4a1e607f0>, <truth_tables.Person object at 0x7fe4a1e60b70>, <truth_tables.Person object at 0x7fe4a1de66a0>, <truth_tables.Person object at 0x7fe4a1de6c88>, <truth_tables.Person object at 0x7fe4a1de6f60>, <truth_tables.Person object at 0x7fe4a1debeb8>, <truth_tables.Person object at 0x7fe4a1df0ac8>, <truth_tables.Person object at 0x7fe4a1df54a8>, <truth_tables.Person object at 0x7fe4a1df5668>, <truth_tables.Person object at 0x7fe4a1df5278>, <truth_tables.Person object at 0x7fe4a1dfaac8>, <truth_tables.Person object at 0x7fe4a1dfe828>, <truth_tables.Person object at 0x7fe4a1dfea90>, <truth_tables.Person object at 0x7fe4a1e05e10>, <truth_tables.Person object at 0x7fe4a1e10cc0>, <truth_tables.Person object at 0x7fe4a1e10e10>, <truth_tables.Person object at 0x7fe4a1e14400>, <truth_tables.Person object at 0x7fe4a1e14f98>, <truth_tables.Person object at 0x7fe4a1e1a278>, <truth_tables.Person object at 0x7fe4a1e1ab70>, <truth_tables.Person object at 0x7fe4a1e1a9e8>, <truth_tables.Person object at 0x7fe4a1e20630>, <truth_tables.Person object at 0x7fe4a1e204a8>, <truth_tables.Person object at 0x7fe4a1da5390>, <truth_tables.Person object at 0x7fe4a1da5940>, <truth_tables.Person object at 0x7fe4a1da5e80>, <truth_tables.Person object at 0x7fe4a1da5eb8>, <truth_tables.Person object at 0x7fe4a1db14e0>, <truth_tables.Person object at 0x7fe4a1db1a58>, <truth_tables.Person object at 0x7fe4a1dbc080>, <truth_tables.Person object at 0x7fe4a1dbc1d0>, <truth_tables.Person object at 0x7fe4a1dbc470>, <truth_tables.Person object at 0x7fe4a1dbce48>, <truth_tables.Person object at 0x7fe4a1dc0940>, <truth_tables.Person object at 0x7fe4a1dc7828>, <truth_tables.Person object at 0x7fe4a1dc7470>, <truth_tables.Person object at 0x7fe4a1dc7b00>, <truth_tables.Person object at 0x7fe4a1dc7b38>, <truth_tables.Person object at 0x7fe4a1dd1b00>, <truth_tables.Person object at 0x7fe4a1dd1f98>, <truth_tables.Person object at 0x7fe4a1dd8e48>, <truth_tables.Person object at 0x7fe4a1dd84e0>, <truth_tables.Person object at 0x7fe4a1dd1d30>, <truth_tables.Person object at 0x7fe4a1dde470>, <truth_tables.Person object at 0x7fe4a1ddedd8>, <truth_tables.Person object at 0x7fe4a1d623c8>, <truth_tables.Person object at 0x7fe4a1ddef60>, <truth_tables.Person object at 0x7fe4a1ddee10>, <truth_tables.Person object at 0x7fe4a1d67be0>, <truth_tables.Person object at 0x7fe4a1d67eb8>, <truth_tables.Person object at 0x7fe4a1d72c88>, <truth_tables.Person object at 0x7fe4a1d77438>, <truth_tables.Person object at 0x7fe4a1d726d8>, <truth_tables.Person object at 0x7fe4a1d777b8>, <truth_tables.Person object at 0x7fe4a1d7b128>, <truth_tables.Person object at 0x7fe4a1d7b5c0>, <truth_tables.Person object at 0x7fe4a1d77cf8>, <truth_tables.Person object at 0x7fe4a1d7bb00>, <truth_tables.Person object at 0x7fe4a1d85828>, <truth_tables.Person object at 0x7fe4a1d81630>, <truth_tables.Person object at 0x7fe4a1d81be0>, <truth_tables.Person object at 0x7fe4a1d85518>, <truth_tables.Person object at 0x7fe4a1d81cc0>, <truth_tables.Person object at 0x7fe4a1d8dc18>, <truth_tables.Person object at 0x7fe4a1d93048>, <truth_tables.Person object at 0x7fe4a1d98b00>, <truth_tables.Person object at 0x7fe4a1d9d470>, <truth_tables.Person object at 0x7fe4a1d98ef0>, <truth_tables.Person object at 0x7fe4a1d9da58>, <truth_tables.Person object at 0x7fe4a1d9dfd0>, <truth_tables.Person object at 0x7fe4a1d2a048>, <truth_tables.Person object at 0x7fe4a1d2af60>, <truth_tables.Person object at 0x7fe4a1d2f4a8>, <truth_tables.Person object at 0x7fe4a1d332b0>, <truth_tables.Person object at 0x7fe4a1d33780>, <truth_tables.Person object at 0x7fe4a1d33898>, <truth_tables.Person object at 0x7fe4a1d390b8>, <truth_tables.Person object at 0x7fe4a1d33da0>, <truth_tables.Person object at 0x7fe4a1d39da0>, <truth_tables.Person object at 0x7fe4a1d39dd8>, <truth_tables.Person object at 0x7fe4a1d40a58>, <truth_tables.Person object at 0x7fe4a1d45748>, <truth_tables.Person object at 0x7fe4a1d45f28>, <truth_tables.Person object at 0x7fe4a1d45c88>, <truth_tables.Person object at 0x7fe4a1d4b898>, <truth_tables.Person object at 0x7fe4a1d4b5f8>, <truth_tables.Person object at 0x7fe4a1d4f390>, <truth_tables.Person object at 0x7fe4a1d544a8>, <truth_tables.Person object at 0x7fe4a1d54748>, <truth_tables.Person object at 0x7fe4a1d54ef0>, <truth_tables.Person object at 0x7fe4a1d54f28>, <truth_tables.Person object at 0x7fe4a1d54fd0>, <truth_tables.Person object at 0x7fe4a1d59cf8>, <truth_tables.Person object at 0x7fe4a1d591d0>, <truth_tables.Person object at 0x7fe4a1d5fb38>, <truth_tables.Person object at 0x7fe4a1d5fcc0>, <truth_tables.Person object at 0x7fe4a1ce4198>, <truth_tables.Person object at 0x7fe4a1ce4630>, <truth_tables.Person object at 0x7fe4a1ce4dd8>, <truth_tables.Person object at 0x7fe4a1ceb198>, <truth_tables.Person object at 0x7fe4a1cebfd0>, <truth_tables.Person object at 0x7fe4a1cf0f60>, <truth_tables.Person object at 0x7fe4a1cf4588>, <truth_tables.Person object at 0x7fe4a1cf4f60>, <truth_tables.Person object at 0x7fe4a1cfc4a8>, <truth_tables.Person object at 0x7fe4a1cfcfd0>, <truth_tables.Person object at 0x7fe4a1cfc630>, <truth_tables.Person object at 0x7fe4a1cfcbe0>, <truth_tables.Person object at 0x7fe4a1d023c8>, <truth_tables.Person object at 0x7fe4a1d026a0>, <truth_tables.Person object at 0x7fe4a1d02f28>, <truth_tables.Person object at 0x7fe4a1d0b8d0>, <truth_tables.Person object at 0x7fe4a1d0bba8>, <truth_tables.Person object at 0x7fe4a1d102e8>, <truth_tables.Person object at 0x7fe4a1d105c0>, <truth_tables.Person object at 0x7fe4a1d10860>, <truth_tables.Person object at 0x7fe4a1d10b00>, <truth_tables.Person object at 0x7fe4a1d10cc0>, <truth_tables.Person object at 0x7fe4a1d10ba8>, <truth_tables.Person object at 0x7fe4a1d179e8>, <truth_tables.Person object at 0x7fe4a1d177b8>, <truth_tables.Person object at 0x7fe4a1d1fdd8>, <truth_tables.Person object at 0x7fe4a1ca3320>, <truth_tables.Person object at 0x7fe4a1d1f8d0>, <truth_tables.Person object at 0x7fe4a1ca3f28>, <truth_tables.Person object at 0x7fe4a1ca9d68>, <truth_tables.Person object at 0x7fe4a1cad470>, <truth_tables.Person object at 0x7fe4a1cad908>, <truth_tables.Person object at 0x7fe4a1cadb70>, <truth_tables.Person object at 0x7fe4a1cb3550>, <truth_tables.Person object at 0x7fe4a1cb8550>, <truth_tables.Person object at 0x7fe4a1cb8e10>, <truth_tables.Person object at 0x7fe4a1cbea90>, <truth_tables.Person object at 0x7fe4a1cc44e0>, <truth_tables.Person object at 0x7fe4a1cc42b0>, <truth_tables.Person object at 0x7fe4a1cc9470>, <truth_tables.Person object at 0x7fe4a1cc9c18>, <truth_tables.Person object at 0x7fe4a1ccf2b0>, <truth_tables.Person object at 0x7fe4a1ccf438>, <truth_tables.Person object at 0x7fe4a1cd47b8>, <truth_tables.Person object at 0x7fe4a1cd4e10>, <truth_tables.Person object at 0x7fe4a1cd9a58>, <truth_tables.Person object at 0x7fe4a1ce09b0>, <truth_tables.Person object at 0x7fe4a1c674e0>, <truth_tables.Person object at 0x7fe4a1c704e0>, <truth_tables.Person object at 0x7fe4a1c6bc88>, <truth_tables.Person object at 0x7fe4a1c70e80>, <truth_tables.Person object at 0x7fe4a1c75a58>, <truth_tables.Person object at 0x7fe4a1c75cf8>, <truth_tables.Person object at 0x7fe4a1c7a518>, <truth_tables.Person object at 0x7fe4a1c75fd0>, <truth_tables.Person object at 0x7fe4a1c7a668>, <truth_tables.Person object at 0x7fe4a1c7afd0>, <truth_tables.Person object at 0x7fe4a1c81710>, <truth_tables.Person object at 0x7fe4a1c81780>, <truth_tables.Person object at 0x7fe4a1c81f28>, <truth_tables.Person object at 0x7fe4a1c861d0>, <truth_tables.Person object at 0x7fe4a1c864a8>, <truth_tables.Person object at 0x7fe4a1c86780>, <truth_tables.Person object at 0x7fe4a1c869e8>, <truth_tables.Person object at 0x7fe4a1c8cc18>, <truth_tables.Person object at 0x7fe4a1c8c3c8>, <truth_tables.Person object at 0x7fe4a1c93b00>, <truth_tables.Person object at 0x7fe4a1c93d30>, <truth_tables.Person object at 0x7fe4a1c98160>, <truth_tables.Person object at 0x7fe4a1c93fd0>, <truth_tables.Person object at 0x7fe4a1c9e8d0>, <truth_tables.Person object at 0x7fe4a1c9efd0>, <truth_tables.Person object at 0x7fe4a1c22f28>, <truth_tables.Person object at 0x7fe4a1c229b0>, <truth_tables.Person object at 0x7fe4a1c28160>, <truth_tables.Person object at 0x7fe4a1c28518>, <truth_tables.Person object at 0x7fe4a1c28668>, <truth_tables.Person object at 0x7fe4a1c2dac8>, <truth_tables.Person object at 0x7fe4a1c319e8>, <truth_tables.Person object at 0x7fe4a1c318d0>, <truth_tables.Person object at 0x7fe4a1c38cf8>, <truth_tables.Person object at 0x7fe4a1c3c5f8>, <truth_tables.Person object at 0x7fe4a1c3c898>, <truth_tables.Person object at 0x7fe4a1c3cc50>, <truth_tables.Person object at 0x7fe4a1c3cf98>, <truth_tables.Person object at 0x7fe4a1c42b38>, <truth_tables.Person object at 0x7fe4a1c3cfd0>, <truth_tables.Person object at 0x7fe4a1c420f0>, <truth_tables.Person object at 0x7fe4a1c46c88>, <truth_tables.Person object at 0x7fe4a1c46f98>, <truth_tables.Person object at 0x7fe4a1c4cba8>, <truth_tables.Person object at 0x7fe4a1c530f0>, <truth_tables.Person object at 0x7fe4a1c53908>, <truth_tables.Person object at 0x7fe4a1c576a0>, <truth_tables.Person object at 0x7fe4a1c60518>, <truth_tables.Person object at 0x7fe4a1c60ac8>, <truth_tables.Person object at 0x7fe4a1c60da0>, <truth_tables.Person object at 0x7fe4a1c60f28>, <truth_tables.Person object at 0x7fe4a1bee160>, <truth_tables.Person object at 0x7fe4a1bee2e8>, <truth_tables.Person object at 0x7fe4a1bee748>, <truth_tables.Person object at 0x7fe4a1bf2358>, <truth_tables.Person object at 0x7fe4a1bf2f98>, <truth_tables.Person object at 0x7fe4a1bf7a90>, <truth_tables.Person object at 0x7fe4a1bfdeb8>, <truth_tables.Person object at 0x7fe4a1bfd4e0>, <truth_tables.Person object at 0x7fe4a1c02eb8>, <truth_tables.Person object at 0x7fe4a1c0a128>, <truth_tables.Person object at 0x7fe4a1c0af28>, <truth_tables.Person object at 0x7fe4a1c0eb00>, <truth_tables.Person object at 0x7fe4a1c15f28>, <truth_tables.Person object at 0x7fe4a1c1a9b0>, <truth_tables.Person object at 0x7fe4a1c1ec88>, <truth_tables.Person object at 0x7fe4a1ba4128>, <truth_tables.Person object at 0x7fe4a1ba46d8>, <truth_tables.Person object at 0x7fe4a1ba4f60>, <truth_tables.Person object at 0x7fe4a1ba4c50>, <truth_tables.Person object at 0x7fe4a1baaa58>, <truth_tables.Person object at 0x7fe4a1baf6d8>, <truth_tables.Person object at 0x7fe4a1baf978>, <truth_tables.Person object at 0x7fe4a1bafef0>, <truth_tables.Person object at 0x7fe4a1bb44e0>, <truth_tables.Person object at 0x7fe4a1bb9198>, <truth_tables.Person object at 0x7fe4a1bb4780>, <truth_tables.Person object at 0x7fe4a1bb9ef0>, <truth_tables.Person object at 0x7fe4a1bbf358>, <truth_tables.Person object at 0x7fe4a1bc5320>, <truth_tables.Person object at 0x7fe4a1bc5cc0>, <truth_tables.Person object at 0x7fe4a1bca710>, <truth_tables.Person object at 0x7fe4a1bcf710>, <truth_tables.Person object at 0x7fe4a1bd3550>, <truth_tables.Person object at 0x7fe4a1bd3780>, <truth_tables.Person object at 0x7fe4a1bd3a90>, <truth_tables.Person object at 0x7fe4a1bd3ba8>, <truth_tables.Person object at 0x7fe4a1bd3f60>, <truth_tables.Person object at 0x7fe4a1bdb9b0>, <truth_tables.Person object at 0x7fe4a1bd3d30>, <truth_tables.Person object at 0x7fe4a1be0470>, <truth_tables.Person object at 0x7fe4a1bdbef0>, <truth_tables.Person object at 0x7fe4a1be07f0>, <truth_tables.Person object at 0x7fe4a1be0a20>, <truth_tables.Person object at 0x7fe4a1be0f60>, <truth_tables.Person object at 0x7fe4a1b65128>, <truth_tables.Person object at 0x7fe4a1b6a080>, <truth_tables.Person object at 0x7fe4a1b6a940>, <truth_tables.Person object at 0x7fe4a1b6a860>, <truth_tables.Person object at 0x7fe4a1b6e3c8>, <truth_tables.Person object at 0x7fe4a1b6e518>, <truth_tables.Person object at 0x7fe4a1b769e8>, <truth_tables.Person object at 0x7fe4a1b76da0>, <truth_tables.Person object at 0x7fe4a1b7dfd0>, <truth_tables.Person object at 0x7fe4a1b81080>, <truth_tables.Person object at 0x7fe4a1b81eb8>, <truth_tables.Person object at 0x7fe4a1b86198>, <truth_tables.Person object at 0x7fe4a1b8a588>, <truth_tables.Person object at 0x7fe4a1b8a828>, <truth_tables.Person object at 0x7fe4a1b97550>, <truth_tables.Person object at 0x7fe4a1b97d30>, <truth_tables.Person object at 0x7fe4a1b9b3c8>, <truth_tables.Person object at 0x7fe4a1ba0320>, <truth_tables.Person object at 0x7fe4a1ba07b8>, <truth_tables.Person object at 0x7fe4a1b27908>, <truth_tables.Person object at 0x7fe4a1b27b38>, <truth_tables.Person object at 0x7fe4a1b2c208>, <truth_tables.Person object at 0x7fe4a1b2cf98>, <truth_tables.Person object at 0x7fe4a1b2c8d0>, <truth_tables.Person object at 0x7fe4a1b31a90>, <truth_tables.Person object at 0x7fe4a1b31c18>, <truth_tables.Person object at 0x7fe4a1b389b0>, <truth_tables.Person object at 0x7fe4a1b3c2b0>, <truth_tables.Person object at 0x7fe4a1b3cac8>, <truth_tables.Person object at 0x7fe4a1b3cd68>, <truth_tables.Person object at 0x7fe4a1b424e0>, <truth_tables.Person object at 0x7fe4a1b48518>, <truth_tables.Person object at 0x7fe4a1b48630>, <truth_tables.Person object at 0x7fe4a1b48f60>, <truth_tables.Person object at 0x7fe4a1b4dd30>, <truth_tables.Person object at 0x7fe4a1b4deb8>, <truth_tables.Person object at 0x7fe4a1b51208>, <truth_tables.Person object at 0x7fe4a1b51b00>, <truth_tables.Person object at 0x7fe4a1b51e48>, <truth_tables.Person object at 0x7fe4a1b57160>, <truth_tables.Person object at 0x7fe4a1b57550>, <truth_tables.Person object at 0x7fe4a1b579e8>, <truth_tables.Person object at 0x7fe4a1b5c780>, <truth_tables.Person object at 0x7fe4a1b57668>, <truth_tables.Person object at 0x7fe4a1b5c3c8>, <truth_tables.Person object at 0x7fe4a1ae2630>, <truth_tables.Person object at 0x7fe4a1b5cf60>, <truth_tables.Person object at 0x7fe4a1ae2f98>, <truth_tables.Person object at 0x7fe4a1ae9c18>, <truth_tables.Person object at 0x7fe4a1ae9f98>, <truth_tables.Person object at 0x7fe4a1aedb00>, <truth_tables.Person object at 0x7fe4a1af24e0>, <truth_tables.Person object at 0x7fe4a1af2390>, <truth_tables.Person object at 0x7fe4a1af2c88>, <truth_tables.Person object at 0x7fe4a1af9208>, <truth_tables.Person object at 0x7fe4a1afe470>, <truth_tables.Person object at 0x7fe4a1af9f60>, <truth_tables.Person object at 0x7fe4a1afea90>, <truth_tables.Person object at 0x7fe4a1b046a0>, <truth_tables.Person object at 0x7fe4a1b04828>, <truth_tables.Person object at 0x7fe4a1b04eb8>, <truth_tables.Person object at 0x7fe4a1b08320>, <truth_tables.Person object at 0x7fe4a1b04cc0>, <truth_tables.Person object at 0x7fe4a1b08eb8>, <truth_tables.Person object at 0x7fe4a1b0ed68>, <truth_tables.Person object at 0x7fe4a1b13da0>, <truth_tables.Person object at 0x7fe4a1b13828>, <truth_tables.Person object at 0x7fe4a1b19d30>, <truth_tables.Person object at 0x7fe4a1b202b0>, <truth_tables.Person object at 0x7fe4a1b20e10>, <truth_tables.Person object at 0x7fe4a1aa4198>, <truth_tables.Person object at 0x7fe4a1aa43c8>, <truth_tables.Person object at 0x7fe4a1aa4630>, <truth_tables.Person object at 0x7fe4a1aa4b70>, <truth_tables.Person object at 0x7fe4a1aa4f98>, <truth_tables.Person object at 0x7fe4a1aa9710>, <truth_tables.Person object at 0x7fe4a1aafbe0>, <truth_tables.Person object at 0x7fe4a1aa9e80>, <truth_tables.Person object at 0x7fe4a1ab55f8>, <truth_tables.Person object at 0x7fe4a1ab5e10>, <truth_tables.Person object at 0x7fe4a1aba3c8>, <truth_tables.Person object at 0x7fe4a1ab5780>, <truth_tables.Person object at 0x7fe4a1ab5ac8>, <truth_tables.Person object at 0x7fe4a1aba710>, <truth_tables.Person object at 0x7fe4a1ac5080>, <truth_tables.Person object at 0x7fe4a1ac5b00>, <truth_tables.Person object at 0x7fe4a1ac53c8>, <truth_tables.Person object at 0x7fe4a1acf198>, <truth_tables.Person object at 0x7fe4a1acae80>, <truth_tables.Person object at 0x7fe4a1acf828>, <truth_tables.Person object at 0x7fe4a1acfb38>, <truth_tables.Person object at 0x7fe4a1ad4160>, <truth_tables.Person object at 0x7fe4a1ad4278>, <truth_tables.Person object at 0x7fe4a1ad4550>, <truth_tables.Person object at 0x7fe4a1ad49b0>, <truth_tables.Person object at 0x7fe4a1ad4a20>, <truth_tables.Person object at 0x7fe4a1adb940>, <truth_tables.Person object at 0x7fe4a1adfa20>, <truth_tables.Person object at 0x7fe4a1adfcc0>, <truth_tables.Person object at 0x7fe4a1a650b8>, <truth_tables.Person object at 0x7fe4a1a653c8>, <truth_tables.Person object at 0x7fe4a1adfd30>, <truth_tables.Person object at 0x7fe4a1a657f0>, <truth_tables.Person object at 0x7fe4a1a68668>, <truth_tables.Person object at 0x7fe4a1a68a20>, <truth_tables.Person object at 0x7fe4a1a70198>, <truth_tables.Person object at 0x7fe4a1a709b0>, <truth_tables.Person object at 0x7fe4a1a706a0>, <truth_tables.Person object at 0x7fe4a1a75a20>, <truth_tables.Person object at 0x7fe4a1a75c88>, <truth_tables.Person object at 0x7fe4a1a75c50>, <truth_tables.Person object at 0x7fe4a1a80d68>, <truth_tables.Person object at 0x7fe4a1a7afd0>, <truth_tables.Person object at 0x7fe4a1a80278>, <truth_tables.Person object at 0x7fe4a1a80a58>, <truth_tables.Person object at 0x7fe4a1a88780>, <truth_tables.Person object at 0x7fe4a1a884a8>, <truth_tables.Person object at 0x7fe4a1a88e80>, <truth_tables.Person object at 0x7fe4a1a8b400>, <truth_tables.Person object at 0x7fe4a1a8bfd0>, <truth_tables.Person object at 0x7fe4a1a915c0>, <truth_tables.Person object at 0x7fe4a1a91d30>, <truth_tables.Person object at 0x7fe4a1a95ba8>, <truth_tables.Person object at 0x7fe4a1a951d0>, <truth_tables.Person object at 0x7fe4a1a95550>, <truth_tables.Person object at 0x7fe4a1a95898>, <truth_tables.Person object at 0x7fe4a1a9c1d0>, <truth_tables.Person object at 0x7fe4a1a9c470>, <truth_tables.Person object at 0x7fe4a1a9c5c0>, <truth_tables.Person object at 0x7fe4a1a9ca58>, <truth_tables.Person object at 0x7fe4a1a21048>, <truth_tables.Person object at 0x7fe4a1a214e0>, <truth_tables.Person object at 0x7fe4a1a21f98>, <truth_tables.Person object at 0x7fe4a1a21fd0>, <truth_tables.Person object at 0x7fe4a1a2c320>, <truth_tables.Person object at 0x7fe4a1a2c588>, <truth_tables.Person object at 0x7fe4a1a2c3c8>, <truth_tables.Person object at 0x7fe4a1a2cef0>, <truth_tables.Person object at 0x7fe4a1a34320>, <truth_tables.Person object at 0x7fe4a1a2cf28>, <truth_tables.Person object at 0x7fe4a1a349e8>, <truth_tables.Person object at 0x7fe4a1a34f60>, <truth_tables.Person object at 0x7fe4a1a39240>, <truth_tables.Person object at 0x7fe4a1a394e0>, <truth_tables.Person object at 0x7fe4a1a39a20>, <truth_tables.Person object at 0x7fe4a1a39b38>, <truth_tables.Person object at 0x7fe4a1a43b70>, <truth_tables.Person object at 0x7fe4a1a43b38>, <truth_tables.Person object at 0x7fe4a1a50978>, <truth_tables.Person object at 0x7fe4a1a50dd8>, <truth_tables.Person object at 0x7fe4a1a55390>, <truth_tables.Person object at 0x7fe4a1a55d68>, <truth_tables.Person object at 0x7fe4a1a55400>, <truth_tables.Person object at 0x7fe4a1a5b358>, <truth_tables.Person object at 0x7fe4a1a5b588>, <truth_tables.Person object at 0x7fe4a1a5bf98>, <truth_tables.Person object at 0x7fe4a19e14a8>, <truth_tables.Person object at 0x7fe4a19e6278>, <truth_tables.Person object at 0x7fe4a19e6978>, <truth_tables.Person object at 0x7fe4a19eb048>, <truth_tables.Person object at 0x7fe4a19eb278>, <truth_tables.Person object at 0x7fe4a19ebd30>, <truth_tables.Person object at 0x7fe4a19ebb70>, <truth_tables.Person object at 0x7fe4a19f1588>, <truth_tables.Person object at 0x7fe4a19f60b8>, <truth_tables.Person object at 0x7fe4a19f6e10>, <truth_tables.Person object at 0x7fe4a19fd470>, <truth_tables.Person object at 0x7fe4a19fd6d8>, <truth_tables.Person object at 0x7fe4a19fdef0>, <truth_tables.Person object at 0x7fe4a1a02748>, <truth_tables.Person object at 0x7fe4a1a06710>, <truth_tables.Person object at 0x7fe4a1a06fd0>, <truth_tables.Person object at 0x7fe4a1a065f8>, <truth_tables.Person object at 0x7fe4a1a06d30>, <truth_tables.Person object at 0x7fe4a1a06d68>, <truth_tables.Person object at 0x7fe4a1a11438>, <truth_tables.Person object at 0x7fe4a1a11a58>, <truth_tables.Person object at 0x7fe4a1a11d68>, <truth_tables.Person object at 0x7fe4a1a17dd8>, <truth_tables.Person object at 0x7fe4a1a17320>, <truth_tables.Person object at 0x7fe4a1a17390>, <truth_tables.Person object at 0x7fe4a1a1bf60>, <truth_tables.Person object at 0x7fe4a19a1780>, <truth_tables.Person object at 0x7fe4a19a1550>, <truth_tables.Person object at 0x7fe4a19a77b8>, <truth_tables.Person object at 0x7fe4a19a7e48>, <truth_tables.Person object at 0x7fe4a19a7e80>, <truth_tables.Person object at 0x7fe4a19a7fd0>, <truth_tables.Person object at 0x7fe4a19b1b38>, <truth_tables.Person object at 0x7fe4a19b1b00>, <truth_tables.Person object at 0x7fe4a19b8438>, <truth_tables.Person object at 0x7fe4a19bc5f8>, <truth_tables.Person object at 0x7fe4a19c1320>, <truth_tables.Person object at 0x7fe4a19c18d0>, <truth_tables.Person object at 0x7fe4a19c7080>, <truth_tables.Person object at 0x7fe4a19c72e8>, <truth_tables.Person object at 0x7fe4a19c7e80>, <truth_tables.Person object at 0x7fe4a19d1978>, <truth_tables.Person object at 0x7fe4a19d1ac8>, <truth_tables.Person object at 0x7fe4a19de080>, <truth_tables.Person object at 0x7fe4a1962208>, <truth_tables.Person object at 0x7fe4a19627b8>, <truth_tables.Person object at 0x7fe4a1962da0>, <truth_tables.Person object at 0x7fe4a1967ac8>, <truth_tables.Person object at 0x7fe4a1967470>, <truth_tables.Person object at 0x7fe4a19675f8>, <truth_tables.Person object at 0x7fe4a19725c0>, <truth_tables.Person object at 0x7fe4a1972d68>, <truth_tables.Person object at 0x7fe4a19772e8>, <truth_tables.Person object at 0x7fe4a1977e10>, <truth_tables.Person object at 0x7fe4a1977470>, <truth_tables.Person object at 0x7fe4a1977940>, <truth_tables.Person object at 0x7fe4a1977780>, <truth_tables.Person object at 0x7fe4a197b2b0>, <truth_tables.Person object at 0x7fe4a197b9e8>, <truth_tables.Person object at 0x7fe4a1982780>, <truth_tables.Person object at 0x7fe4a1982cf8>, <truth_tables.Person object at 0x7fe4a19824a8>, <truth_tables.Person object at 0x7fe4a1982438>, <truth_tables.Person object at 0x7fe4a198b390>, <truth_tables.Person object at 0x7fe4a198b9e8>, <truth_tables.Person object at 0x7fe4a198bf28>, <truth_tables.Person object at 0x7fe4a19920b8>, <truth_tables.Person object at 0x7fe4a1995048>, <truth_tables.Person object at 0x7fe4a19954e0>, <truth_tables.Person object at 0x7fe4a1995d30>, <truth_tables.Person object at 0x7fe4a199c320>, <truth_tables.Person object at 0x7fe4a19215f8>, <truth_tables.Person object at 0x7fe4a1921748>, <truth_tables.Person object at 0x7fe4a1921ba8>, <truth_tables.Person object at 0x7fe4a19275f8>, <truth_tables.Person object at 0x7fe4a1927b38>, <truth_tables.Person object at 0x7fe4a192d2b0>, <truth_tables.Person object at 0x7fe4a192d588>, <truth_tables.Person object at 0x7fe4a192dba8>, <truth_tables.Person object at 0x7fe4a192df60>, <truth_tables.Person object at 0x7fe4a192dbe0>, <truth_tables.Person object at 0x7fe4a19323c8>, <truth_tables.Person object at 0x7fe4a1932ef0>, <truth_tables.Person object at 0x7fe4a1932f28>, <truth_tables.Person object at 0x7fe4a1938208>, <truth_tables.Person object at 0x7fe4a193d828>, <truth_tables.Person object at 0x7fe4a1949908>, <truth_tables.Person object at 0x7fe4a19495f8>, <truth_tables.Person object at 0x7fe4a1949fd0>, <truth_tables.Person object at 0x7fe4a1949cf8>, <truth_tables.Person object at 0x7fe4a194e630>, <truth_tables.Person object at 0x7fe4a1954390>, <truth_tables.Person object at 0x7fe4a1954518>, <truth_tables.Person object at 0x7fe4a1959470>, <truth_tables.Person object at 0x7fe4a195d2b0>, <truth_tables.Person object at 0x7fe4a18e3588>, <truth_tables.Person object at 0x7fe4a18e3be0>, <truth_tables.Person object at 0x7fe4a18e9160>, <truth_tables.Person object at 0x7fe4a18e97b8>, <truth_tables.Person object at 0x7fe4a18e9b70>, <truth_tables.Person object at 0x7fe4a18e9eb8>, <truth_tables.Person object at 0x7fe4a18f0278>, <truth_tables.Person object at 0x7fe4a18f52e8>, <truth_tables.Person object at 0x7fe4a18f5b38>, <truth_tables.Person object at 0x7fe4a18f9588>, <truth_tables.Person object at 0x7fe4a18f9278>, <truth_tables.Person object at 0x7fe4a18ff860>, <truth_tables.Person object at 0x7fe4a18ffe48>, <truth_tables.Person object at 0x7fe4a18ffd68>, <truth_tables.Person object at 0x7fe4a190a438>, <truth_tables.Person object at 0x7fe4a190a710>, <truth_tables.Person object at 0x7fe4a190acc0>, <truth_tables.Person object at 0x7fe4a190ab00>, <truth_tables.Person object at 0x7fe4a1910860>, <truth_tables.Person object at 0x7fe4a19147b8>, <truth_tables.Person object at 0x7fe4a1914630>, <truth_tables.Person object at 0x7fe4a1919978>, <truth_tables.Person object at 0x7fe4a19196d8>, <truth_tables.Person object at 0x7fe4a1919a20>, <truth_tables.Person object at 0x7fe4a1919f98>, <truth_tables.Person object at 0x7fe4a191eba8>, <truth_tables.Person object at 0x7fe4a18a65f8>, <truth_tables.Person object at 0x7fe4a18a6c88>, <truth_tables.Person object at 0x7fe4a18a6f28>, <truth_tables.Person object at 0x7fe4a18ab780>, <truth_tables.Person object at 0x7fe4a18a6f60>, <truth_tables.Person object at 0x7fe4a18a6fd0>, <truth_tables.Person object at 0x7fe4a18abe10>, <truth_tables.Person object at 0x7fe4a18af160>, <truth_tables.Person object at 0x7fe4a18af438>, <truth_tables.Person object at 0x7fe4a18af668>, <truth_tables.Person object at 0x7fe4a18b6240>, <truth_tables.Person object at 0x7fe4a18b6fd0>, <truth_tables.Person object at 0x7fe4a18b6da0>, <truth_tables.Person object at 0x7fe4a18bac50>, <truth_tables.Person object at 0x7fe4a18c7400>, <truth_tables.Person object at 0x7fe4a18c7ba8>, <truth_tables.Person object at 0x7fe4a18cb7f0>, <truth_tables.Person object at 0x7fe4a18cb630>, <truth_tables.Person object at 0x7fe4a18d19b0>, <truth_tables.Person object at 0x7fe4a18d1be0>, <truth_tables.Person object at 0x7fe4a18d6668>, <truth_tables.Person object at 0x7fe4a18d6908>, <truth_tables.Person object at 0x7fe4a18d6e48>, <truth_tables.Person object at 0x7fe4a18db978>, <truth_tables.Person object at 0x7fe4a18dbc18>, <truth_tables.Person object at 0x7fe4a1863518>, <truth_tables.Person object at 0x7fe4a18637f0>, <truth_tables.Person object at 0x7fe4a1868cf8>, <truth_tables.Person object at 0x7fe4a18689e8>, <truth_tables.Person object at 0x7fe4a186c0b8>, <truth_tables.Person object at 0x7fe4a186c9b0>, <truth_tables.Person object at 0x7fe4a186ce80>, <truth_tables.Person object at 0x7fe4a186cbe0>, <truth_tables.Person object at 0x7fe4a186c9e8>, <truth_tables.Person object at 0x7fe4a186ca20>, <truth_tables.Person object at 0x7fe4a1872128>, <truth_tables.Person object at 0x7fe4a18782b0>, <truth_tables.Person object at 0x7fe4a1872c18>, <truth_tables.Person object at 0x7fe4a1878908>, <truth_tables.Person object at 0x7fe4a187d3c8>, <truth_tables.Person object at 0x7fe4a187d6d8>, <truth_tables.Person object at 0x7fe4a187de80>, <truth_tables.Person object at 0x7fe4a1883588>, <truth_tables.Person object at 0x7fe4a18830f0>, <truth_tables.Person object at 0x7fe4a1887da0>, <truth_tables.Person object at 0x7fe4a188c2b0>, <truth_tables.Person object at 0x7fe4a18914e0>, <truth_tables.Person object at 0x7fe4a1891358>, <truth_tables.Person object at 0x7fe4a1891128>, <truth_tables.Person object at 0x7fe4a1896320>, <truth_tables.Person object at 0x7fe4a1896a20>, <truth_tables.Person object at 0x7fe4a189d1d0>, <truth_tables.Person object at 0x7fe4a1896cf8>, <truth_tables.Person object at 0x7fe4a189dac8>, <truth_tables.Person object at 0x7fe4a1822b70>, <truth_tables.Person object at 0x7fe4a1822eb8>, <truth_tables.Person object at 0x7fe4a1828278>, <truth_tables.Person object at 0x7fe4a1828da0>, <truth_tables.Person object at 0x7fe4a182eb00>, <truth_tables.Person object at 0x7fe4a182e518>, <truth_tables.Person object at 0x7fe4a1834908>, <truth_tables.Person object at 0x7fe4a18344a8>, <truth_tables.Person object at 0x7fe4a1834ef0>, <truth_tables.Person object at 0x7fe4a183a240>, <truth_tables.Person object at 0x7fe4a183a940>, <truth_tables.Person object at 0x7fe4a183e518>, <truth_tables.Person object at 0x7fe4a18440f0>, <truth_tables.Person object at 0x7fe4a1844cc0>, <truth_tables.Person object at 0x7fe4a1844828>, <truth_tables.Person object at 0x7fe4a1844a20>, <truth_tables.Person object at 0x7fe4a184ae80>, <truth_tables.Person object at 0x7fe4a184a748>, <truth_tables.Person object at 0x7fe4a184eb00>, <truth_tables.Person object at 0x7fe4a18555c0>, <truth_tables.Person object at 0x7fe4a1855e80>, <truth_tables.Person object at 0x7fe4a1859908>, <truth_tables.Person object at 0x7fe4a1859668>, <truth_tables.Person object at 0x7fe4a17e1208>, <truth_tables.Person object at 0x7fe4a17e1978>, <truth_tables.Person object at 0x7fe4a17e1ef0>, <truth_tables.Person object at 0x7fe4a17e1a20>, <truth_tables.Person object at 0x7fe4a17e7400>, <truth_tables.Person object at 0x7fe4a17e7630>, <truth_tables.Person object at 0x7fe4a17f14a8>, <truth_tables.Person object at 0x7fe4a17edf98>, <truth_tables.Person object at 0x7fe4a17f1630>, <truth_tables.Person object at 0x7fe4a17f1898>, <truth_tables.Person object at 0x7fe4a17f8320>, <truth_tables.Person object at 0x7fe4a17f8160>, <truth_tables.Person object at 0x7fe4a17f8e48>, <truth_tables.Person object at 0x7fe4a17f8fd0>, <truth_tables.Person object at 0x7fe4a17ffe80>, <truth_tables.Person object at 0x7fe4a17ffd68>, <truth_tables.Person object at 0x7fe4a1803438>, <truth_tables.Person object at 0x7fe4a1803588>, <truth_tables.Person object at 0x7fe4a1808208>, <truth_tables.Person object at 0x7fe4a18084e0>, <truth_tables.Person object at 0x7fe4a1803eb8>, <truth_tables.Person object at 0x7fe4a1808390>, <truth_tables.Person object at 0x7fe4a180c5c0>, <truth_tables.Person object at 0x7fe4a180c780>, <truth_tables.Person object at 0x7fe4a180cc88>, <truth_tables.Person object at 0x7fe4a1813208>, <truth_tables.Person object at 0x7fe4a18139e8>, <truth_tables.Person object at 0x7fe4a18136d8>, <truth_tables.Person object at 0x7fe4a1818898>, <truth_tables.Person object at 0x7fe4a181e1d0>, <truth_tables.Person object at 0x7fe4a1818748>, <truth_tables.Person object at 0x7fe4a17a32e8>, <truth_tables.Person object at 0x7fe4a181eb00>, <truth_tables.Person object at 0x7fe4a17a39e8>, <truth_tables.Person object at 0x7fe4a17a3780>, <truth_tables.Person object at 0x7fe4a17a8f60>, <truth_tables.Person object at 0x7fe4a17a89e8>, <truth_tables.Person object at 0x7fe4a17ad9e8>, <truth_tables.Person object at 0x7fe4a17b40f0>, <truth_tables.Person object at 0x7fe4a17b4860>, <truth_tables.Person object at 0x7fe4a17b4dd8>, <truth_tables.Person object at 0x7fe4a17ba0f0>, <truth_tables.Person object at 0x7fe4a17ba6d8>, <truth_tables.Person object at 0x7fe4a17ba358>, <truth_tables.Person object at 0x7fe4a17babe0>, <truth_tables.Person object at 0x7fe4a17bf978>, <truth_tables.Person object at 0x7fe4a17bf4e0>, <truth_tables.Person object at 0x7fe4a17bfef0>, <truth_tables.Person object at 0x7fe4a17c4e48>, <truth_tables.Person object at 0x7fe4a17c4e80>, <truth_tables.Person object at 0x7fe4a17c9e10>, <truth_tables.Person object at 0x7fe4a17c9b70>, <truth_tables.Person object at 0x7fe4a17cef60>, <truth_tables.Person object at 0x7fe4a17d5cc0>, <truth_tables.Person object at 0x7fe4a17da780>, <truth_tables.Person object at 0x7fe4a17daa20>, <truth_tables.Person object at 0x7fe4a17df278>, <truth_tables.Person object at 0x7fe4a17dfa90>, <truth_tables.Person object at 0x7fe4a17dffd0>, <truth_tables.Person object at 0x7fe4a1764be0>, <truth_tables.Person object at 0x7fe4a1764e80>, <truth_tables.Person object at 0x7fe4a176afd0>, <truth_tables.Person object at 0x7fe4a1770a58>, <truth_tables.Person object at 0x7fe4a1770748>, <truth_tables.Person object at 0x7fe4a17752b0>, <truth_tables.Person object at 0x7fe4a1775a20>, <truth_tables.Person object at 0x7fe4a177c278>, <truth_tables.Person object at 0x7fe4a177cdd8>, <truth_tables.Person object at 0x7fe4a1780748>, <truth_tables.Person object at 0x7fe4a1784908>, <truth_tables.Person object at 0x7fe4a1780cf8>, <truth_tables.Person object at 0x7fe4a178b518>, <truth_tables.Person object at 0x7fe4a17903c8>, <truth_tables.Person object at 0x7fe4a1790668>, <truth_tables.Person object at 0x7fe4a17950b8>]
```

```python
rset = set(recid)

surv = [i for i in pop if i not in rset]
```

Define a function for a bar plot.


```python

import matplotlib.pyplot as plt

def bar_plot(x, y):
    t = table(list(x), list(y))

    plt.bar(range(len(t)), list(t.values()), align='center') # Create a bar graph

    plt.xticks(range(len(t)), list(t.keys())) # Create xlabel names
```

<!-- ```{python} -->
<!-- bar_plot(recid, surv) -->
<!-- plt.title("All defendants") -->
<!-- plt.show() -->

<!-- ``` -->

That number is higher for African Americans at 44.85% and lower for whites at 23.45%.

<!-- ```{python} -->

<!-- is_afam = is_race("African-American") -->

<!-- bar_plot(filter(is_afam, recid), filter(is_afam, surv)) -->
<!-- plt.title("Black defendants") -->
<!-- plt.show() -->

<!-- ``` -->

<!-- ```{python} -->
<!-- is_white = is_race("Caucasian") -->

<!-- bar_plot(filter(is_white, recid), filter(is_white, surv)) -->
<!-- plt.title("White defendants") -->
<!-- plt.show() -->

<!-- ``` -->

### Risk of violent recidivism

Compas also offers a score that aims to measure a persons risk of violent recidivism, which has a similar overall accuracy to the Recidivism score.


```python

vpeople = []

with open("./data/cox-violent-parsed.csv") as f:
    reader = PeekyReader(DictReader(f))
    try:
        while True:
            p = Person(reader)
            if p.valid:
                vpeople.append(p)
    except StopIteration:
        pass

vpop = list(filter(lambda i: ((i.violent_recidivist == True and i.lifetime <= 730) or
                              i.lifetime > 730), list(filter(lambda x: x.vscore_valid, vpeople))))

vrecid = list(filter(lambda i: i.violent_recidivist == True and i.lifetime <= 730, vpeople))

vrset = set(vrecid)

vsurv = [i for i in vpop if i not in vrset]
```

<!-- ```{python} -->
<!-- bar_plot(vrecid, vsurv) -->
<!-- plt.title("All defendants") -->
<!-- plt.show() -->
<!-- ``` -->

Even more so for Black defendants.

<!-- ````{python} -->
<!-- is_afam = is_race("African-American") -->

<!-- bar_plot(filter(is_afam, vrecid), filter(is_afam, vsurv)) -->
<!-- plt.title("Black defendants") -->
<!-- plt.show() -->

<!-- ``` -->

<!-- ```{python} -->
<!-- is_white = is_race("Caucasian") -->

<!-- bar_plot(filter(is_white, vrecid), filter(is_white, vsurv)) -->
<!-- plt.title("White defendants") -->
<!-- plt.show() -->
<!-- ``` -->












<!--chapter:end:03_algorithm_accuracy.Rmd-->


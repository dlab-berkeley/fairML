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
## [<truth_tables.Person object at 0x7fd3062ab390>, <truth_tables.Person object at 0x7fd3062ab5f8>, <truth_tables.Person object at 0x7fd3062b00f0>, <truth_tables.Person object at 0x7fd3062b0e10>, <truth_tables.Person object at 0x7fd3062b5588>, <truth_tables.Person object at 0x7fd3062b5ac8>, <truth_tables.Person object at 0x7fd3062ba080>, <truth_tables.Person object at 0x7fd3062bab38>, <truth_tables.Person object at 0x7fd3062bae10>, <truth_tables.Person object at 0x7fd3062baf28>, <truth_tables.Person object at 0x7fd3062c02e8>, <truth_tables.Person object at 0x7fd3062c0588>, <truth_tables.Person object at 0x7fd3062c0c88>, <truth_tables.Person object at 0x7fd3062c0cf8>, <truth_tables.Person object at 0x7fd30624b0b8>, <truth_tables.Person object at 0x7fd30624b518>, <truth_tables.Person object at 0x7fd30624bd68>, <truth_tables.Person object at 0x7fd306250048>, <truth_tables.Person object at 0x7fd306250860>, <truth_tables.Person object at 0x7fd306254470>, <truth_tables.Person object at 0x7fd306254710>, <truth_tables.Person object at 0x7fd306254898>, <truth_tables.Person object at 0x7fd30625cba8>, <truth_tables.Person object at 0x7fd30625ce80>, <truth_tables.Person object at 0x7fd30625ccf8>, <truth_tables.Person object at 0x7fd30625cd68>, <truth_tables.Person object at 0x7fd306260ef0>, <truth_tables.Person object at 0x7fd306268470>, <truth_tables.Person object at 0x7fd306268c88>, <truth_tables.Person object at 0x7fd306268eb8>, <truth_tables.Person object at 0x7fd30626df98>, <truth_tables.Person object at 0x7fd306271630>, <truth_tables.Person object at 0x7fd30626dda0>, <truth_tables.Person object at 0x7fd306271fd0>, <truth_tables.Person object at 0x7fd30626de48>, <truth_tables.Person object at 0x7fd30626dcf8>, <truth_tables.Person object at 0x7fd306278c18>, <truth_tables.Person object at 0x7fd30627e4e0>, <truth_tables.Person object at 0x7fd30627ef60>, <truth_tables.Person object at 0x7fd30627ec50>, <truth_tables.Person object at 0x7fd306282da0>, <truth_tables.Person object at 0x7fd306207cf8>, <truth_tables.Person object at 0x7fd30620d0b8>, <truth_tables.Person object at 0x7fd30620d9e8>, <truth_tables.Person object at 0x7fd30620de10>, <truth_tables.Person object at 0x7fd30620d390>, <truth_tables.Person object at 0x7fd306213358>, <truth_tables.Person object at 0x7fd306213630>, <truth_tables.Person object at 0x7fd306213908>, <truth_tables.Person object at 0x7fd306213e48>, <truth_tables.Person object at 0x7fd306213da0>, <truth_tables.Person object at 0x7fd306219748>, <truth_tables.Person object at 0x7fd306219be0>, <truth_tables.Person object at 0x7fd306219ef0>, <truth_tables.Person object at 0x7fd30621ec18>, <truth_tables.Person object at 0x7fd306224780>, <truth_tables.Person object at 0x7fd306224cf8>, <truth_tables.Person object at 0x7fd30622f470>, <truth_tables.Person object at 0x7fd30622fda0>, <truth_tables.Person object at 0x7fd30622fb70>, <truth_tables.Person object at 0x7fd30622fa90>, <truth_tables.Person object at 0x7fd306233710>, <truth_tables.Person object at 0x7fd3062333c8>, <truth_tables.Person object at 0x7fd3062392e8>, <truth_tables.Person object at 0x7fd306239470>, <truth_tables.Person object at 0x7fd306239898>, <truth_tables.Person object at 0x7fd3062396d8>, <truth_tables.Person object at 0x7fd30623ef28>, <truth_tables.Person object at 0x7fd30623eb70>, <truth_tables.Person object at 0x7fd3061cd860>, <truth_tables.Person object at 0x7fd3061cd470>, <truth_tables.Person object at 0x7fd3061d2908>, <truth_tables.Person object at 0x7fd3061cdf60>, <truth_tables.Person object at 0x7fd3061d2198>, <truth_tables.Person object at 0x7fd3061d2dd8>, <truth_tables.Person object at 0x7fd3061d2e80>, <truth_tables.Person object at 0x7fd3061d2f28>, <truth_tables.Person object at 0x7fd3061dd358>, <truth_tables.Person object at 0x7fd3061ddba8>, <truth_tables.Person object at 0x7fd3061e4668>, <truth_tables.Person object at 0x7fd3061dde48>, <truth_tables.Person object at 0x7fd3061ddf98>, <truth_tables.Person object at 0x7fd3061e8588>, <truth_tables.Person object at 0x7fd3061e4128>, <truth_tables.Person object at 0x7fd3061e8cc0>, <truth_tables.Person object at 0x7fd3061ed6a0>, <truth_tables.Person object at 0x7fd3061edf28>, <truth_tables.Person object at 0x7fd3061f2978>, <truth_tables.Person object at 0x7fd3061f8ba8>, <truth_tables.Person object at 0x7fd306200a58>, <truth_tables.Person object at 0x7fd3061846d8>, <truth_tables.Person object at 0x7fd306189080>, <truth_tables.Person object at 0x7fd306189400>, <truth_tables.Person object at 0x7fd306189be0>, <truth_tables.Person object at 0x7fd306189e80>, <truth_tables.Person object at 0x7fd30618f0f0>, <truth_tables.Person object at 0x7fd30618f5f8>, <truth_tables.Person object at 0x7fd30618fc18>, <truth_tables.Person object at 0x7fd30618feb8>, <truth_tables.Person object at 0x7fd306195400>, <truth_tables.Person object at 0x7fd306195748>, <truth_tables.Person object at 0x7fd3061959e8>, <truth_tables.Person object at 0x7fd306195cc0>, <truth_tables.Person object at 0x7fd30619c668>, <truth_tables.Person object at 0x7fd306195eb8>, <truth_tables.Person object at 0x7fd3061a0048>, <truth_tables.Person object at 0x7fd3061a0cf8>, <truth_tables.Person object at 0x7fd3061a5278>, <truth_tables.Person object at 0x7fd3061a0a20>, <truth_tables.Person object at 0x7fd3061a50f0>, <truth_tables.Person object at 0x7fd3061af080>, <truth_tables.Person object at 0x7fd3061af940>, <truth_tables.Person object at 0x7fd3061b5470>, <truth_tables.Person object at 0x7fd3061b5a90>, <truth_tables.Person object at 0x7fd3061ba160>, <truth_tables.Person object at 0x7fd3061ba9b0>, <truth_tables.Person object at 0x7fd3061c04e0>, <truth_tables.Person object at 0x7fd3061c0630>, <truth_tables.Person object at 0x7fd306145128>, <truth_tables.Person object at 0x7fd306145f60>, <truth_tables.Person object at 0x7fd30614a2b0>, <truth_tables.Person object at 0x7fd3061504a8>, <truth_tables.Person object at 0x7fd306150cf8>, <truth_tables.Person object at 0x7fd306154518>, <truth_tables.Person object at 0x7fd306154e10>, <truth_tables.Person object at 0x7fd30615a1d0>, <truth_tables.Person object at 0x7fd30615a588>, <truth_tables.Person object at 0x7fd30615a898>, <truth_tables.Person object at 0x7fd30615f080>, <truth_tables.Person object at 0x7fd30615add8>, <truth_tables.Person object at 0x7fd30615f8d0>, <truth_tables.Person object at 0x7fd30615fba8>, <truth_tables.Person object at 0x7fd30616b550>, <truth_tables.Person object at 0x7fd30616bba8>, <truth_tables.Person object at 0x7fd306172588>, <truth_tables.Person object at 0x7fd306172710>, <truth_tables.Person object at 0x7fd306172978>, <truth_tables.Person object at 0x7fd30617c128>, <truth_tables.Person object at 0x7fd30617c8d0>, <truth_tables.Person object at 0x7fd30617ceb8>, <truth_tables.Person object at 0x7fd30617cef0>, <truth_tables.Person object at 0x7fd306181780>, <truth_tables.Person object at 0x7fd306181a90>, <truth_tables.Person object at 0x7fd3061075c0>, <truth_tables.Person object at 0x7fd306107860>, <truth_tables.Person object at 0x7fd306181fd0>, <truth_tables.Person object at 0x7fd306107da0>, <truth_tables.Person object at 0x7fd30610d390>, <truth_tables.Person object at 0x7fd30610d400>, <truth_tables.Person object at 0x7fd306112748>, <truth_tables.Person object at 0x7fd306112a20>, <truth_tables.Person object at 0x7fd3061174a8>, <truth_tables.Person object at 0x7fd306117cf8>, <truth_tables.Person object at 0x7fd30611d898>, <truth_tables.Person object at 0x7fd30611d400>, <truth_tables.Person object at 0x7fd306123748>, <truth_tables.Person object at 0x7fd306123fd0>, <truth_tables.Person object at 0x7fd306129358>, <truth_tables.Person object at 0x7fd306129e10>, <truth_tables.Person object at 0x7fd30612d940>, <truth_tables.Person object at 0x7fd30612de80>, <truth_tables.Person object at 0x7fd3061370f0>, <truth_tables.Person object at 0x7fd306137390>, <truth_tables.Person object at 0x7fd30613e7b8>, <truth_tables.Person object at 0x7fd3060c52e8>, <truth_tables.Person object at 0x7fd3060c59b0>, <truth_tables.Person object at 0x7fd3060c9e10>, <truth_tables.Person object at 0x7fd3060cf0f0>, <truth_tables.Person object at 0x7fd3060d5b00>, <truth_tables.Person object at 0x7fd3060d5f98>, <truth_tables.Person object at 0x7fd3060e04a8>, <truth_tables.Person object at 0x7fd3060e0c88>, <truth_tables.Person object at 0x7fd3060d9f60>, <truth_tables.Person object at 0x7fd3060e6710>, <truth_tables.Person object at 0x7fd3060e6978>, <truth_tables.Person object at 0x7fd3060e0fd0>, <truth_tables.Person object at 0x7fd3060ebc88>, <truth_tables.Person object at 0x7fd3060eb978>, <truth_tables.Person object at 0x7fd3060eb7f0>, <truth_tables.Person object at 0x7fd3060f5400>, <truth_tables.Person object at 0x7fd3060f50b8>, <truth_tables.Person object at 0x7fd3060fb2e8>, <truth_tables.Person object at 0x7fd3060fb668>, <truth_tables.Person object at 0x7fd3060fbcc0>, <truth_tables.Person object at 0x7fd306101e48>, <truth_tables.Person object at 0x7fd306087c88>, <truth_tables.Person object at 0x7fd306092630>, <truth_tables.Person object at 0x7fd306092898>, <truth_tables.Person object at 0x7fd306097c50>, <truth_tables.Person object at 0x7fd3060973c8>, <truth_tables.Person object at 0x7fd306097550>, <truth_tables.Person object at 0x7fd30609d278>, <truth_tables.Person object at 0x7fd30609d5c0>, <truth_tables.Person object at 0x7fd30609d828>, <truth_tables.Person object at 0x7fd3060a2630>, <truth_tables.Person object at 0x7fd3060a2b70>, <truth_tables.Person object at 0x7fd3060a85f8>, <truth_tables.Person object at 0x7fd3060a8da0>, <truth_tables.Person object at 0x7fd3060ae400>, <truth_tables.Person object at 0x7fd3060aebe0>, <truth_tables.Person object at 0x7fd3060aee10>, <truth_tables.Person object at 0x7fd3060b3828>, <truth_tables.Person object at 0x7fd3060aee48>, <truth_tables.Person object at 0x7fd3060b9b38>, <truth_tables.Person object at 0x7fd3060b9eb8>, <truth_tables.Person object at 0x7fd3060be240>, <truth_tables.Person object at 0x7fd3060be390>, <truth_tables.Person object at 0x7fd3060beba8>, <truth_tables.Person object at 0x7fd3060bee48>, <truth_tables.Person object at 0x7fd306044128>, <truth_tables.Person object at 0x7fd306044438>, <truth_tables.Person object at 0x7fd306044748>, <truth_tables.Person object at 0x7fd3060449b0>, <truth_tables.Person object at 0x7fd306044c50>, <truth_tables.Person object at 0x7fd306049e80>, <truth_tables.Person object at 0x7fd30604f668>, <truth_tables.Person object at 0x7fd30604fb38>, <truth_tables.Person object at 0x7fd30604f7b8>, <truth_tables.Person object at 0x7fd306055d30>, <truth_tables.Person object at 0x7fd30605a278>, <truth_tables.Person object at 0x7fd3060610f0>, <truth_tables.Person object at 0x7fd3060613c8>, <truth_tables.Person object at 0x7fd3060616a0>, <truth_tables.Person object at 0x7fd306061f60>, <truth_tables.Person object at 0x7fd306066438>, <truth_tables.Person object at 0x7fd306066518>, <truth_tables.Person object at 0x7fd30606c748>, <truth_tables.Person object at 0x7fd30606c208>, <truth_tables.Person object at 0x7fd30606cc50>, <truth_tables.Person object at 0x7fd30606cf28>, <truth_tables.Person object at 0x7fd3060763c8>, <truth_tables.Person object at 0x7fd3060766a0>, <truth_tables.Person object at 0x7fd306076978>, <truth_tables.Person object at 0x7fd30607c2e8>, <truth_tables.Person object at 0x7fd30600b438>, <truth_tables.Person object at 0x7fd30600b9e8>, <truth_tables.Person object at 0x7fd3060105c0>, <truth_tables.Person object at 0x7fd3060102e8>, <truth_tables.Person object at 0x7fd306010908>, <truth_tables.Person object at 0x7fd306010b70>, <truth_tables.Person object at 0x7fd306017b00>, <truth_tables.Person object at 0x7fd306017828>, <truth_tables.Person object at 0x7fd30601c198>, <truth_tables.Person object at 0x7fd30601c438>, <truth_tables.Person object at 0x7fd3060215c0>, <truth_tables.Person object at 0x7fd30601c908>, <truth_tables.Person object at 0x7fd306021cc0>, <truth_tables.Person object at 0x7fd306021390>, <truth_tables.Person object at 0x7fd306026518>, <truth_tables.Person object at 0x7fd30601c828>, <truth_tables.Person object at 0x7fd306026b38>, <truth_tables.Person object at 0x7fd306033518>, <truth_tables.Person object at 0x7fd306033240>, <truth_tables.Person object at 0x7fd306033cc0>, <truth_tables.Person object at 0x7fd306039898>, <truth_tables.Person object at 0x7fd30603d630>, <truth_tables.Person object at 0x7fd306042048>, <truth_tables.Person object at 0x7fd30603dcf8>, <truth_tables.Person object at 0x7fd305fca128>, <truth_tables.Person object at 0x7fd305fca550>, <truth_tables.Person object at 0x7fd305fcaba8>, <truth_tables.Person object at 0x7fd305fd0160>, <truth_tables.Person object at 0x7fd305fd06d8>, <truth_tables.Person object at 0x7fd305fd5198>, <truth_tables.Person object at 0x7fd305fd52e8>, <truth_tables.Person object at 0x7fd305fd9278>, <truth_tables.Person object at 0x7fd305fde0b8>, <truth_tables.Person object at 0x7fd305fde908>, <truth_tables.Person object at 0x7fd305fdeac8>, <truth_tables.Person object at 0x7fd305fdee80>, <truth_tables.Person object at 0x7fd305fe65c0>, <truth_tables.Person object at 0x7fd305fe6eb8>, <truth_tables.Person object at 0x7fd305feb3c8>, <truth_tables.Person object at 0x7fd305ff05c0>, <truth_tables.Person object at 0x7fd305ff09e8>, <truth_tables.Person object at 0x7fd305ff6748>, <truth_tables.Person object at 0x7fd305ff6978>, <truth_tables.Person object at 0x7fd305ff0898>, <truth_tables.Person object at 0x7fd305ffb7f0>, <truth_tables.Person object at 0x7fd3060020b8>, <truth_tables.Person object at 0x7fd306002b70>, <truth_tables.Person object at 0x7fd306002668>, <truth_tables.Person object at 0x7fd306002320>, <truth_tables.Person object at 0x7fd305f87a90>, <truth_tables.Person object at 0x7fd305f8c240>, <truth_tables.Person object at 0x7fd305f8c860>, <truth_tables.Person object at 0x7fd305f8c6d8>, <truth_tables.Person object at 0x7fd305f92b00>, <truth_tables.Person object at 0x7fd305f92828>, <truth_tables.Person object at 0x7fd305f92b38>, <truth_tables.Person object at 0x7fd305f96710>, <truth_tables.Person object at 0x7fd305f96be0>, <truth_tables.Person object at 0x7fd305f969b0>, <truth_tables.Person object at 0x7fd305fa3780>, <truth_tables.Person object at 0x7fd305fa3400>, <truth_tables.Person object at 0x7fd305f9ef98>, <truth_tables.Person object at 0x7fd305fa3b70>, <truth_tables.Person object at 0x7fd305fa3cc0>, <truth_tables.Person object at 0x7fd305fa9240>, <truth_tables.Person object at 0x7fd305fa93c8>, <truth_tables.Person object at 0x7fd305fa9e48>, <truth_tables.Person object at 0x7fd305fae160>, <truth_tables.Person object at 0x7fd305fae470>, <truth_tables.Person object at 0x7fd305fb3da0>, <truth_tables.Person object at 0x7fd305fbf390>, <truth_tables.Person object at 0x7fd305fbf4e0>, <truth_tables.Person object at 0x7fd305fbfdd8>, <truth_tables.Person object at 0x7fd305f44400>, <truth_tables.Person object at 0x7fd305f447b8>, <truth_tables.Person object at 0x7fd305f44668>, <truth_tables.Person object at 0x7fd305f49278>, <truth_tables.Person object at 0x7fd305f49400>, <truth_tables.Person object at 0x7fd305f4f470>, <truth_tables.Person object at 0x7fd305f530f0>, <truth_tables.Person object at 0x7fd305f53860>, <truth_tables.Person object at 0x7fd305f53ac8>, <truth_tables.Person object at 0x7fd305f539b0>, <truth_tables.Person object at 0x7fd305f5b940>, <truth_tables.Person object at 0x7fd305f5beb8>, <truth_tables.Person object at 0x7fd305f5bb38>, <truth_tables.Person object at 0x7fd305f64908>, <truth_tables.Person object at 0x7fd305f64c18>, <truth_tables.Person object at 0x7fd305f646d8>, <truth_tables.Person object at 0x7fd305f64d68>, <truth_tables.Person object at 0x7fd305f72208>, <truth_tables.Person object at 0x7fd305f72f60>, <truth_tables.Person object at 0x7fd305f7b160>, <truth_tables.Person object at 0x7fd305f76f60>, <truth_tables.Person object at 0x7fd305f7ba20>, <truth_tables.Person object at 0x7fd305f7b780>, <truth_tables.Person object at 0x7fd305f7bf98>, <truth_tables.Person object at 0x7fd305f80f98>, <truth_tables.Person object at 0x7fd305f059e8>, <truth_tables.Person object at 0x7fd305f05cf8>, <truth_tables.Person object at 0x7fd305f0db70>, <truth_tables.Person object at 0x7fd305f17940>, <truth_tables.Person object at 0x7fd305f17e10>, <truth_tables.Person object at 0x7fd305f1d160>, <truth_tables.Person object at 0x7fd305f1d630>, <truth_tables.Person object at 0x7fd305f21080>, <truth_tables.Person object at 0x7fd305f21668>, <truth_tables.Person object at 0x7fd305f217f0>, <truth_tables.Person object at 0x7fd305f2e0b8>, <truth_tables.Person object at 0x7fd305f27cf8>, <truth_tables.Person object at 0x7fd305f2e3c8>, <truth_tables.Person object at 0x7fd305f33a20>, <truth_tables.Person object at 0x7fd305f39da0>, <truth_tables.Person object at 0x7fd305f33fd0>, <truth_tables.Person object at 0x7fd305f3f128>, <truth_tables.Person object at 0x7fd305ec3160>, <truth_tables.Person object at 0x7fd305ec3630>, <truth_tables.Person object at 0x7fd305ec3c88>, <truth_tables.Person object at 0x7fd305ec9748>, <truth_tables.Person object at 0x7fd305ec9eb8>, <truth_tables.Person object at 0x7fd305ece8d0>, <truth_tables.Person object at 0x7fd305ed5208>, <truth_tables.Person object at 0x7fd305ed5710>, <truth_tables.Person object at 0x7fd305ed5978>, <truth_tables.Person object at 0x7fd305ee00b8>, <truth_tables.Person object at 0x7fd305ee0908>, <truth_tables.Person object at 0x7fd305ee0b38>, <truth_tables.Person object at 0x7fd305ee5828>, <truth_tables.Person object at 0x7fd305ee5b38>, <truth_tables.Person object at 0x7fd305ee9588>, <truth_tables.Person object at 0x7fd305ee9c50>, <truth_tables.Person object at 0x7fd305ee9ac8>, <truth_tables.Person object at 0x7fd305eeebe0>, <truth_tables.Person object at 0x7fd305eeecf8>, <truth_tables.Person object at 0x7fd305ef45f8>, <truth_tables.Person object at 0x7fd305ef4940>, <truth_tables.Person object at 0x7fd305ef4c88>, <truth_tables.Person object at 0x7fd305ef82b0>, <truth_tables.Person object at 0x7fd305ef8e80>, <truth_tables.Person object at 0x7fd305eff400>, <truth_tables.Person object at 0x7fd305ef8f98>, <truth_tables.Person object at 0x7fd305e854a8>, <truth_tables.Person object at 0x7fd305e85a20>, <truth_tables.Person object at 0x7fd305e85780>, <truth_tables.Person object at 0x7fd305e85f60>, <truth_tables.Person object at 0x7fd305e85f98>, <truth_tables.Person object at 0x7fd305e85fd0>, <truth_tables.Person object at 0x7fd305e8aa58>, <truth_tables.Person object at 0x7fd305e92da0>, <truth_tables.Person object at 0x7fd305e96d30>, <truth_tables.Person object at 0x7fd305e96f98>, <truth_tables.Person object at 0x7fd305e96d68>, <truth_tables.Person object at 0x7fd305e9b7b8>, <truth_tables.Person object at 0x7fd305e9be80>, <truth_tables.Person object at 0x7fd305ea6f28>, <truth_tables.Person object at 0x7fd305eadeb8>, <truth_tables.Person object at 0x7fd305eb23c8>, <truth_tables.Person object at 0x7fd305eb86a0>, <truth_tables.Person object at 0x7fd305eb8780>, <truth_tables.Person object at 0x7fd305eb89b0>, <truth_tables.Person object at 0x7fd305eb8e10>, <truth_tables.Person object at 0x7fd305ec23c8>, <truth_tables.Person object at 0x7fd305ebefd0>, <truth_tables.Person object at 0x7fd305ec28d0>, <truth_tables.Person object at 0x7fd305e494e0>, <truth_tables.Person object at 0x7fd305e49240>, <truth_tables.Person object at 0x7fd305e49898>, <truth_tables.Person object at 0x7fd305e49da0>, <truth_tables.Person object at 0x7fd305e499b0>, <truth_tables.Person object at 0x7fd305e532b0>, <truth_tables.Person object at 0x7fd305e53ba8>, <truth_tables.Person object at 0x7fd305e53908>, <truth_tables.Person object at 0x7fd305e53e10>, <truth_tables.Person object at 0x7fd305e58198>, <truth_tables.Person object at 0x7fd305e58940>, <truth_tables.Person object at 0x7fd305e58d68>, <truth_tables.Person object at 0x7fd305e58710>, <truth_tables.Person object at 0x7fd305e5d320>, <truth_tables.Person object at 0x7fd305e5ddd8>, <truth_tables.Person object at 0x7fd305e64ba8>, <truth_tables.Person object at 0x7fd305e68080>, <truth_tables.Person object at 0x7fd305e64908>, <truth_tables.Person object at 0x7fd305e6e518>, <truth_tables.Person object at 0x7fd305e73400>, <truth_tables.Person object at 0x7fd305e73a58>, <truth_tables.Person object at 0x7fd305e787f0>, <truth_tables.Person object at 0x7fd305e73d30>, <truth_tables.Person object at 0x7fd305e78d68>, <truth_tables.Person object at 0x7fd305e7f2b0>, <truth_tables.Person object at 0x7fd305e789b0>, <truth_tables.Person object at 0x7fd305e7fac8>, <truth_tables.Person object at 0x7fd305e7f3c8>, <truth_tables.Person object at 0x7fd305e05668>, <truth_tables.Person object at 0x7fd305e05c88>, <truth_tables.Person object at 0x7fd305e05ef0>, <truth_tables.Person object at 0x7fd305e0a198>, <truth_tables.Person object at 0x7fd305e05fd0>, <truth_tables.Person object at 0x7fd305e0aeb8>, <truth_tables.Person object at 0x7fd305e0f908>, <truth_tables.Person object at 0x7fd305e13908>, <truth_tables.Person object at 0x7fd305e13fd0>, <truth_tables.Person object at 0x7fd305e19f60>, <truth_tables.Person object at 0x7fd305e19588>, <truth_tables.Person object at 0x7fd305e19c88>, <truth_tables.Person object at 0x7fd305e21780>, <truth_tables.Person object at 0x7fd305e219b0>, <truth_tables.Person object at 0x7fd305e21be0>, <truth_tables.Person object at 0x7fd305e2b470>, <truth_tables.Person object at 0x7fd305e2ba58>, <truth_tables.Person object at 0x7fd305e2bf98>, <truth_tables.Person object at 0x7fd305e30278>, <truth_tables.Person object at 0x7fd305e36f28>, <truth_tables.Person object at 0x7fd305e3a2e8>, <truth_tables.Person object at 0x7fd305e3acf8>, <truth_tables.Person object at 0x7fd305e41e48>, <truth_tables.Person object at 0x7fd305dc6908>, <truth_tables.Person object at 0x7fd305dc6ef0>, <truth_tables.Person object at 0x7fd305dc6c18>, <truth_tables.Person object at 0x7fd305dcafd0>, <truth_tables.Person object at 0x7fd305dd15f8>, <truth_tables.Person object at 0x7fd305dd1c88>, <truth_tables.Person object at 0x7fd305dd6898>, <truth_tables.Person object at 0x7fd305dd6e80>, <truth_tables.Person object at 0x7fd305dd6710>, <truth_tables.Person object at 0x7fd305ddceb8>, <truth_tables.Person object at 0x7fd305ddc550>, <truth_tables.Person object at 0x7fd305de2f98>, <truth_tables.Person object at 0x7fd305de68d0>, <truth_tables.Person object at 0x7fd305de2588>, <truth_tables.Person object at 0x7fd305de6ef0>, <truth_tables.Person object at 0x7fd305dedc18>, <truth_tables.Person object at 0x7fd305df14e0>, <truth_tables.Person object at 0x7fd305df1c50>, <truth_tables.Person object at 0x7fd305df1da0>, <truth_tables.Person object at 0x7fd305dfd9b0>, <truth_tables.Person object at 0x7fd305dfdda0>, <truth_tables.Person object at 0x7fd305e012b0>, <truth_tables.Person object at 0x7fd305d890f0>, <truth_tables.Person object at 0x7fd305d89278>, <truth_tables.Person object at 0x7fd305d895f8>, <truth_tables.Person object at 0x7fd305d8eac8>, <truth_tables.Person object at 0x7fd305d8efd0>, <truth_tables.Person object at 0x7fd305d9a278>, <truth_tables.Person object at 0x7fd305d9a7f0>, <truth_tables.Person object at 0x7fd305d9f470>, <truth_tables.Person object at 0x7fd305d9f710>, <truth_tables.Person object at 0x7fd305d9ff28>, <truth_tables.Person object at 0x7fd305da5390>, <truth_tables.Person object at 0x7fd305da58d0>, <truth_tables.Person object at 0x7fd305db0128>, <truth_tables.Person object at 0x7fd305db0860>, <truth_tables.Person object at 0x7fd305db0ba8>, <truth_tables.Person object at 0x7fd305db50f0>, <truth_tables.Person object at 0x7fd305db0e48>, <truth_tables.Person object at 0x7fd305db9588>, <truth_tables.Person object at 0x7fd305db9828>, <truth_tables.Person object at 0x7fd305db9c50>, <truth_tables.Person object at 0x7fd305d460f0>, <truth_tables.Person object at 0x7fd305dc16a0>, <truth_tables.Person object at 0x7fd305dc1b70>, <truth_tables.Person object at 0x7fd305d46e10>, <truth_tables.Person object at 0x7fd305d4aac8>, <truth_tables.Person object at 0x7fd305d4ac50>, <truth_tables.Person object at 0x7fd305d4f080>, <truth_tables.Person object at 0x7fd305d4f2e8>, <truth_tables.Person object at 0x7fd305d4f358>, <truth_tables.Person object at 0x7fd305d4feb8>, <truth_tables.Person object at 0x7fd305d5bc50>, <truth_tables.Person object at 0x7fd305d5beb8>, <truth_tables.Person object at 0x7fd305d5bc88>, <truth_tables.Person object at 0x7fd305d5fc50>, <truth_tables.Person object at 0x7fd305d65748>, <truth_tables.Person object at 0x7fd305d65d68>, <truth_tables.Person object at 0x7fd305d65be0>, <truth_tables.Person object at 0x7fd305d6f588>, <truth_tables.Person object at 0x7fd305d6b908>, <truth_tables.Person object at 0x7fd305d6fd68>, <truth_tables.Person object at 0x7fd305d77710>, <truth_tables.Person object at 0x7fd305d7a940>, <truth_tables.Person object at 0x7fd305d807b8>, <truth_tables.Person object at 0x7fd305d80b00>, <truth_tables.Person object at 0x7fd305d80e48>, <truth_tables.Person object at 0x7fd305d07278>, <truth_tables.Person object at 0x7fd305d07ef0>, <truth_tables.Person object at 0x7fd305d0c5c0>, <truth_tables.Person object at 0x7fd305d07fd0>, <truth_tables.Person object at 0x7fd305d0ce10>, <truth_tables.Person object at 0x7fd305d12b00>, <truth_tables.Person object at 0x7fd305d17dd8>, <truth_tables.Person object at 0x7fd305d22860>, <truth_tables.Person object at 0x7fd305d1dd68>, <truth_tables.Person object at 0x7fd305d274a8>, <truth_tables.Person object at 0x7fd305d27780>, <truth_tables.Person object at 0x7fd305d27e10>, <truth_tables.Person object at 0x7fd305d2e438>, <truth_tables.Person object at 0x7fd305d2e898>, <truth_tables.Person object at 0x7fd305d2e9b0>, <truth_tables.Person object at 0x7fd305d334a8>, <truth_tables.Person object at 0x7fd305d33780>, <truth_tables.Person object at 0x7fd305d379e8>, <truth_tables.Person object at 0x7fd305d3c550>, <truth_tables.Person object at 0x7fd305d412b0>, <truth_tables.Person object at 0x7fd305d41400>, <truth_tables.Person object at 0x7fd305d41e48>, <truth_tables.Person object at 0x7fd305cc9198>, <truth_tables.Person object at 0x7fd305ccf6a0>, <truth_tables.Person object at 0x7fd305cc9940>, <truth_tables.Person object at 0x7fd305ccfb70>, <truth_tables.Person object at 0x7fd305cd35c0>, <truth_tables.Person object at 0x7fd305cd3cc0>, <truth_tables.Person object at 0x7fd305cd3e48>, <truth_tables.Person object at 0x7fd305cd8d30>, <truth_tables.Person object at 0x7fd305cdc828>, <truth_tables.Person object at 0x7fd305cdc3c8>, <truth_tables.Person object at 0x7fd305ce36a0>, <truth_tables.Person object at 0x7fd305ce8160>, <truth_tables.Person object at 0x7fd305ce3d30>, <truth_tables.Person object at 0x7fd305ce8ac8>, <truth_tables.Person object at 0x7fd305ce3e80>, <truth_tables.Person object at 0x7fd305ced400>, <truth_tables.Person object at 0x7fd305cf3160>, <truth_tables.Person object at 0x7fd305cf3940>, <truth_tables.Person object at 0x7fd305cf3e10>, <truth_tables.Person object at 0x7fd305cff470>, <truth_tables.Person object at 0x7fd305cff9e8>, <truth_tables.Person object at 0x7fd305cfff60>, <truth_tables.Person object at 0x7fd305c843c8>, <truth_tables.Person object at 0x7fd305c84be0>, <truth_tables.Person object at 0x7fd305c8e588>, <truth_tables.Person object at 0x7fd305c8efd0>, <truth_tables.Person object at 0x7fd305c914e0>, <truth_tables.Person object at 0x7fd305c91710>, <truth_tables.Person object at 0x7fd305c91d30>, <truth_tables.Person object at 0x7fd305c91b38>, <truth_tables.Person object at 0x7fd305c9b470>, <truth_tables.Person object at 0x7fd305c9bc18>, <truth_tables.Person object at 0x7fd305c9bda0>, <truth_tables.Person object at 0x7fd305ca0588>, <truth_tables.Person object at 0x7fd305ca0940>, <truth_tables.Person object at 0x7fd305ca4e80>, <truth_tables.Person object at 0x7fd305caa1d0>, <truth_tables.Person object at 0x7fd305caae80>, <truth_tables.Person object at 0x7fd305cae320>, <truth_tables.Person object at 0x7fd305cb4828>, <truth_tables.Person object at 0x7fd305cb4978>, <truth_tables.Person object at 0x7fd305cb4dd8>, <truth_tables.Person object at 0x7fd305cb92e8>, <truth_tables.Person object at 0x7fd305cbe1d0>, <truth_tables.Person object at 0x7fd305cbe5f8>, <truth_tables.Person object at 0x7fd305cbeb38>, <truth_tables.Person object at 0x7fd305cbedd8>, <truth_tables.Person object at 0x7fd305c44588>, <truth_tables.Person object at 0x7fd305c44c18>, <truth_tables.Person object at 0x7fd305c4ac50>, <truth_tables.Person object at 0x7fd305c508d0>, <truth_tables.Person object at 0x7fd305c50e48>, <truth_tables.Person object at 0x7fd305c56b38>, <truth_tables.Person object at 0x7fd305c56828>, <truth_tables.Person object at 0x7fd305c5be48>, <truth_tables.Person object at 0x7fd305c5f9b0>, <truth_tables.Person object at 0x7fd305c5fe48>, <truth_tables.Person object at 0x7fd305c65d68>, <truth_tables.Person object at 0x7fd305c6bba8>, <truth_tables.Person object at 0x7fd305c727b8>, <truth_tables.Person object at 0x7fd305c6bda0>, <truth_tables.Person object at 0x7fd305c72e10>, <truth_tables.Person object at 0x7fd305c76630>, <truth_tables.Person object at 0x7fd305c76f28>, <truth_tables.Person object at 0x7fd305c7c470>, <truth_tables.Person object at 0x7fd305c7c6d8>, <truth_tables.Person object at 0x7fd305c7cc18>, <truth_tables.Person object at 0x7fd305c7cef0>, <truth_tables.Person object at 0x7fd305c04ac8>, <truth_tables.Person object at 0x7fd305c04dd8>, <truth_tables.Person object at 0x7fd305c04ba8>, <truth_tables.Person object at 0x7fd305c09ac8>, <truth_tables.Person object at 0x7fd305c09e48>, <truth_tables.Person object at 0x7fd305c0e0f0>, <truth_tables.Person object at 0x7fd305c0e668>, <truth_tables.Person object at 0x7fd305c0ee48>, <truth_tables.Person object at 0x7fd305c12390>, <truth_tables.Person object at 0x7fd305c173c8>, <truth_tables.Person object at 0x7fd305c17668>, <truth_tables.Person object at 0x7fd305c1f940>, <truth_tables.Person object at 0x7fd305c1fef0>, <truth_tables.Person object at 0x7fd305c25278>, <truth_tables.Person object at 0x7fd305c25780>, <truth_tables.Person object at 0x7fd305c25ef0>, <truth_tables.Person object at 0x7fd305c29470>, <truth_tables.Person object at 0x7fd305c29cc0>, <truth_tables.Person object at 0x7fd305c2e320>, <truth_tables.Person object at 0x7fd305c2e828>, <truth_tables.Person object at 0x7fd305c2eb38>, <truth_tables.Person object at 0x7fd305c32128>, <truth_tables.Person object at 0x7fd305c3a080>, <truth_tables.Person object at 0x7fd305c3a390>, <truth_tables.Person object at 0x7fd305c3a668>, <truth_tables.Person object at 0x7fd305c3ae48>, <truth_tables.Person object at 0x7fd305c3a908>, <truth_tables.Person object at 0x7fd305c3ab70>, <truth_tables.Person object at 0x7fd305c3fd30>, <truth_tables.Person object at 0x7fd305c3fc18>, <truth_tables.Person object at 0x7fd305bcb2e8>, <truth_tables.Person object at 0x7fd305bcb860>, <truth_tables.Person object at 0x7fd305bcb588>, <truth_tables.Person object at 0x7fd305bcbda0>, <truth_tables.Person object at 0x7fd305bce320>, <truth_tables.Person object at 0x7fd305bd6048>, <truth_tables.Person object at 0x7fd305bd66d8>, <truth_tables.Person object at 0x7fd305bd6f60>, <truth_tables.Person object at 0x7fd305bdb780>, <truth_tables.Person object at 0x7fd305be09e8>, <truth_tables.Person object at 0x7fd305be0828>, <truth_tables.Person object at 0x7fd305bede48>, <truth_tables.Person object at 0x7fd305bf32b0>, <truth_tables.Person object at 0x7fd305bf38d0>, <truth_tables.Person object at 0x7fd305bf3b70>, <truth_tables.Person object at 0x7fd305bf7160>, <truth_tables.Person object at 0x7fd305bf7438>, <truth_tables.Person object at 0x7fd305bf7828>, <truth_tables.Person object at 0x7fd305bfc9e8>, <truth_tables.Person object at 0x7fd305c01240>, <truth_tables.Person object at 0x7fd305bfce48>, <truth_tables.Person object at 0x7fd305b85828>, <truth_tables.Person object at 0x7fd305b85cf8>, <truth_tables.Person object at 0x7fd305b85be0>, <truth_tables.Person object at 0x7fd305b8c0b8>, <truth_tables.Person object at 0x7fd305b90160>, <truth_tables.Person object at 0x7fd305b904a8>, <truth_tables.Person object at 0x7fd305b909b0>, <truth_tables.Person object at 0x7fd305b97438>, <truth_tables.Person object at 0x7fd305b97908>, <truth_tables.Person object at 0x7fd305b976a0>, <truth_tables.Person object at 0x7fd305b9ba20>, <truth_tables.Person object at 0x7fd305b9b7b8>, <truth_tables.Person object at 0x7fd305ba00f0>, <truth_tables.Person object at 0x7fd305ba0b38>, <truth_tables.Person object at 0x7fd305ba0d30>, <truth_tables.Person object at 0x7fd305ba0390>, <truth_tables.Person object at 0x7fd305ba0cf8>, <truth_tables.Person object at 0x7fd305ba8dd8>, <truth_tables.Person object at 0x7fd305bad2b0>, <truth_tables.Person object at 0x7fd305badc50>, <truth_tables.Person object at 0x7fd305bb2550>, <truth_tables.Person object at 0x7fd305bb2748>, <truth_tables.Person object at 0x7fd305bb2ef0>, <truth_tables.Person object at 0x7fd305bb2e80>, <truth_tables.Person object at 0x7fd305b430b8>, <truth_tables.Person object at 0x7fd305bbdcf8>, <truth_tables.Person object at 0x7fd305bbde10>, <truth_tables.Person object at 0x7fd305b43898>, <truth_tables.Person object at 0x7fd305b43f60>, <truth_tables.Person object at 0x7fd305b4b278>, <truth_tables.Person object at 0x7fd305b4b940>, <truth_tables.Person object at 0x7fd305b4e1d0>, <truth_tables.Person object at 0x7fd305b4e5f8>, <truth_tables.Person object at 0x7fd305b4e860>, <truth_tables.Person object at 0x7fd305b536d8>, <truth_tables.Person object at 0x7fd305b58748>, <truth_tables.Person object at 0x7fd305b5e8d0>, <truth_tables.Person object at 0x7fd305b670f0>, <truth_tables.Person object at 0x7fd305b67588>, <truth_tables.Person object at 0x7fd305b5ecf8>, <truth_tables.Person object at 0x7fd305b5ecc0>, <truth_tables.Person object at 0x7fd305b6b908>, <truth_tables.Person object at 0x7fd305b70860>, <truth_tables.Person object at 0x7fd305b75438>, <truth_tables.Person object at 0x7fd305b75978>, <truth_tables.Person object at 0x7fd305b75e80>, <truth_tables.Person object at 0x7fd305b7b588>, <truth_tables.Person object at 0x7fd305b7bfd0>, <truth_tables.Person object at 0x7fd305b05f60>, <truth_tables.Person object at 0x7fd305b05b38>, <truth_tables.Person object at 0x7fd305b0a7b8>, <truth_tables.Person object at 0x7fd305b0acf8>, <truth_tables.Person object at 0x7fd305b161d0>, <truth_tables.Person object at 0x7fd305b16438>, <truth_tables.Person object at 0x7fd305b16be0>, <truth_tables.Person object at 0x7fd305b16e80>, <truth_tables.Person object at 0x7fd305b16fd0>, <truth_tables.Person object at 0x7fd305b22128>, <truth_tables.Person object at 0x7fd305b228d0>, <truth_tables.Person object at 0x7fd305b22b70>, <truth_tables.Person object at 0x7fd305b27320>, <truth_tables.Person object at 0x7fd305b22e80>, <truth_tables.Person object at 0x7fd305b27ac8>, <truth_tables.Person object at 0x7fd305b27dd8>, <truth_tables.Person object at 0x7fd305b2ce80>, <truth_tables.Person object at 0x7fd305b32390>, <truth_tables.Person object at 0x7fd305b37a58>, <truth_tables.Person object at 0x7fd305b37d30>, <truth_tables.Person object at 0x7fd305b37fd0>, <truth_tables.Person object at 0x7fd305b3d278>, <truth_tables.Person object at 0x7fd305b3d908>, <truth_tables.Person object at 0x7fd305b414a8>, <truth_tables.Person object at 0x7fd305b41be0>, <truth_tables.Person object at 0x7fd305ac6390>, <truth_tables.Person object at 0x7fd305ac6d30>, <truth_tables.Person object at 0x7fd305acecf8>, <truth_tables.Person object at 0x7fd305acee80>, <truth_tables.Person object at 0x7fd305ad4940>, <truth_tables.Person object at 0x7fd305ad4b38>, <truth_tables.Person object at 0x7fd305ad9fd0>, <truth_tables.Person object at 0x7fd305ad9a90>, <truth_tables.Person object at 0x7fd305ad9cf8>, <truth_tables.Person object at 0x7fd305add5c0>, <truth_tables.Person object at 0x7fd305add898>, <truth_tables.Person object at 0x7fd305addcf8>, <truth_tables.Person object at 0x7fd305ae48d0>, <truth_tables.Person object at 0x7fd305aefa58>, <truth_tables.Person object at 0x7fd305aeada0>, <truth_tables.Person object at 0x7fd305af4710>, <truth_tables.Person object at 0x7fd305afa668>, <truth_tables.Person object at 0x7fd305a86518>, <truth_tables.Person object at 0x7fd305a86940>, <truth_tables.Person object at 0x7fd305a86cc0>, <truth_tables.Person object at 0x7fd305a86f28>, <truth_tables.Person object at 0x7fd305a8bdd8>, <truth_tables.Person object at 0x7fd305a90940>, <truth_tables.Person object at 0x7fd305a90f28>, <truth_tables.Person object at 0x7fd305a9b160>, <truth_tables.Person object at 0x7fd305aa74e0>, <truth_tables.Person object at 0x7fd305aab048>, <truth_tables.Person object at 0x7fd305aabf28>, <truth_tables.Person object at 0x7fd305ab1588>, <truth_tables.Person object at 0x7fd305aabc18>, <truth_tables.Person object at 0x7fd305ab1f28>, <truth_tables.Person object at 0x7fd305abbeb8>, <truth_tables.Person object at 0x7fd305ac1470>, <truth_tables.Person object at 0x7fd305ac1940>, <truth_tables.Person object at 0x7fd305a462b0>, <truth_tables.Person object at 0x7fd305a46710>, <truth_tables.Person object at 0x7fd305a53470>, <truth_tables.Person object at 0x7fd305a53ef0>, <truth_tables.Person object at 0x7fd305a53978>, <truth_tables.Person object at 0x7fd305a53710>, <truth_tables.Person object at 0x7fd305a59160>, <truth_tables.Person object at 0x7fd305a59908>, <truth_tables.Person object at 0x7fd305a59ba8>, <truth_tables.Person object at 0x7fd305a5e438>, <truth_tables.Person object at 0x7fd305a5e9b0>, <truth_tables.Person object at 0x7fd305a63080>, <truth_tables.Person object at 0x7fd305a5eeb8>, <truth_tables.Person object at 0x7fd305a633c8>, <truth_tables.Person object at 0x7fd305a63da0>, <truth_tables.Person object at 0x7fd305a63ac8>, <truth_tables.Person object at 0x7fd305a675c0>, <truth_tables.Person object at 0x7fd305a677b8>, <truth_tables.Person object at 0x7fd305a73f28>, <truth_tables.Person object at 0x7fd305a79e10>, <truth_tables.Person object at 0x7fd305a7e8d0>, <truth_tables.Person object at 0x7fd305a7eb38>, <truth_tables.Person object at 0x7fd305a03240>, <truth_tables.Person object at 0x7fd305a030f0>, <truth_tables.Person object at 0x7fd305a03c18>, <truth_tables.Person object at 0x7fd305a093c8>, <truth_tables.Person object at 0x7fd305a03fd0>, <truth_tables.Person object at 0x7fd305a0e2b0>, <truth_tables.Person object at 0x7fd305a0eef0>, <truth_tables.Person object at 0x7fd305a0efd0>, <truth_tables.Person object at 0x7fd305a12208>, <truth_tables.Person object at 0x7fd305a18390>, <truth_tables.Person object at 0x7fd305a18fd0>, <truth_tables.Person object at 0x7fd305a18e80>, <truth_tables.Person object at 0x7fd305a1dc18>, <truth_tables.Person object at 0x7fd305a238d0>, <truth_tables.Person object at 0x7fd305a23ba8>, <truth_tables.Person object at 0x7fd305a23940>, <truth_tables.Person object at 0x7fd305a29160>, <truth_tables.Person object at 0x7fd305a29320>, <truth_tables.Person object at 0x7fd305a2d9b0>, <truth_tables.Person object at 0x7fd305a2dac8>, <truth_tables.Person object at 0x7fd305a334a8>, <truth_tables.Person object at 0x7fd305a33940>, <truth_tables.Person object at 0x7fd305a33e80>, <truth_tables.Person object at 0x7fd305a39ac8>, <truth_tables.Person object at 0x7fd305a39940>, <truth_tables.Person object at 0x7fd305a3ee48>, <truth_tables.Person object at 0x7fd3059c54a8>, <truth_tables.Person object at 0x7fd305a3ecc0>, <truth_tables.Person object at 0x7fd3059c5a20>, <truth_tables.Person object at 0x7fd3059c5cc0>, <truth_tables.Person object at 0x7fd3059ca358>, <truth_tables.Person object at 0x7fd3059cac88>, <truth_tables.Person object at 0x7fd3059d00f0>, <truth_tables.Person object at 0x7fd3059caf28>, <truth_tables.Person object at 0x7fd3059cadd8>, <truth_tables.Person object at 0x7fd3059d4208>, <truth_tables.Person object at 0x7fd3059d4940>, <truth_tables.Person object at 0x7fd3059d47f0>, <truth_tables.Person object at 0x7fd3059db6a0>, <truth_tables.Person object at 0x7fd3059db518>, <truth_tables.Person object at 0x7fd3059dbe10>, <truth_tables.Person object at 0x7fd3059e0320>, <truth_tables.Person object at 0x7fd3059e0d30>, <truth_tables.Person object at 0x7fd3059e0f60>, <truth_tables.Person object at 0x7fd3059e5518>, <truth_tables.Person object at 0x7fd3059e5be0>, <truth_tables.Person object at 0x7fd3059ea1d0>, <truth_tables.Person object at 0x7fd3059ea470>, <truth_tables.Person object at 0x7fd3059eadd8>, <truth_tables.Person object at 0x7fd3059ef438>, <truth_tables.Person object at 0x7fd3059eff98>, <truth_tables.Person object at 0x7fd3059efda0>, <truth_tables.Person object at 0x7fd3059fb400>, <truth_tables.Person object at 0x7fd3059ff5c0>, <truth_tables.Person object at 0x7fd305985390>, <truth_tables.Person object at 0x7fd3059856a0>, <truth_tables.Person object at 0x7fd305985dd8>, <truth_tables.Person object at 0x7fd30598a860>, <truth_tables.Person object at 0x7fd3059959e8>, <truth_tables.Person object at 0x7fd30599a438>, <truth_tables.Person object at 0x7fd3059a1978>, <truth_tables.Person object at 0x7fd3059a12b0>, <truth_tables.Person object at 0x7fd3059a1f28>, <truth_tables.Person object at 0x7fd3059a67f0>, <truth_tables.Person object at 0x7fd3059a6860>, <truth_tables.Person object at 0x7fd3059b1b38>, <truth_tables.Person object at 0x7fd3059b1f98>, <truth_tables.Person object at 0x7fd3059b5a58>, <truth_tables.Person object at 0x7fd3059b5e48>, <truth_tables.Person object at 0x7fd3059bcd30>, <truth_tables.Person object at 0x7fd3059c19e8>, <truth_tables.Person object at 0x7fd3059c1eb8>, <truth_tables.Person object at 0x7fd3059483c8>, <truth_tables.Person object at 0x7fd305948198>, <truth_tables.Person object at 0x7fd30594d240>, <truth_tables.Person object at 0x7fd30594d860>, <truth_tables.Person object at 0x7fd3059525f8>, <truth_tables.Person object at 0x7fd30594dba8>, <truth_tables.Person object at 0x7fd305952c18>, <truth_tables.Person object at 0x7fd30595cf28>, <truth_tables.Person object at 0x7fd3059655c0>, <truth_tables.Person object at 0x7fd30596e7f0>, <truth_tables.Person object at 0x7fd305973198>, <truth_tables.Person object at 0x7fd305978470>, <truth_tables.Person object at 0x7fd30597eb38>, <truth_tables.Person object at 0x7fd30597ef98>, <truth_tables.Person object at 0x7fd305982d68>, <truth_tables.Person object at 0x7fd305908b00>, <truth_tables.Person object at 0x7fd30590d4e0>, <truth_tables.Person object at 0x7fd3059120b8>, <truth_tables.Person object at 0x7fd3059127b8>, <truth_tables.Person object at 0x7fd305912c88>, <truth_tables.Person object at 0x7fd305918908>, <truth_tables.Person object at 0x7fd305918f98>, <truth_tables.Person object at 0x7fd30591f4e0>, <truth_tables.Person object at 0x7fd30591fb00>, <truth_tables.Person object at 0x7fd305922eb8>, <truth_tables.Person object at 0x7fd305927ac8>, <truth_tables.Person object at 0x7fd305927898>, <truth_tables.Person object at 0x7fd305927668>, <truth_tables.Person object at 0x7fd30592d898>, <truth_tables.Person object at 0x7fd305933ba8>, <truth_tables.Person object at 0x7fd305937f98>, <truth_tables.Person object at 0x7fd30593e588>, <truth_tables.Person object at 0x7fd30593e9e8>, <truth_tables.Person object at 0x7fd30593ee10>, <truth_tables.Person object at 0x7fd3059425f8>, <truth_tables.Person object at 0x7fd3059429e8>, <truth_tables.Person object at 0x7fd3058c7240>, <truth_tables.Person object at 0x7fd3058c73c8>, <truth_tables.Person object at 0x7fd3058c7ac8>, <truth_tables.Person object at 0x7fd3058c7588>, <truth_tables.Person object at 0x7fd3058c7fd0>, <truth_tables.Person object at 0x7fd3058cf278>, <truth_tables.Person object at 0x7fd3058cf320>, <truth_tables.Person object at 0x7fd3058cfd30>, <truth_tables.Person object at 0x7fd3058e1400>, <truth_tables.Person object at 0x7fd3058e5f28>, <truth_tables.Person object at 0x7fd3058ec4a8>, <truth_tables.Person object at 0x7fd3058ec9e8>, <truth_tables.Person object at 0x7fd3058f0a58>, <truth_tables.Person object at 0x7fd3058f5160>, <truth_tables.Person object at 0x7fd3058fb278>, <truth_tables.Person object at 0x7fd3058f59b0>, <truth_tables.Person object at 0x7fd3058ff8d0>, <truth_tables.Person object at 0x7fd3058ffa20>, <truth_tables.Person object at 0x7fd3058858d0>, <truth_tables.Person object at 0x7fd305885d30>, <truth_tables.Person object at 0x7fd30588b668>, <truth_tables.Person object at 0x7fd30588b8d0>, <truth_tables.Person object at 0x7fd30588be48>, <truth_tables.Person object at 0x7fd30588bf98>, <truth_tables.Person object at 0x7fd3058907b8>, <truth_tables.Person object at 0x7fd305890e48>, <truth_tables.Person object at 0x7fd3058956a0>, <truth_tables.Person object at 0x7fd305890e80>, <truth_tables.Person object at 0x7fd305890cf8>, <truth_tables.Person object at 0x7fd3058955f8>, <truth_tables.Person object at 0x7fd30589c2e8>, <truth_tables.Person object at 0x7fd30589c5c0>, <truth_tables.Person object at 0x7fd30589c748>, <truth_tables.Person object at 0x7fd3058a1320>, <truth_tables.Person object at 0x7fd3058a1b38>, <truth_tables.Person object at 0x7fd3058a1dd8>, <truth_tables.Person object at 0x7fd3058a1eb8>, <truth_tables.Person object at 0x7fd3058a7c50>, <truth_tables.Person object at 0x7fd3058a7cf8>, <truth_tables.Person object at 0x7fd3058b1550>, <truth_tables.Person object at 0x7fd3058b7908>, <truth_tables.Person object at 0x7fd3058b1c88>, <truth_tables.Person object at 0x7fd3058bcb38>, <truth_tables.Person object at 0x7fd3058bc828>, <truth_tables.Person object at 0x7fd3058bcf28>, <truth_tables.Person object at 0x7fd305843710>, <truth_tables.Person object at 0x7fd305843a20>, <truth_tables.Person object at 0x7fd305848128>, <truth_tables.Person object at 0x7fd305848940>, <truth_tables.Person object at 0x7fd30584d048>, <truth_tables.Person object at 0x7fd30584d358>, <truth_tables.Person object at 0x7fd30584d630>, <truth_tables.Person object at 0x7fd30584de48>, <truth_tables.Person object at 0x7fd30584dba8>, <truth_tables.Person object at 0x7fd3058531d0>, <truth_tables.Person object at 0x7fd305853780>, <truth_tables.Person object at 0x7fd305853c88>, <truth_tables.Person object at 0x7fd305859358>, <truth_tables.Person object at 0x7fd3058597b8>, <truth_tables.Person object at 0x7fd305859b70>, <truth_tables.Person object at 0x7fd30585e080>, <truth_tables.Person object at 0x7fd30585e588>, <truth_tables.Person object at 0x7fd30585e320>, <truth_tables.Person object at 0x7fd30585edd8>, <truth_tables.Person object at 0x7fd305865240>, <truth_tables.Person object at 0x7fd3058693c8>, <truth_tables.Person object at 0x7fd305869dd8>, <truth_tables.Person object at 0x7fd30586d080>, <truth_tables.Person object at 0x7fd30586d358>, <truth_tables.Person object at 0x7fd30586db38>, <truth_tables.Person object at 0x7fd30586df60>, <truth_tables.Person object at 0x7fd3058752b0>, <truth_tables.Person object at 0x7fd305875b70>, <truth_tables.Person object at 0x7fd305879898>, <truth_tables.Person object at 0x7fd30587f1d0>, <truth_tables.Person object at 0x7fd30587f438>, <truth_tables.Person object at 0x7fd30587f908>, <truth_tables.Person object at 0x7fd305805f60>, <truth_tables.Person object at 0x7fd30580b208>, <truth_tables.Person object at 0x7fd30580b470>, <truth_tables.Person object at 0x7fd30580b6a0>, <truth_tables.Person object at 0x7fd305811780>, <truth_tables.Person object at 0x7fd305811128>, <truth_tables.Person object at 0x7fd305811278>, <truth_tables.Person object at 0x7fd305815748>, <truth_tables.Person object at 0x7fd305815a58>, <truth_tables.Person object at 0x7fd305815ba8>, <truth_tables.Person object at 0x7fd30581b668>, <truth_tables.Person object at 0x7fd30581bba8>, <truth_tables.Person object at 0x7fd30581bf60>, <truth_tables.Person object at 0x7fd305821630>, <truth_tables.Person object at 0x7fd305821978>, <truth_tables.Person object at 0x7fd305826048>, <truth_tables.Person object at 0x7fd305821e80>, <truth_tables.Person object at 0x7fd305826a58>, <truth_tables.Person object at 0x7fd30582b978>, <truth_tables.Person object at 0x7fd305830940>, <truth_tables.Person object at 0x7fd305830cf8>, <truth_tables.Person object at 0x7fd3058354e0>, <truth_tables.Person object at 0x7fd305835978>, <truth_tables.Person object at 0x7fd305835fd0>, <truth_tables.Person object at 0x7fd305835be0>, <truth_tables.Person object at 0x7fd30583a1d0>, <truth_tables.Person object at 0x7fd30583a8d0>, <truth_tables.Person object at 0x7fd30583aa20>, <truth_tables.Person object at 0x7fd30583abe0>, <truth_tables.Person object at 0x7fd30583ffd0>, <truth_tables.Person object at 0x7fd3057cc9e8>, <truth_tables.Person object at 0x7fd3057cce48>, <truth_tables.Person object at 0x7fd3057d2630>, <truth_tables.Person object at 0x7fd3057d2a58>, <truth_tables.Person object at 0x7fd3057d7eb8>, <truth_tables.Person object at 0x7fd3057db358>, <truth_tables.Person object at 0x7fd3057db5c0>, <truth_tables.Person object at 0x7fd3057e20f0>, <truth_tables.Person object at 0x7fd3057e2630>, <truth_tables.Person object at 0x7fd3057e2278>, <truth_tables.Person object at 0x7fd3057e2ac8>, <truth_tables.Person object at 0x7fd3057ebc18>, <truth_tables.Person object at 0x7fd3057f2b70>, <truth_tables.Person object at 0x7fd3057f2e48>, <truth_tables.Person object at 0x7fd3057f8198>, <truth_tables.Person object at 0x7fd3057f88d0>, <truth_tables.Person object at 0x7fd3057f8a58>, <truth_tables.Person object at 0x7fd3057f8c50>, <truth_tables.Person object at 0x7fd3057f8d68>, <truth_tables.Person object at 0x7fd305802048>, <truth_tables.Person object at 0x7fd3057879e8>, <truth_tables.Person object at 0x7fd305802eb8>, <truth_tables.Person object at 0x7fd305787d68>, <truth_tables.Person object at 0x7fd30578ca20>, <truth_tables.Person object at 0x7fd30578ce10>, <truth_tables.Person object at 0x7fd30578ccc0>, <truth_tables.Person object at 0x7fd30578cf98>, <truth_tables.Person object at 0x7fd3057934a8>, <truth_tables.Person object at 0x7fd305799588>, <truth_tables.Person object at 0x7fd305799d30>, <truth_tables.Person object at 0x7fd30579d160>, <truth_tables.Person object at 0x7fd3057a2470>, <truth_tables.Person object at 0x7fd3057a2a20>, <truth_tables.Person object at 0x7fd3057a2b00>, <truth_tables.Person object at 0x7fd3057a66d8>, <truth_tables.Person object at 0x7fd3057b3cc0>, <truth_tables.Person object at 0x7fd3057b3898>, <truth_tables.Person object at 0x7fd3057b84e0>, <truth_tables.Person object at 0x7fd3057be5f8>, <truth_tables.Person object at 0x7fd3057beb00>, <truth_tables.Person object at 0x7fd3057432b0>, <truth_tables.Person object at 0x7fd3057437b8>, <truth_tables.Person object at 0x7fd305743c50>, <truth_tables.Person object at 0x7fd305743ef0>, <truth_tables.Person object at 0x7fd30574f400>, <truth_tables.Person object at 0x7fd30574fe48>, <truth_tables.Person object at 0x7fd305759048>, <truth_tables.Person object at 0x7fd305759748>, <truth_tables.Person object at 0x7fd305760668>, <truth_tables.Person object at 0x7fd305759c88>, <truth_tables.Person object at 0x7fd305760ac8>, <truth_tables.Person object at 0x7fd305760978>, <truth_tables.Person object at 0x7fd3057605c0>, <truth_tables.Person object at 0x7fd30576b6a0>, <truth_tables.Person object at 0x7fd30576bbe0>, <truth_tables.Person object at 0x7fd305776048>, <truth_tables.Person object at 0x7fd305776320>, <truth_tables.Person object at 0x7fd305776898>, <truth_tables.Person object at 0x7fd305776fd0>, <truth_tables.Person object at 0x7fd30577b470>, <truth_tables.Person object at 0x7fd30577be48>, <truth_tables.Person object at 0x7fd3057813c8>, <truth_tables.Person object at 0x7fd30570ca20>, <truth_tables.Person object at 0x7fd3057116a0>, <truth_tables.Person object at 0x7fd305711ba8>, <truth_tables.Person object at 0x7fd305716860>, <truth_tables.Person object at 0x7fd305716128>, <truth_tables.Person object at 0x7fd305711978>, <truth_tables.Person object at 0x7fd305721400>, <truth_tables.Person object at 0x7fd30571de10>, <truth_tables.Person object at 0x7fd3057219e8>, <truth_tables.Person object at 0x7fd305721780>, <truth_tables.Person object at 0x7fd30571dcf8>, <truth_tables.Person object at 0x7fd30572c080>, <truth_tables.Person object at 0x7fd305728d30>, <truth_tables.Person object at 0x7fd30572ca90>, <truth_tables.Person object at 0x7fd3057327b8>, <truth_tables.Person object at 0x7fd305739908>, <truth_tables.Person object at 0x7fd30573e5c0>, <truth_tables.Person object at 0x7fd3056c40b8>, <truth_tables.Person object at 0x7fd3056c47f0>, <truth_tables.Person object at 0x7fd3056c80f0>, <truth_tables.Person object at 0x7fd3056c86d8>, <truth_tables.Person object at 0x7fd3056c8a20>, <truth_tables.Person object at 0x7fd3056c8f98>, <truth_tables.Person object at 0x7fd3056cf278>, <truth_tables.Person object at 0x7fd3056c8c88>, <truth_tables.Person object at 0x7fd3056cf160>, <truth_tables.Person object at 0x7fd3056c8fd0>, <truth_tables.Person object at 0x7fd3056d5780>, <truth_tables.Person object at 0x7fd3056da240>, <truth_tables.Person object at 0x7fd3056dab00>, <truth_tables.Person object at 0x7fd3056d5f98>, <truth_tables.Person object at 0x7fd3056df940>, <truth_tables.Person object at 0x7fd3056e4668>, <truth_tables.Person object at 0x7fd3056e47f0>, <truth_tables.Person object at 0x7fd3056e4c18>, <truth_tables.Person object at 0x7fd3056e4f98>, <truth_tables.Person object at 0x7fd3056e9da0>, <truth_tables.Person object at 0x7fd3056ef080>, <truth_tables.Person object at 0x7fd3056f4390>, <truth_tables.Person object at 0x7fd3056fa2b0>, <truth_tables.Person object at 0x7fd3056fa8d0>, <truth_tables.Person object at 0x7fd3056fafd0>, <truth_tables.Person object at 0x7fd3056facf8>, <truth_tables.Person object at 0x7fd3056fab38>, <truth_tables.Person object at 0x7fd305700eb8>, <truth_tables.Person object at 0x7fd3056facc0>, <truth_tables.Person object at 0x7fd305686780>, <truth_tables.Person object at 0x7fd305686e10>, <truth_tables.Person object at 0x7fd30568b7f0>, <truth_tables.Person object at 0x7fd30568f390>, <truth_tables.Person object at 0x7fd30568b400>, <truth_tables.Person object at 0x7fd3056957f0>, <truth_tables.Person object at 0x7fd30569a320>, <truth_tables.Person object at 0x7fd30569a748>, <truth_tables.Person object at 0x7fd30569a668>, <truth_tables.Person object at 0x7fd3056a1ba8>, <truth_tables.Person object at 0x7fd3056a1e80>, <truth_tables.Person object at 0x7fd3056a62b0>, <truth_tables.Person object at 0x7fd3056a60b8>, <truth_tables.Person object at 0x7fd3056ab198>, <truth_tables.Person object at 0x7fd3056ab438>, <truth_tables.Person object at 0x7fd3056abbe0>, <truth_tables.Person object at 0x7fd3056abfd0>, <truth_tables.Person object at 0x7fd3056b10f0>, <truth_tables.Person object at 0x7fd3056b1da0>, <truth_tables.Person object at 0x7fd3056be2b0>, <truth_tables.Person object at 0x7fd3056becf8>, <truth_tables.Person object at 0x7fd3056befd0>, <truth_tables.Person object at 0x7fd305643550>, <truth_tables.Person object at 0x7fd305649a90>, <truth_tables.Person object at 0x7fd305643438>, <truth_tables.Person object at 0x7fd305643f98>, <truth_tables.Person object at 0x7fd305643e10>, <truth_tables.Person object at 0x7fd3056497b8>, <truth_tables.Person object at 0x7fd30564d5f8>, <truth_tables.Person object at 0x7fd30564db38>, <truth_tables.Person object at 0x7fd30564d898>, <truth_tables.Person object at 0x7fd305654160>, <truth_tables.Person object at 0x7fd305654978>, <truth_tables.Person object at 0x7fd305659cf8>, <truth_tables.Person object at 0x7fd30565ef60>, <truth_tables.Person object at 0x7fd305664438>, <truth_tables.Person object at 0x7fd305668668>, <truth_tables.Person object at 0x7fd305668e48>, <truth_tables.Person object at 0x7fd30566e3c8>, <truth_tables.Person object at 0x7fd305668e80>, <truth_tables.Person object at 0x7fd305674a20>, <truth_tables.Person object at 0x7fd305674b70>, <truth_tables.Person object at 0x7fd305678b00>, <truth_tables.Person object at 0x7fd305678400>, <truth_tables.Person object at 0x7fd30567e2e8>, <truth_tables.Person object at 0x7fd30567e588>, <truth_tables.Person object at 0x7fd3056067f0>, <truth_tables.Person object at 0x7fd30560a080>, <truth_tables.Person object at 0x7fd30560ab00>, <truth_tables.Person object at 0x7fd30560a630>, <truth_tables.Person object at 0x7fd305615400>, <truth_tables.Person object at 0x7fd305615940>, <truth_tables.Person object at 0x7fd30561bba8>, <truth_tables.Person object at 0x7fd305621c50>, <truth_tables.Person object at 0x7fd3056216d8>, <truth_tables.Person object at 0x7fd3056256a0>, <truth_tables.Person object at 0x7fd30562d048>, <truth_tables.Person object at 0x7fd305625d68>, <truth_tables.Person object at 0x7fd30562d438>, <truth_tables.Person object at 0x7fd30562d898>, <truth_tables.Person object at 0x7fd305631630>, <truth_tables.Person object at 0x7fd305631748>, <truth_tables.Person object at 0x7fd305631ef0>, <truth_tables.Person object at 0x7fd305636c18>, <truth_tables.Person object at 0x7fd30563d2b0>, <truth_tables.Person object at 0x7fd30563d550>, <truth_tables.Person object at 0x7fd30563da20>, <truth_tables.Person object at 0x7fd30563dcc0>, <truth_tables.Person object at 0x7fd3056412b0>, <truth_tables.Person object at 0x7fd305641438>, <truth_tables.Person object at 0x7fd305641cc0>, <truth_tables.Person object at 0x7fd3055c9668>, <truth_tables.Person object at 0x7fd3055c9e80>, <truth_tables.Person object at 0x7fd3055cd048>, <truth_tables.Person object at 0x7fd3055cd588>, <truth_tables.Person object at 0x7fd3055d2198>, <truth_tables.Person object at 0x7fd3055d2710>, <truth_tables.Person object at 0x7fd3055d2a58>, <truth_tables.Person object at 0x7fd3055d2ba8>, <truth_tables.Person object at 0x7fd3055d2fd0>, <truth_tables.Person object at 0x7fd3055dc5c0>, <truth_tables.Person object at 0x7fd3055dcb70>, <truth_tables.Person object at 0x7fd3055dcda0>, <truth_tables.Person object at 0x7fd3055e27b8>, <truth_tables.Person object at 0x7fd3055e2b00>, <truth_tables.Person object at 0x7fd3055e7278>, <truth_tables.Person object at 0x7fd3055e7a58>, <truth_tables.Person object at 0x7fd3055e7ef0>, <truth_tables.Person object at 0x7fd3055ed550>, <truth_tables.Person object at 0x7fd3055ed3c8>, <truth_tables.Person object at 0x7fd3055edc18>, <truth_tables.Person object at 0x7fd3055f8668>, <truth_tables.Person object at 0x7fd3055f8cc0>, <truth_tables.Person object at 0x7fd3055fe400>, <truth_tables.Person object at 0x7fd3055fec18>, <truth_tables.Person object at 0x7fd3055feda0>, <truth_tables.Person object at 0x7fd305602588>, <truth_tables.Person object at 0x7fd305602c50>, <truth_tables.Person object at 0x7fd305602ef0>, <truth_tables.Person object at 0x7fd305602f28>, <truth_tables.Person object at 0x7fd305588588>, <truth_tables.Person object at 0x7fd3055886d8>, <truth_tables.Person object at 0x7fd30558c240>, <truth_tables.Person object at 0x7fd30558c4e0>, <truth_tables.Person object at 0x7fd30558c780>, <truth_tables.Person object at 0x7fd305592080>, <truth_tables.Person object at 0x7fd305592390>, <truth_tables.Person object at 0x7fd3055925f8>, <truth_tables.Person object at 0x7fd305592b70>, <truth_tables.Person object at 0x7fd3055970f0>, <truth_tables.Person object at 0x7fd3055975f8>, <truth_tables.Person object at 0x7fd305597748>, <truth_tables.Person object at 0x7fd3055a2208>, <truth_tables.Person object at 0x7fd3055a25c0>, <truth_tables.Person object at 0x7fd3055a83c8>, <truth_tables.Person object at 0x7fd3055a8320>, <truth_tables.Person object at 0x7fd3055a8908>, <truth_tables.Person object at 0x7fd3055a8f98>, <truth_tables.Person object at 0x7fd3055ad048>, <truth_tables.Person object at 0x7fd3055b42b0>, <truth_tables.Person object at 0x7fd3055b4588>, <truth_tables.Person object at 0x7fd3055b4668>, <truth_tables.Person object at 0x7fd3055b4e80>, <truth_tables.Person object at 0x7fd3055bf358>, <truth_tables.Person object at 0x7fd305543080>, <truth_tables.Person object at 0x7fd305543518>, <truth_tables.Person object at 0x7fd305543780>, <truth_tables.Person object at 0x7fd305543a20>, <truth_tables.Person object at 0x7fd305543c88>, <truth_tables.Person object at 0x7fd305548b00>, <truth_tables.Person object at 0x7fd30554ef98>, <truth_tables.Person object at 0x7fd30554eb00>, <truth_tables.Person object at 0x7fd305553320>, <truth_tables.Person object at 0x7fd30555b358>, <truth_tables.Person object at 0x7fd30555bb38>, <truth_tables.Person object at 0x7fd30555fe80>, <truth_tables.Person object at 0x7fd305565390>, <truth_tables.Person object at 0x7fd3055656a0>, <truth_tables.Person object at 0x7fd305565940>, <truth_tables.Person object at 0x7fd305569668>, <truth_tables.Person object at 0x7fd3055693c8>, <truth_tables.Person object at 0x7fd305569978>, <truth_tables.Person object at 0x7fd3055699b0>, <truth_tables.Person object at 0x7fd305569fd0>, <truth_tables.Person object at 0x7fd3055756d8>, <truth_tables.Person object at 0x7fd305569c50>, <truth_tables.Person object at 0x7fd305575978>, <truth_tables.Person object at 0x7fd305575c18>, <truth_tables.Person object at 0x7fd30557b2b0>, <truth_tables.Person object at 0x7fd30557b550>, <truth_tables.Person object at 0x7fd30557ba58>, <truth_tables.Person object at 0x7fd30557b940>, <truth_tables.Person object at 0x7fd3055812e8>, <truth_tables.Person object at 0x7fd305581e80>, <truth_tables.Person object at 0x7fd3055815c0>, <truth_tables.Person object at 0x7fd305507e48>, <truth_tables.Person object at 0x7fd30550b208>, <truth_tables.Person object at 0x7fd305507940>, <truth_tables.Person object at 0x7fd305507b70>, <truth_tables.Person object at 0x7fd305511208>, <truth_tables.Person object at 0x7fd305511518>, <truth_tables.Person object at 0x7fd305516048>, <truth_tables.Person object at 0x7fd305516390>, <truth_tables.Person object at 0x7fd30551da20>, <truth_tables.Person object at 0x7fd3055239e8>, <truth_tables.Person object at 0x7fd30551d7f0>, <truth_tables.Person object at 0x7fd305529358>, <truth_tables.Person object at 0x7fd305529d68>, <truth_tables.Person object at 0x7fd30552def0>, <truth_tables.Person object at 0x7fd3055395f8>, <truth_tables.Person object at 0x7fd305539d30>, <truth_tables.Person object at 0x7fd30553f278>, <truth_tables.Person object at 0x7fd30553ffd0>, <truth_tables.Person object at 0x7fd30553f7b8>, <truth_tables.Person object at 0x7fd3054c4ac8>, <truth_tables.Person object at 0x7fd3054c9908>, <truth_tables.Person object at 0x7fd3054cd9e8>, <truth_tables.Person object at 0x7fd3054cd780>, <truth_tables.Person object at 0x7fd3054d3a58>, <truth_tables.Person object at 0x7fd3054d9320>, <truth_tables.Person object at 0x7fd3054d3da0>, <truth_tables.Person object at 0x7fd3054d3d68>, <truth_tables.Person object at 0x7fd3054d9ef0>, <truth_tables.Person object at 0x7fd3054d9c50>, <truth_tables.Person object at 0x7fd3054e5828>, <truth_tables.Person object at 0x7fd3054e56d8>, <truth_tables.Person object at 0x7fd3054ea240>, <truth_tables.Person object at 0x7fd3054ea7f0>, <truth_tables.Person object at 0x7fd3054f0630>, <truth_tables.Person object at 0x7fd3054f6710>, <truth_tables.Person object at 0x7fd3054f6a58>, <truth_tables.Person object at 0x7fd3054fb2b0>, <truth_tables.Person object at 0x7fd3054f6d68>, <truth_tables.Person object at 0x7fd3054fbcc0>, <truth_tables.Person object at 0x7fd3054fbf28>, <truth_tables.Person object at 0x7fd305501550>, <truth_tables.Person object at 0x7fd305501d68>, <truth_tables.Person object at 0x7fd30548b390>, <truth_tables.Person object at 0x7fd30548be48>, <truth_tables.Person object at 0x7fd30548b780>, <truth_tables.Person object at 0x7fd3054954a8>, <truth_tables.Person object at 0x7fd305495cc0>, <truth_tables.Person object at 0x7fd305495e48>, <truth_tables.Person object at 0x7fd30549b748>, <truth_tables.Person object at 0x7fd3054a14e0>, <truth_tables.Person object at 0x7fd3054a1240>, <truth_tables.Person object at 0x7fd3054a1550>, <truth_tables.Person object at 0x7fd3054a84e0>, <truth_tables.Person object at 0x7fd3054a8630>, <truth_tables.Person object at 0x7fd3054a8e80>, <truth_tables.Person object at 0x7fd3054ad668>, <truth_tables.Person object at 0x7fd3054a8ef0>, <truth_tables.Person object at 0x7fd3054b2240>, <truth_tables.Person object at 0x7fd3054adf28>, <truth_tables.Person object at 0x7fd3054add30>, <truth_tables.Person object at 0x7fd3054b7278>, <truth_tables.Person object at 0x7fd3054b7390>, <truth_tables.Person object at 0x7fd3054b7b70>, <truth_tables.Person object at 0x7fd3054bc0f0>, <truth_tables.Person object at 0x7fd3054bccc0>, <truth_tables.Person object at 0x7fd3054bcfd0>, <truth_tables.Person object at 0x7fd3054c2e48>, <truth_tables.Person object at 0x7fd30544d198>, <truth_tables.Person object at 0x7fd3054521d0>, <truth_tables.Person object at 0x7fd305452358>, <truth_tables.Person object at 0x7fd305452b38>, <truth_tables.Person object at 0x7fd305452f60>, <truth_tables.Person object at 0x7fd305452d68>, <truth_tables.Person object at 0x7fd305456358>, <truth_tables.Person object at 0x7fd3054526d8>, <truth_tables.Person object at 0x7fd305456d30>, <truth_tables.Person object at 0x7fd30545f9b0>, <truth_tables.Person object at 0x7fd305464d68>, <truth_tables.Person object at 0x7fd305468d30>, <truth_tables.Person object at 0x7fd30546da20>, <truth_tables.Person object at 0x7fd305468da0>, <truth_tables.Person object at 0x7fd305468eb8>, <truth_tables.Person object at 0x7fd305472588>, <truth_tables.Person object at 0x7fd305472860>, <truth_tables.Person object at 0x7fd305479748>, <truth_tables.Person object at 0x7fd305479f60>, <truth_tables.Person object at 0x7fd30547e4a8>, <truth_tables.Person object at 0x7fd305403128>, <truth_tables.Person object at 0x7fd305409668>, <truth_tables.Person object at 0x7fd30540d6d8>, <truth_tables.Person object at 0x7fd305409c50>, <truth_tables.Person object at 0x7fd305409f28>, <truth_tables.Person object at 0x7fd30540d2e8>, <truth_tables.Person object at 0x7fd30540ddd8>, <truth_tables.Person object at 0x7fd3054136a0>, <truth_tables.Person object at 0x7fd305413d68>, <truth_tables.Person object at 0x7fd30541aba8>, <truth_tables.Person object at 0x7fd30541e358>, <truth_tables.Person object at 0x7fd3054236d8>, <truth_tables.Person object at 0x7fd30541efd0>, <truth_tables.Person object at 0x7fd30542a400>, <truth_tables.Person object at 0x7fd30542fdd8>, <truth_tables.Person object at 0x7fd3054366d8>, <truth_tables.Person object at 0x7fd305436b00>, <truth_tables.Person object at 0x7fd3054369b0>, <truth_tables.Person object at 0x7fd30543a710>, <truth_tables.Person object at 0x7fd30543fb00>, <truth_tables.Person object at 0x7fd30543fd68>, <truth_tables.Person object at 0x7fd3053c4668>, <truth_tables.Person object at 0x7fd30543fd30>, <truth_tables.Person object at 0x7fd3053ca160>, <truth_tables.Person object at 0x7fd3053c4898>, <truth_tables.Person object at 0x7fd3053ca6d8>, <truth_tables.Person object at 0x7fd3053d0eb8>, <truth_tables.Person object at 0x7fd3053d91d0>, <truth_tables.Person object at 0x7fd3053d9550>, <truth_tables.Person object at 0x7fd3053d9438>, <truth_tables.Person object at 0x7fd3053e66d8>, <truth_tables.Person object at 0x7fd3053dfa90>, <truth_tables.Person object at 0x7fd3053e6208>, <truth_tables.Person object at 0x7fd3053df6a0>, <truth_tables.Person object at 0x7fd3053df9b0>, <truth_tables.Person object at 0x7fd3053ef1d0>, <truth_tables.Person object at 0x7fd3053f8630>, <truth_tables.Person object at 0x7fd3053f8e80>, <truth_tables.Person object at 0x7fd3053fecc0>, <truth_tables.Person object at 0x7fd3053fefd0>, <truth_tables.Person object at 0x7fd305383a58>, <truth_tables.Person object at 0x7fd3053833c8>, <truth_tables.Person object at 0x7fd305383390>, <truth_tables.Person object at 0x7fd3053884e0>, <truth_tables.Person object at 0x7fd305388630>, <truth_tables.Person object at 0x7fd30538e9e8>, <truth_tables.Person object at 0x7fd30538ed30>, <truth_tables.Person object at 0x7fd30538ed68>, <truth_tables.Person object at 0x7fd305393b00>, <truth_tables.Person object at 0x7fd305398710>, <truth_tables.Person object at 0x7fd3053937b8>, <truth_tables.Person object at 0x7fd3053a4048>, <truth_tables.Person object at 0x7fd30539ec88>, <truth_tables.Person object at 0x7fd3053a9940>, <truth_tables.Person object at 0x7fd30539eb00>, <truth_tables.Person object at 0x7fd3053a95f8>, <truth_tables.Person object at 0x7fd3053ae710>, <truth_tables.Person object at 0x7fd3053aeef0>, <truth_tables.Person object at 0x7fd3053b56a0>, <truth_tables.Person object at 0x7fd3053ba828>, <truth_tables.Person object at 0x7fd3053ba550>, <truth_tables.Person object at 0x7fd3053bf710>, <truth_tables.Person object at 0x7fd3053451d0>, <truth_tables.Person object at 0x7fd305345358>, <truth_tables.Person object at 0x7fd305345f28>, <truth_tables.Person object at 0x7fd305345a20>, <truth_tables.Person object at 0x7fd305345c50>, <truth_tables.Person object at 0x7fd3053494a8>, <truth_tables.Person object at 0x7fd305349dd8>, <truth_tables.Person object at 0x7fd30534f0f0>, <truth_tables.Person object at 0x7fd30534f3c8>, <truth_tables.Person object at 0x7fd30534fac8>, <truth_tables.Person object at 0x7fd30534fbe0>, <truth_tables.Person object at 0x7fd305356128>, <truth_tables.Person object at 0x7fd3053566d8>, <truth_tables.Person object at 0x7fd30535b128>, <truth_tables.Person object at 0x7fd30535b550>, <truth_tables.Person object at 0x7fd3053611d0>, <truth_tables.Person object at 0x7fd305361978>, <truth_tables.Person object at 0x7fd305361e80>, <truth_tables.Person object at 0x7fd305365518>, <truth_tables.Person object at 0x7fd305365a90>, <truth_tables.Person object at 0x7fd30536a518>, <truth_tables.Person object at 0x7fd305365978>, <truth_tables.Person object at 0x7fd30536a470>, <truth_tables.Person object at 0x7fd305371080>, <truth_tables.Person object at 0x7fd305371828>, <truth_tables.Person object at 0x7fd3053762e8>, <truth_tables.Person object at 0x7fd305371f60>, <truth_tables.Person object at 0x7fd305371d30>, <truth_tables.Person object at 0x7fd305376a58>, <truth_tables.Person object at 0x7fd305376e80>, <truth_tables.Person object at 0x7fd30537bba8>, <truth_tables.Person object at 0x7fd30537b978>, <truth_tables.Person object at 0x7fd30537f6d8>, <truth_tables.Person object at 0x7fd30537fc50>, <truth_tables.Person object at 0x7fd30537fac8>, <truth_tables.Person object at 0x7fd30537fbe0>, <truth_tables.Person object at 0x7fd305305d30>, <truth_tables.Person object at 0x7fd30537fc18>, <truth_tables.Person object at 0x7fd305305dd8>, <truth_tables.Person object at 0x7fd305311cc0>, <truth_tables.Person object at 0x7fd3053189b0>, <truth_tables.Person object at 0x7fd305318ef0>, <truth_tables.Person object at 0x7fd30531ccc0>, <truth_tables.Person object at 0x7fd305318dd8>, <truth_tables.Person object at 0x7fd30531c9b0>, <truth_tables.Person object at 0x7fd30531cfd0>, <truth_tables.Person object at 0x7fd305323320>, <truth_tables.Person object at 0x7fd305323898>, <truth_tables.Person object at 0x7fd305323b70>, <truth_tables.Person object at 0x7fd305323f28>, <truth_tables.Person object at 0x7fd305329358>, <truth_tables.Person object at 0x7fd305329a58>, <truth_tables.Person object at 0x7fd30532d630>, <truth_tables.Person object at 0x7fd305332160>, <truth_tables.Person object at 0x7fd305332da0>, <truth_tables.Person object at 0x7fd305332fd0>, <truth_tables.Person object at 0x7fd305337908>, <truth_tables.Person object at 0x7fd305337b38>, <truth_tables.Person object at 0x7fd305337cf8>, <truth_tables.Person object at 0x7fd30533d710>, <truth_tables.Person object at 0x7fd30533d4e0>, <truth_tables.Person object at 0x7fd3052c40b8>, <truth_tables.Person object at 0x7fd3052c45f8>, <truth_tables.Person object at 0x7fd3052c4470>, <truth_tables.Person object at 0x7fd3052c4ba8>, <truth_tables.Person object at 0x7fd3052c4cf8>, <truth_tables.Person object at 0x7fd3052c86d8>, <truth_tables.Person object at 0x7fd3052c8a20>, <truth_tables.Person object at 0x7fd3052cd748>, <truth_tables.Person object at 0x7fd3052cdef0>, <truth_tables.Person object at 0x7fd3052d30b8>, <truth_tables.Person object at 0x7fd3052d3860>, <truth_tables.Person object at 0x7fd3052d33c8>, <truth_tables.Person object at 0x7fd3052d3358>, <truth_tables.Person object at 0x7fd3052d3d30>, <truth_tables.Person object at 0x7fd3052d3fd0>, <truth_tables.Person object at 0x7fd3052d8c50>, <truth_tables.Person object at 0x7fd3052df1d0>, <truth_tables.Person object at 0x7fd3052d8978>, <truth_tables.Person object at 0x7fd3052d8780>, <truth_tables.Person object at 0x7fd3052df860>, <truth_tables.Person object at 0x7fd3052dfba8>, <truth_tables.Person object at 0x7fd3052e41d0>, <truth_tables.Person object at 0x7fd3052e4828>, <truth_tables.Person object at 0x7fd3052e8b70>, <truth_tables.Person object at 0x7fd3052ed4a8>, <truth_tables.Person object at 0x7fd3052f22b0>, <truth_tables.Person object at 0x7fd3052f2e48>, <truth_tables.Person object at 0x7fd3052f8390>, <truth_tables.Person object at 0x7fd3052f8d68>, <truth_tables.Person object at 0x7fd305283b00>, <truth_tables.Person object at 0x7fd3052890f0>, <truth_tables.Person object at 0x7fd305289400>, <truth_tables.Person object at 0x7fd3052896d8>, <truth_tables.Person object at 0x7fd305289a20>, <truth_tables.Person object at 0x7fd30528e278>, <truth_tables.Person object at 0x7fd30528e908>, <truth_tables.Person object at 0x7fd30528ee80>, <truth_tables.Person object at 0x7fd305295ef0>, <truth_tables.Person object at 0x7fd305295c50>, <truth_tables.Person object at 0x7fd305299470>, <truth_tables.Person object at 0x7fd3052997f0>, <truth_tables.Person object at 0x7fd30529f320>, <truth_tables.Person object at 0x7fd30529fef0>, <truth_tables.Person object at 0x7fd30529fc50>, <truth_tables.Person object at 0x7fd30529fa58>, <truth_tables.Person object at 0x7fd3052a46a0>, <truth_tables.Person object at 0x7fd3052a4e80>, <truth_tables.Person object at 0x7fd3052a4eb8>, <truth_tables.Person object at 0x7fd3052a4f98>, <truth_tables.Person object at 0x7fd3052aa9b0>, <truth_tables.Person object at 0x7fd3052b0550>, <truth_tables.Person object at 0x7fd3052b44e0>, <truth_tables.Person object at 0x7fd3052b4d68>, <truth_tables.Person object at 0x7fd3052ba160>, <truth_tables.Person object at 0x7fd3052c09e8>, <truth_tables.Person object at 0x7fd3052c0f28>, <truth_tables.Person object at 0x7fd30524a198>, <truth_tables.Person object at 0x7fd30524a5c0>, <truth_tables.Person object at 0x7fd305244c50>, <truth_tables.Person object at 0x7fd30524ac50>, <truth_tables.Person object at 0x7fd305250128>, <truth_tables.Person object at 0x7fd3052503c8>, <truth_tables.Person object at 0x7fd305250828>, <truth_tables.Person object at 0x7fd3052566a0>, <truth_tables.Person object at 0x7fd305256b00>, <truth_tables.Person object at 0x7fd30525aac8>, <truth_tables.Person object at 0x7fd30525fc18>, <truth_tables.Person object at 0x7fd30525feb8>, <truth_tables.Person object at 0x7fd305266668>, <truth_tables.Person object at 0x7fd305266dd8>, <truth_tables.Person object at 0x7fd305266b00>, <truth_tables.Person object at 0x7fd30526c908>, <truth_tables.Person object at 0x7fd305272240>, <truth_tables.Person object at 0x7fd30526cef0>, <truth_tables.Person object at 0x7fd305272748>, <truth_tables.Person object at 0x7fd305276908>, <truth_tables.Person object at 0x7fd305276be0>, <truth_tables.Person object at 0x7fd305276eb8>, <truth_tables.Person object at 0x7fd30527bc88>, <truth_tables.Person object at 0x7fd30527bda0>, <truth_tables.Person object at 0x7fd3052820f0>, <truth_tables.Person object at 0x7fd305282a20>, <truth_tables.Person object at 0x7fd305282c50>, <truth_tables.Person object at 0x7fd305282f28>, <truth_tables.Person object at 0x7fd305208fd0>, <truth_tables.Person object at 0x7fd30520e470>, <truth_tables.Person object at 0x7fd305213128>, <truth_tables.Person object at 0x7fd305213e48>, <truth_tables.Person object at 0x7fd3052193c8>, <truth_tables.Person object at 0x7fd30521e7b8>, <truth_tables.Person object at 0x7fd30521ea58>, <truth_tables.Person object at 0x7fd305223128>, <truth_tables.Person object at 0x7fd305223898>, <truth_tables.Person object at 0x7fd305228320>, <truth_tables.Person object at 0x7fd30522f6a0>, <truth_tables.Person object at 0x7fd30522f550>, <truth_tables.Person object at 0x7fd3052346a0>, <truth_tables.Person object at 0x7fd305234f98>, <truth_tables.Person object at 0x7fd30523afd0>, <truth_tables.Person object at 0x7fd30523a630>, <truth_tables.Person object at 0x7fd3051c34a8>, <truth_tables.Person object at 0x7fd3051c3898>, <truth_tables.Person object at 0x7fd3051c3cf8>, <truth_tables.Person object at 0x7fd3051c3fd0>, <truth_tables.Person object at 0x7fd3051c8320>, <truth_tables.Person object at 0x7fd3051c8550>, <truth_tables.Person object at 0x7fd3051c87f0>, <truth_tables.Person object at 0x7fd3051c8da0>, <truth_tables.Person object at 0x7fd3051ce198>, <truth_tables.Person object at 0x7fd3051ce940>, <truth_tables.Person object at 0x7fd3051cebe0>, <truth_tables.Person object at 0x7fd3051cef60>, <truth_tables.Person object at 0x7fd3051d6a20>, <truth_tables.Person object at 0x7fd3051d6ba8>, <truth_tables.Person object at 0x7fd3051db400>, <truth_tables.Person object at 0x7fd3051df320>, <truth_tables.Person object at 0x7fd3051dbeb8>, <truth_tables.Person object at 0x7fd3051dfa58>, <truth_tables.Person object at 0x7fd3051e5978>, <truth_tables.Person object at 0x7fd3051e53c8>, <truth_tables.Person object at 0x7fd3051e5e48>, <truth_tables.Person object at 0x7fd3051e5be0>, <truth_tables.Person object at 0x7fd3051e54e0>, <truth_tables.Person object at 0x7fd3051f10f0>, <truth_tables.Person object at 0x7fd3051f1518>, <truth_tables.Person object at 0x7fd3051f63c8>, <truth_tables.Person object at 0x7fd3051fa160>, <truth_tables.Person object at 0x7fd3051fa4e0>, <truth_tables.Person object at 0x7fd3052022b0>, <truth_tables.Person object at 0x7fd305202198>, <truth_tables.Person object at 0x7fd305202748>, <truth_tables.Person object at 0x7fd30518ddd8>, <truth_tables.Person object at 0x7fd3051926d8>, <truth_tables.Person object at 0x7fd305197470>, <truth_tables.Person object at 0x7fd3051976d8>, <truth_tables.Person object at 0x7fd305197f60>, <truth_tables.Person object at 0x7fd30519d710>, <truth_tables.Person object at 0x7fd30519da58>, <truth_tables.Person object at 0x7fd3051a2080>, <truth_tables.Person object at 0x7fd3051a2f28>, <truth_tables.Person object at 0x7fd3051a27b8>, <truth_tables.Person object at 0x7fd3051a70b8>, <truth_tables.Person object at 0x7fd3051a7c18>, <truth_tables.Person object at 0x7fd3051a7ac8>, <truth_tables.Person object at 0x7fd3051b1278>, <truth_tables.Person object at 0x7fd3051ac668>, <truth_tables.Person object at 0x7fd3051b7358>, <truth_tables.Person object at 0x7fd3051b7668>, <truth_tables.Person object at 0x7fd3051b77f0>, <truth_tables.Person object at 0x7fd3051bb4e0>, <truth_tables.Person object at 0x7fd3051bb198>, <truth_tables.Person object at 0x7fd3051c2470>, <truth_tables.Person object at 0x7fd3051c26d8>, <truth_tables.Person object at 0x7fd305146208>, <truth_tables.Person object at 0x7fd305146518>, <truth_tables.Person object at 0x7fd305146630>, <truth_tables.Person object at 0x7fd30514ca90>, <truth_tables.Person object at 0x7fd305151438>, <truth_tables.Person object at 0x7fd305151c88>, <truth_tables.Person object at 0x7fd3051575c0>, <truth_tables.Person object at 0x7fd305157b00>, <truth_tables.Person object at 0x7fd305157e48>, <truth_tables.Person object at 0x7fd30515eb00>, <truth_tables.Person object at 0x7fd3051611d0>, <truth_tables.Person object at 0x7fd30515e208>, <truth_tables.Person object at 0x7fd305161588>, <truth_tables.Person object at 0x7fd3051677f0>, <truth_tables.Person object at 0x7fd305167eb8>, <truth_tables.Person object at 0x7fd305167d68>, <truth_tables.Person object at 0x7fd30516c5c0>, <truth_tables.Person object at 0x7fd30516ce48>, <truth_tables.Person object at 0x7fd305171c50>, <truth_tables.Person object at 0x7fd30516cd68>, <truth_tables.Person object at 0x7fd30517d358>, <truth_tables.Person object at 0x7fd30517d780>, <truth_tables.Person object at 0x7fd30517dba8>, <truth_tables.Person object at 0x7fd305104160>, <truth_tables.Person object at 0x7fd305104898>, <truth_tables.Person object at 0x7fd305104ef0>, <truth_tables.Person object at 0x7fd305104b38>, <truth_tables.Person object at 0x7fd3051086a0>, <truth_tables.Person object at 0x7fd30510e080>, <truth_tables.Person object at 0x7fd305108b38>, <truth_tables.Person object at 0x7fd30510ebe0>, <truth_tables.Person object at 0x7fd3051147b8>, <truth_tables.Person object at 0x7fd305114940>, <truth_tables.Person object at 0x7fd305114d30>, <truth_tables.Person object at 0x7fd305119a90>, <truth_tables.Person object at 0x7fd305119d30>, <truth_tables.Person object at 0x7fd30511ef98>, <truth_tables.Person object at 0x7fd30511ec18>, <truth_tables.Person object at 0x7fd30511e7b8>, <truth_tables.Person object at 0x7fd30512a2e8>, <truth_tables.Person object at 0x7fd30512ac88>, <truth_tables.Person object at 0x7fd30512af28>, <truth_tables.Person object at 0x7fd30512f828>, <truth_tables.Person object at 0x7fd30512f9b0>, <truth_tables.Person object at 0x7fd30512fcc0>, <truth_tables.Person object at 0x7fd305132160>, <truth_tables.Person object at 0x7fd305132828>, <truth_tables.Person object at 0x7fd305132b38>, <truth_tables.Person object at 0x7fd30513a128>, <truth_tables.Person object at 0x7fd30513ab38>, <truth_tables.Person object at 0x7fd30513aa58>, <truth_tables.Person object at 0x7fd30513e710>, <truth_tables.Person object at 0x7fd3050cb0b8>, <truth_tables.Person object at 0x7fd3050cb860>, <truth_tables.Person object at 0x7fd3050d52e8>, <truth_tables.Person object at 0x7fd3050d0320>, <truth_tables.Person object at 0x7fd3050d5668>, <truth_tables.Person object at 0x7fd3050d5f60>, <truth_tables.Person object at 0x7fd3050e0208>, <truth_tables.Person object at 0x7fd3050d9860>, <truth_tables.Person object at 0x7fd3050e64a8>, <truth_tables.Person object at 0x7fd3050e0978>, <truth_tables.Person object at 0x7fd3050e09b0>, <truth_tables.Person object at 0x7fd3050e67f0>, <truth_tables.Person object at 0x7fd3050eb1d0>, <truth_tables.Person object at 0x7fd3050e6860>, <truth_tables.Person object at 0x7fd3050eba58>, <truth_tables.Person object at 0x7fd3050f02e8>, <truth_tables.Person object at 0x7fd3050f0668>, <truth_tables.Person object at 0x7fd3050f0c18>, <truth_tables.Person object at 0x7fd3050f0940>, <truth_tables.Person object at 0x7fd3050f5358>, <truth_tables.Person object at 0x7fd3050f5b70>, <truth_tables.Person object at 0x7fd3050fb358>, <truth_tables.Person object at 0x7fd3050fb630>, <truth_tables.Person object at 0x7fd305100b38>, <truth_tables.Person object at 0x7fd305087160>, <truth_tables.Person object at 0x7fd305087668>, <truth_tables.Person object at 0x7fd305087e48>, <truth_tables.Person object at 0x7fd3050903c8>, <truth_tables.Person object at 0x7fd305090518>, <truth_tables.Person object at 0x7fd305090ba8>, <truth_tables.Person object at 0x7fd305096828>, <truth_tables.Person object at 0x7fd3050961d0>, <truth_tables.Person object at 0x7fd3050964a8>, <truth_tables.Person object at 0x7fd305096f28>, <truth_tables.Person object at 0x7fd30509c2b0>, <truth_tables.Person object at 0x7fd30509c710>, <truth_tables.Person object at 0x7fd3050a2390>, <truth_tables.Person object at 0x7fd30509c588>, <truth_tables.Person object at 0x7fd3050a2dd8>, <truth_tables.Person object at 0x7fd3050a8080>, <truth_tables.Person object at 0x7fd3050a8390>, <truth_tables.Person object at 0x7fd3050a8898>, <truth_tables.Person object at 0x7fd3050a8f60>, <truth_tables.Person object at 0x7fd3050a8ac8>, <truth_tables.Person object at 0x7fd3050ac320>, <truth_tables.Person object at 0x7fd3050b3828>, <truth_tables.Person object at 0x7fd3050b3ac8>, <truth_tables.Person object at 0x7fd3050b8048>, <truth_tables.Person object at 0x7fd3050b3d68>, <truth_tables.Person object at 0x7fd3050b8dd8>, <truth_tables.Person object at 0x7fd3050bc400>, <truth_tables.Person object at 0x7fd3050bc588>, <truth_tables.Person object at 0x7fd3050c2198>, <truth_tables.Person object at 0x7fd3050c24e0>, <truth_tables.Person object at 0x7fd3050c2ac8>, <truth_tables.Person object at 0x7fd3050c2d68>, <truth_tables.Person object at 0x7fd3050c2b00>, <truth_tables.Person object at 0x7fd305048a58>, <truth_tables.Person object at 0x7fd30504c080>, <truth_tables.Person object at 0x7fd30504c668>, <truth_tables.Person object at 0x7fd30504cbe0>, <truth_tables.Person object at 0x7fd30504ce80>, <truth_tables.Person object at 0x7fd305053780>, <truth_tables.Person object at 0x7fd30504ceb8>, <truth_tables.Person object at 0x7fd305053240>, <truth_tables.Person object at 0x7fd30505dac8>, <truth_tables.Person object at 0x7fd30505d6d8>, <truth_tables.Person object at 0x7fd30505de10>, <truth_tables.Person object at 0x7fd305064668>, <truth_tables.Person object at 0x7fd305064be0>, <truth_tables.Person object at 0x7fd305064a58>, <truth_tables.Person object at 0x7fd305069160>, <truth_tables.Person object at 0x7fd30506f0f0>, <truth_tables.Person object at 0x7fd30506f8d0>, <truth_tables.Person object at 0x7fd30506fa20>, <truth_tables.Person object at 0x7fd3050756a0>, <truth_tables.Person object at 0x7fd305075be0>, <truth_tables.Person object at 0x7fd30507a588>, <truth_tables.Person object at 0x7fd30507ab70>, <truth_tables.Person object at 0x7fd30507abe0>, <truth_tables.Person object at 0x7fd305080780>, <truth_tables.Person object at 0x7fd305080dd8>, <truth_tables.Person object at 0x7fd305080e10>, <truth_tables.Person object at 0x7fd30500afd0>, <truth_tables.Person object at 0x7fd30500f128>, <truth_tables.Person object at 0x7fd30500fe80>, <truth_tables.Person object at 0x7fd30500f9b0>, <truth_tables.Person object at 0x7fd3050159b0>, <truth_tables.Person object at 0x7fd30501a2b0>, <truth_tables.Person object at 0x7fd305015ac8>, <truth_tables.Person object at 0x7fd305021898>, <truth_tables.Person object at 0x7fd3050217b8>, <truth_tables.Person object at 0x7fd305026358>, <truth_tables.Person object at 0x7fd305026f98>, <truth_tables.Person object at 0x7fd305026fd0>, <truth_tables.Person object at 0x7fd30502ab00>, <truth_tables.Person object at 0x7fd305030f28>, <truth_tables.Person object at 0x7fd305030f60>, <truth_tables.Person object at 0x7fd305035390>, <truth_tables.Person object at 0x7fd30503beb8>, <truth_tables.Person object at 0x7fd3050402e8>, <truth_tables.Person object at 0x7fd305040748>, <truth_tables.Person object at 0x7fd304fc6b00>, <truth_tables.Person object at 0x7fd305040f98>, <truth_tables.Person object at 0x7fd304fc6e10>, <truth_tables.Person object at 0x7fd304fc6e80>, <truth_tables.Person object at 0x7fd304fcc4a8>, <truth_tables.Person object at 0x7fd304fcc7f0>, <truth_tables.Person object at 0x7fd304fcc978>, <truth_tables.Person object at 0x7fd304fcce48>, <truth_tables.Person object at 0x7fd304fd1400>, <truth_tables.Person object at 0x7fd304fd7630>, <truth_tables.Person object at 0x7fd304fde198>, <truth_tables.Person object at 0x7fd304fde4a8>, <truth_tables.Person object at 0x7fd304fdec18>, <truth_tables.Person object at 0x7fd304fde978>, <truth_tables.Person object at 0x7fd304fe8908>, <truth_tables.Person object at 0x7fd304fedbe0>, <truth_tables.Person object at 0x7fd304fedc50>, <truth_tables.Person object at 0x7fd304ff3518>, <truth_tables.Person object at 0x7fd304ff3fd0>, <truth_tables.Person object at 0x7fd305002128>, <truth_tables.Person object at 0x7fd3050023c8>, <truth_tables.Person object at 0x7fd305002518>, <truth_tables.Person object at 0x7fd305002cf8>, <truth_tables.Person object at 0x7fd3050029e8>, <truth_tables.Person object at 0x7fd304f872e8>, <truth_tables.Person object at 0x7fd304f8d5c0>, <truth_tables.Person object at 0x7fd304f8d8d0>, <truth_tables.Person object at 0x7fd304f8d940>, <truth_tables.Person object at 0x7fd304f996d8>, <truth_tables.Person object at 0x7fd304f92d68>, <truth_tables.Person object at 0x7fd304f993c8>, <truth_tables.Person object at 0x7fd304f9da90>, <truth_tables.Person object at 0x7fd304fa39e8>, <truth_tables.Person object at 0x7fd304fa3320>, <truth_tables.Person object at 0x7fd304fa34e0>, <truth_tables.Person object at 0x7fd304fa3f60>, <truth_tables.Person object at 0x7fd304fa9128>, <truth_tables.Person object at 0x7fd304faee80>, <truth_tables.Person object at 0x7fd304faeba8>, <truth_tables.Person object at 0x7fd304fb4400>, <truth_tables.Person object at 0x7fd304fbe5f8>, <truth_tables.Person object at 0x7fd304f450f0>, <truth_tables.Person object at 0x7fd304f45550>, <truth_tables.Person object at 0x7fd304f45828>, <truth_tables.Person object at 0x7fd304f45ef0>, <truth_tables.Person object at 0x7fd304f45c88>, <truth_tables.Person object at 0x7fd304f45fd0>, <truth_tables.Person object at 0x7fd304f499e8>, <truth_tables.Person object at 0x7fd304f4fe10>, <truth_tables.Person object at 0x7fd304f56b00>, <truth_tables.Person object at 0x7fd304f56da0>, <truth_tables.Person object at 0x7fd304f5a630>, <truth_tables.Person object at 0x7fd304f5abe0>, <truth_tables.Person object at 0x7fd304f61198>, <truth_tables.Person object at 0x7fd304f61e80>, <truth_tables.Person object at 0x7fd304f655c0>, <truth_tables.Person object at 0x7fd304f65588>, <truth_tables.Person object at 0x7fd304f65898>, <truth_tables.Person object at 0x7fd304f6b828>, <truth_tables.Person object at 0x7fd304f6b4a8>, <truth_tables.Person object at 0x7fd304f6b390>, <truth_tables.Person object at 0x7fd304f70048>, <truth_tables.Person object at 0x7fd304f702e8>, <truth_tables.Person object at 0x7fd304f70b00>, <truth_tables.Person object at 0x7fd304f752e8>, <truth_tables.Person object at 0x7fd304f70d68>, <truth_tables.Person object at 0x7fd304f75e48>, <truth_tables.Person object at 0x7fd304f75f98>, <truth_tables.Person object at 0x7fd304f7bb00>, <truth_tables.Person object at 0x7fd304f81940>, <truth_tables.Person object at 0x7fd304f81e80>, <truth_tables.Person object at 0x7fd304f81fd0>, <truth_tables.Person object at 0x7fd304f05828>, <truth_tables.Person object at 0x7fd304f05438>, <truth_tables.Person object at 0x7fd304f05f28>, <truth_tables.Person object at 0x7fd304f05ef0>, <truth_tables.Person object at 0x7fd304f0c128>, <truth_tables.Person object at 0x7fd304f0cfd0>, <truth_tables.Person object at 0x7fd304f12198>, <truth_tables.Person object at 0x7fd304f125c0>, <truth_tables.Person object at 0x7fd304f12a90>, <truth_tables.Person object at 0x7fd304f12c18>, <truth_tables.Person object at 0x7fd304f176a0>, <truth_tables.Person object at 0x7fd304f1d9b0>, <truth_tables.Person object at 0x7fd304f1dc18>, <truth_tables.Person object at 0x7fd304f21a20>, <truth_tables.Person object at 0x7fd304f27198>, <truth_tables.Person object at 0x7fd304f27438>, <truth_tables.Person object at 0x7fd304f277f0>, <truth_tables.Person object at 0x7fd304f27cc0>, <truth_tables.Person object at 0x7fd304f2dcf8>, <truth_tables.Person object at 0x7fd304f32e48>, <truth_tables.Person object at 0x7fd304f398d0>, <truth_tables.Person object at 0x7fd304f3f3c8>, <truth_tables.Person object at 0x7fd304f3fb70>, <truth_tables.Person object at 0x7fd304f3ff98>, <truth_tables.Person object at 0x7fd304ec5cf8>, <truth_tables.Person object at 0x7fd304ecae10>, <truth_tables.Person object at 0x7fd304eca4a8>, <truth_tables.Person object at 0x7fd304ecf8d0>, <truth_tables.Person object at 0x7fd304ecfe10>, <truth_tables.Person object at 0x7fd304ed72b0>, <truth_tables.Person object at 0x7fd304ed7828>, <truth_tables.Person object at 0x7fd304eda0b8>, <truth_tables.Person object at 0x7fd304ed7b00>, <truth_tables.Person object at 0x7fd304ed7ef0>, <truth_tables.Person object at 0x7fd304ee0748>, <truth_tables.Person object at 0x7fd304ee09b0>, <truth_tables.Person object at 0x7fd304ee0ac8>, <truth_tables.Person object at 0x7fd304ee71d0>, <truth_tables.Person object at 0x7fd304ee76a0>, <truth_tables.Person object at 0x7fd304ee0f60>, <truth_tables.Person object at 0x7fd304ee7d68>, <truth_tables.Person object at 0x7fd304eec390>, <truth_tables.Person object at 0x7fd304ef1278>, <truth_tables.Person object at 0x7fd304ef1588>, <truth_tables.Person object at 0x7fd304ef1908>, <truth_tables.Person object at 0x7fd304ef6518>, <truth_tables.Person object at 0x7fd304efc320>, <truth_tables.Person object at 0x7fd304efc5c0>, <truth_tables.Person object at 0x7fd304efc710>, <truth_tables.Person object at 0x7fd304f01358>, <truth_tables.Person object at 0x7fd304efcfd0>, <truth_tables.Person object at 0x7fd304f01780>, <truth_tables.Person object at 0x7fd304e86eb8>, <truth_tables.Person object at 0x7fd304e86c88>, <truth_tables.Person object at 0x7fd304e86cc0>, <truth_tables.Person object at 0x7fd304e90940>, <truth_tables.Person object at 0x7fd304e90c18>, <truth_tables.Person object at 0x7fd304e97160>, <truth_tables.Person object at 0x7fd304e9d710>, <truth_tables.Person object at 0x7fd304e9dc88>, <truth_tables.Person object at 0x7fd304ea2710>, <truth_tables.Person object at 0x7fd304ea2438>, <truth_tables.Person object at 0x7fd304ea84e0>, <truth_tables.Person object at 0x7fd304ea8a58>, <truth_tables.Person object at 0x7fd304ea8ba8>, <truth_tables.Person object at 0x7fd304eac278>, <truth_tables.Person object at 0x7fd304eacef0>, <truth_tables.Person object at 0x7fd304eaccc0>, <truth_tables.Person object at 0x7fd304eba3c8>, <truth_tables.Person object at 0x7fd304eb3e48>, <truth_tables.Person object at 0x7fd304ebe278>, <truth_tables.Person object at 0x7fd304ebe4e0>, <truth_tables.Person object at 0x7fd304ebe828>, <truth_tables.Person object at 0x7fd304e48278>, <truth_tables.Person object at 0x7fd304e48518>, <truth_tables.Person object at 0x7fd304e48cf8>, <truth_tables.Person object at 0x7fd304e4f208>, <truth_tables.Person object at 0x7fd304e55080>, <truth_tables.Person object at 0x7fd304e55208>, <truth_tables.Person object at 0x7fd304e55668>, <truth_tables.Person object at 0x7fd304e55ef0>, <truth_tables.Person object at 0x7fd304e5a3c8>, <truth_tables.Person object at 0x7fd304e5af60>, <truth_tables.Person object at 0x7fd304e60c50>, <truth_tables.Person object at 0x7fd304e64ba8>, <truth_tables.Person object at 0x7fd304e64e48>, <truth_tables.Person object at 0x7fd304e6a198>, <truth_tables.Person object at 0x7fd304e71160>, <truth_tables.Person object at 0x7fd304e71668>, <truth_tables.Person object at 0x7fd304e71da0>, <truth_tables.Person object at 0x7fd304e756d8>, <truth_tables.Person object at 0x7fd304e75fd0>, <truth_tables.Person object at 0x7fd304e75940>, <truth_tables.Person object at 0x7fd304e7bcc0>, <truth_tables.Person object at 0x7fd304e7b9b0>, <truth_tables.Person object at 0x7fd304e7f6a0>, <truth_tables.Person object at 0x7fd304e7fbe0>, <truth_tables.Person object at 0x7fd304e05048>, <truth_tables.Person object at 0x7fd304e054e0>, <truth_tables.Person object at 0x7fd304e059b0>, <truth_tables.Person object at 0x7fd304e0b3c8>, <truth_tables.Person object at 0x7fd304e11e10>, <truth_tables.Person object at 0x7fd304e160f0>, <truth_tables.Person object at 0x7fd304e16240>, <truth_tables.Person object at 0x7fd304e16ac8>, <truth_tables.Person object at 0x7fd304e165c0>, <truth_tables.Person object at 0x7fd304e1a2e8>, <truth_tables.Person object at 0x7fd304e1a5f8>, <truth_tables.Person object at 0x7fd304e1a780>, <truth_tables.Person object at 0x7fd304e1af60>, <truth_tables.Person object at 0x7fd304e23630>, <truth_tables.Person object at 0x7fd304e2f588>, <truth_tables.Person object at 0x7fd304e3a1d0>, <truth_tables.Person object at 0x7fd304e34be0>, <truth_tables.Person object at 0x7fd304e3ae48>, <truth_tables.Person object at 0x7fd304e3ee80>, <truth_tables.Person object at 0x7fd304dc4198>, <truth_tables.Person object at 0x7fd304e3e898>, <truth_tables.Person object at 0x7fd304dc4780>, <truth_tables.Person object at 0x7fd304dc45f8>, <truth_tables.Person object at 0x7fd304dd0400>, <truth_tables.Person object at 0x7fd304dcbcf8>, <truth_tables.Person object at 0x7fd304dd0550>, <truth_tables.Person object at 0x7fd304dd0908>, <truth_tables.Person object at 0x7fd304dd0780>, <truth_tables.Person object at 0x7fd304dd5ac8>, <truth_tables.Person object at 0x7fd304dd5390>, <truth_tables.Person object at 0x7fd304dd9358>, <truth_tables.Person object at 0x7fd304de0ac8>, <truth_tables.Person object at 0x7fd304de7630>, <truth_tables.Person object at 0x7fd304deb748>, <truth_tables.Person object at 0x7fd304debf60>, <truth_tables.Person object at 0x7fd304df0240>, <truth_tables.Person object at 0x7fd304df0518>, <truth_tables.Person object at 0x7fd304df5080>, <truth_tables.Person object at 0x7fd304df5898>, <truth_tables.Person object at 0x7fd304df5ba8>, <truth_tables.Person object at 0x7fd304dfb438>, <truth_tables.Person object at 0x7fd304dfb278>, <truth_tables.Person object at 0x7fd304dfbf60>, <truth_tables.Person object at 0x7fd304d83828>, <truth_tables.Person object at 0x7fd304d83cc0>, <truth_tables.Person object at 0x7fd304d880b8>, <truth_tables.Person object at 0x7fd304d88fd0>, <truth_tables.Person object at 0x7fd304d88d30>, <truth_tables.Person object at 0x7fd304d8c390>, <truth_tables.Person object at 0x7fd304d8cba8>, <truth_tables.Person object at 0x7fd304d8c630>, <truth_tables.Person object at 0x7fd304d93198>, <truth_tables.Person object at 0x7fd304d8ce10>, <truth_tables.Person object at 0x7fd304d934e0>, <truth_tables.Person object at 0x7fd304d93a20>, <truth_tables.Person object at 0x7fd304d935c0>, <truth_tables.Person object at 0x7fd304d98240>, <truth_tables.Person object at 0x7fd304d98390>, <truth_tables.Person object at 0x7fd304d9dc18>, <truth_tables.Person object at 0x7fd304da3320>, <truth_tables.Person object at 0x7fd304da8198>, <truth_tables.Person object at 0x7fd304da82e8>, <truth_tables.Person object at 0x7fd304da8710>, <truth_tables.Person object at 0x7fd304da8c50>, <truth_tables.Person object at 0x7fd304daf5c0>, <truth_tables.Person object at 0x7fd304dafef0>, <truth_tables.Person object at 0x7fd304db3a90>, <truth_tables.Person object at 0x7fd304db3908>, <truth_tables.Person object at 0x7fd304db36d8>, <truth_tables.Person object at 0x7fd304dba3c8>, <truth_tables.Person object at 0x7fd304dbaef0>, <truth_tables.Person object at 0x7fd304dc0c18>, <truth_tables.Person object at 0x7fd304d44240>, <truth_tables.Person object at 0x7fd304dc0ef0>, <truth_tables.Person object at 0x7fd304dc0c88>, <truth_tables.Person object at 0x7fd304d4a358>, <truth_tables.Person object at 0x7fd304d4ac18>, <truth_tables.Person object at 0x7fd304d50fd0>, <truth_tables.Person object at 0x7fd304d500f0>, <truth_tables.Person object at 0x7fd304d55588>, <truth_tables.Person object at 0x7fd304d55080>, <truth_tables.Person object at 0x7fd304d55a90>, <truth_tables.Person object at 0x7fd304d5b4e0>, <truth_tables.Person object at 0x7fd304d5bf28>, <truth_tables.Person object at 0x7fd304d60278>, <truth_tables.Person object at 0x7fd304d604e0>, <truth_tables.Person object at 0x7fd304d60a20>, <truth_tables.Person object at 0x7fd304d60cc0>, <truth_tables.Person object at 0x7fd304d65be0>, <truth_tables.Person object at 0x7fd304d65eb8>, <truth_tables.Person object at 0x7fd304d6b748>, <truth_tables.Person object at 0x7fd304d6b9e8>, <truth_tables.Person object at 0x7fd304d6bf28>, <truth_tables.Person object at 0x7fd304d70cc0>, <truth_tables.Person object at 0x7fd304d76278>, <truth_tables.Person object at 0x7fd304d76400>, <truth_tables.Person object at 0x7fd304d76898>, <truth_tables.Person object at 0x7fd304d7b978>, <truth_tables.Person object at 0x7fd304d7bef0>, <truth_tables.Person object at 0x7fd304d815c0>, <truth_tables.Person object at 0x7fd304d81cf8>, <truth_tables.Person object at 0x7fd304d81f28>, <truth_tables.Person object at 0x7fd304d079b0>, <truth_tables.Person object at 0x7fd304d0c400>, <truth_tables.Person object at 0x7fd304d0c978>, <truth_tables.Person object at 0x7fd304d0ceb8>, <truth_tables.Person object at 0x7fd304d111d0>, <truth_tables.Person object at 0x7fd304d11358>, <truth_tables.Person object at 0x7fd304d162b0>, <truth_tables.Person object at 0x7fd304d167b8>, <truth_tables.Person object at 0x7fd304d16550>, <truth_tables.Person object at 0x7fd304d1d0b8>, <truth_tables.Person object at 0x7fd304d164e0>, <truth_tables.Person object at 0x7fd304d16cc0>, <truth_tables.Person object at 0x7fd304d1da90>, <truth_tables.Person object at 0x7fd304d1dcf8>, <truth_tables.Person object at 0x7fd304d29780>, <truth_tables.Person object at 0x7fd304d29a20>, <truth_tables.Person object at 0x7fd304d2f320>, <truth_tables.Person object at 0x7fd304d2f5c0>, <truth_tables.Person object at 0x7fd304d2f8d0>, <truth_tables.Person object at 0x7fd304d2fb38>, <truth_tables.Person object at 0x7fd304d346d8>, <truth_tables.Person object at 0x7fd304d2f898>, <truth_tables.Person object at 0x7fd304d2f9e8>, <truth_tables.Person object at 0x7fd304d39ef0>, <truth_tables.Person object at 0x7fd304cc3a58>, <truth_tables.Person object at 0x7fd304cc3e80>, <truth_tables.Person object at 0x7fd304cc79e8>, <truth_tables.Person object at 0x7fd304cc78d0>, <truth_tables.Person object at 0x7fd304cce4e0>, <truth_tables.Person object at 0x7fd304cc7898>, <truth_tables.Person object at 0x7fd304ccef28>, <truth_tables.Person object at 0x7fd304cd4080>, <truth_tables.Person object at 0x7fd304cd4518>, <truth_tables.Person object at 0x7fd304cd4cf8>, <truth_tables.Person object at 0x7fd304cd9940>, <truth_tables.Person object at 0x7fd304cd95c0>, <truth_tables.Person object at 0x7fd304cdd7f0>, <truth_tables.Person object at 0x7fd304cdda90>, <truth_tables.Person object at 0x7fd304ce2518>, <truth_tables.Person object at 0x7fd304ce2320>, <truth_tables.Person object at 0x7fd304ce8fd0>, <truth_tables.Person object at 0x7fd304ceda20>, <truth_tables.Person object at 0x7fd304cedd30>, <truth_tables.Person object at 0x7fd304cf4da0>, <truth_tables.Person object at 0x7fd304cf9518>, <truth_tables.Person object at 0x7fd304cf9cc0>, <truth_tables.Person object at 0x7fd304cf9a20>, <truth_tables.Person object at 0x7fd304c882b0>, <truth_tables.Person object at 0x7fd304c88550>, <truth_tables.Person object at 0x7fd304c90198>, <truth_tables.Person object at 0x7fd304c90b38>, <truth_tables.Person object at 0x7fd304c95080>, <truth_tables.Person object at 0x7fd304c952b0>, <truth_tables.Person object at 0x7fd304c990f0>, <truth_tables.Person object at 0x7fd304c9f160>, <truth_tables.Person object at 0x7fd304c9f978>, <truth_tables.Person object at 0x7fd304c9fda0>, <truth_tables.Person object at 0x7fd304c9ffd0>, <truth_tables.Person object at 0x7fd304ca4ac8>, <truth_tables.Person object at 0x7fd304caa7b8>, <truth_tables.Person object at 0x7fd304caad30>, <truth_tables.Person object at 0x7fd304cb0048>, <truth_tables.Person object at 0x7fd304cb05c0>, <truth_tables.Person object at 0x7fd304cb0b00>, <truth_tables.Person object at 0x7fd304cb0e80>, <truth_tables.Person object at 0x7fd304cbb5c0>, <truth_tables.Person object at 0x7fd304cbbb00>, <truth_tables.Person object at 0x7fd304cc1358>, <truth_tables.Person object at 0x7fd304cc1dd8>, <truth_tables.Person object at 0x7fd304cc13c8>, <truth_tables.Person object at 0x7fd304c45588>, <truth_tables.Person object at 0x7fd304cc1898>, <truth_tables.Person object at 0x7fd304c45dd8>, <truth_tables.Person object at 0x7fd304c4b0b8>, <truth_tables.Person object at 0x7fd304c4b8d0>, <truth_tables.Person object at 0x7fd304c4b390>, <truth_tables.Person object at 0x7fd304c4bd30>, <truth_tables.Person object at 0x7fd304c512e8>, <truth_tables.Person object at 0x7fd304c5c898>, <truth_tables.Person object at 0x7fd304c56668>, <truth_tables.Person object at 0x7fd304c5c208>, <truth_tables.Person object at 0x7fd304c67160>, <truth_tables.Person object at 0x7fd304c6c080>, <truth_tables.Person object at 0x7fd304c67630>, <truth_tables.Person object at 0x7fd304c6c240>, <truth_tables.Person object at 0x7fd304c6c898>, <truth_tables.Person object at 0x7fd304c70390>, <truth_tables.Person object at 0x7fd304c70518>, <truth_tables.Person object at 0x7fd304c76048>, <truth_tables.Person object at 0x7fd304c765c0>, <truth_tables.Person object at 0x7fd304c7b048>, <truth_tables.Person object at 0x7fd304c7b898>, <truth_tables.Person object at 0x7fd304c812e8>, <truth_tables.Person object at 0x7fd304c817f0>, <truth_tables.Person object at 0x7fd304c06048>, <truth_tables.Person object at 0x7fd304c061d0>, <truth_tables.Person object at 0x7fd304c06f28>, <truth_tables.Person object at 0x7fd304c0cdd8>, <truth_tables.Person object at 0x7fd304c0cb38>, <truth_tables.Person object at 0x7fd304c14160>, <truth_tables.Person object at 0x7fd304c142b0>, <truth_tables.Person object at 0x7fd304c14cf8>, <truth_tables.Person object at 0x7fd304c14fd0>, <truth_tables.Person object at 0x7fd304c192b0>, <truth_tables.Person object at 0x7fd304c19908>, <truth_tables.Person object at 0x7fd304c1e1d0>, <truth_tables.Person object at 0x7fd304c1e4a8>, <truth_tables.Person object at 0x7fd304c22048>, <truth_tables.Person object at 0x7fd304c22278>, <truth_tables.Person object at 0x7fd304c284e0>, <truth_tables.Person object at 0x7fd304c2e208>, <truth_tables.Person object at 0x7fd304c2ecc0>, <truth_tables.Person object at 0x7fd304c338d0>, <truth_tables.Person object at 0x7fd304c33e10>, <truth_tables.Person object at 0x7fd304c33b70>, <truth_tables.Person object at 0x7fd304c395c0>, <truth_tables.Person object at 0x7fd304c39860>, <truth_tables.Person object at 0x7fd304c39d30>, <truth_tables.Person object at 0x7fd304c39fd0>, <truth_tables.Person object at 0x7fd304c3fac8>, <truth_tables.Person object at 0x7fd304c3feb8>, <truth_tables.Person object at 0x7fd304bc55c0>, <truth_tables.Person object at 0x7fd304bc9518>, <truth_tables.Person object at 0x7fd304bc5cf8>, <truth_tables.Person object at 0x7fd304bc9ef0>, <truth_tables.Person object at 0x7fd304bc9dd8>, <truth_tables.Person object at 0x7fd304bcebe0>, <truth_tables.Person object at 0x7fd304bd42b0>, <truth_tables.Person object at 0x7fd304bd4550>, <truth_tables.Person object at 0x7fd304bd49e8>, <truth_tables.Person object at 0x7fd304bd9940>, <truth_tables.Person object at 0x7fd304bdf898>, <truth_tables.Person object at 0x7fd304be5470>, <truth_tables.Person object at 0x7fd304be5cc0>, <truth_tables.Person object at 0x7fd304bea588>, <truth_tables.Person object at 0x7fd304beada0>, <truth_tables.Person object at 0x7fd304bef908>, <truth_tables.Person object at 0x7fd304befc18>, <truth_tables.Person object at 0x7fd304bf42b0>, <truth_tables.Person object at 0x7fd304bf4e48>, <truth_tables.Person object at 0x7fd304bf4780>, <truth_tables.Person object at 0x7fd304bf91d0>, <truth_tables.Person object at 0x7fd304bf9978>, <truth_tables.Person object at 0x7fd304bf96d8>, <truth_tables.Person object at 0x7fd304c01400>, <truth_tables.Person object at 0x7fd304c01908>, <truth_tables.Person object at 0x7fd304b86ba8>, <truth_tables.Person object at 0x7fd304b8c630>, <truth_tables.Person object at 0x7fd304b8c940>, <truth_tables.Person object at 0x7fd304b8cc50>, <truth_tables.Person object at 0x7fd304b8cf98>, <truth_tables.Person object at 0x7fd304b8cfd0>, <truth_tables.Person object at 0x7fd304b92c88>, <truth_tables.Person object at 0x7fd304b97438>, <truth_tables.Person object at 0x7fd304b9d898>, <truth_tables.Person object at 0x7fd304ba22b0>, <truth_tables.Person object at 0x7fd304ba2780>, <truth_tables.Person object at 0x7fd304ba2f60>, <truth_tables.Person object at 0x7fd304ba72e8>, <truth_tables.Person object at 0x7fd304bad080>, <truth_tables.Person object at 0x7fd304ba75f8>, <truth_tables.Person object at 0x7fd304bad860>, <truth_tables.Person object at 0x7fd304badb38>, <truth_tables.Person object at 0x7fd304bade48>, <truth_tables.Person object at 0x7fd304bb3320>, <truth_tables.Person object at 0x7fd304bb3ac8>, <truth_tables.Person object at 0x7fd304bb9358>, <truth_tables.Person object at 0x7fd304bb96a0>, <truth_tables.Person object at 0x7fd304bb9ac8>, <truth_tables.Person object at 0x7fd304bbef60>, <truth_tables.Person object at 0x7fd304b43278>, <truth_tables.Person object at 0x7fd304b43eb8>, <truth_tables.Person object at 0x7fd304b509b0>, <truth_tables.Person object at 0x7fd304b50b00>, <truth_tables.Person object at 0x7fd304b55668>, <truth_tables.Person object at 0x7fd304b55f98>, <truth_tables.Person object at 0x7fd304b55cf8>, <truth_tables.Person object at 0x7fd304b5a908>, <truth_tables.Person object at 0x7fd304b5e0b8>, <truth_tables.Person object at 0x7fd304b5e5c0>, <truth_tables.Person object at 0x7fd304b5eac8>, <truth_tables.Person object at 0x7fd304b5e668>, <truth_tables.Person object at 0x7fd304b63c50>, <truth_tables.Person object at 0x7fd304b6ab38>, <truth_tables.Person object at 0x7fd304b6acc0>, <truth_tables.Person object at 0x7fd304b706a0>, <truth_tables.Person object at 0x7fd304b70400>, <truth_tables.Person object at 0x7fd304b75828>, <truth_tables.Person object at 0x7fd304b75198>, <truth_tables.Person object at 0x7fd304b70c88>, <truth_tables.Person object at 0x7fd304b75f98>, <truth_tables.Person object at 0x7fd304b80748>, <truth_tables.Person object at 0x7fd304b80c88>, <truth_tables.Person object at 0x7fd304b06780>, <truth_tables.Person object at 0x7fd304b0c278>, <truth_tables.Person object at 0x7fd304b0cac8>, <truth_tables.Person object at 0x7fd304b11080>, <truth_tables.Person object at 0x7fd304b11860>, <truth_tables.Person object at 0x7fd304b17908>, <truth_tables.Person object at 0x7fd304b1b2b0>, <truth_tables.Person object at 0x7fd304b22a20>, <truth_tables.Person object at 0x7fd304b22cc0>, <truth_tables.Person object at 0x7fd304b27048>, <truth_tables.Person object at 0x7fd304b27588>, <truth_tables.Person object at 0x7fd304b27ba8>, <truth_tables.Person object at 0x7fd304b2d908>, <truth_tables.Person object at 0x7fd304b33470>, <truth_tables.Person object at 0x7fd304b335c0>, <truth_tables.Person object at 0x7fd304b33f60>, <truth_tables.Person object at 0x7fd304b377b8>, <truth_tables.Person object at 0x7fd304b375c0>, <truth_tables.Person object at 0x7fd304b379b0>, <truth_tables.Person object at 0x7fd304b3d358>, <truth_tables.Person object at 0x7fd304ac34e0>, <truth_tables.Person object at 0x7fd304ac3748>, <truth_tables.Person object at 0x7fd304ac8208>, <truth_tables.Person object at 0x7fd304ac8668>, <truth_tables.Person object at 0x7fd304ac8390>, <truth_tables.Person object at 0x7fd304ac3978>, <truth_tables.Person object at 0x7fd304ac8c88>, <truth_tables.Person object at 0x7fd304acebe0>, <truth_tables.Person object at 0x7fd304ace710>, <truth_tables.Person object at 0x7fd304ad2080>, <truth_tables.Person object at 0x7fd304acecc0>, <truth_tables.Person object at 0x7fd304ad28d0>, <truth_tables.Person object at 0x7fd304ada668>, <truth_tables.Person object at 0x7fd304adabe0>, <truth_tables.Person object at 0x7fd304adf1d0>, <truth_tables.Person object at 0x7fd304adf710>, <truth_tables.Person object at 0x7fd304ae43c8>, <truth_tables.Person object at 0x7fd304ae4da0>, <truth_tables.Person object at 0x7fd304aea0b8>, <truth_tables.Person object at 0x7fd304aea2e8>, <truth_tables.Person object at 0x7fd304aea400>, <truth_tables.Person object at 0x7fd304aeada0>, <truth_tables.Person object at 0x7fd304af0198>, <truth_tables.Person object at 0x7fd304af0978>, <truth_tables.Person object at 0x7fd304af5048>, <truth_tables.Person object at 0x7fd304af0be0>, <truth_tables.Person object at 0x7fd304af5e10>, <truth_tables.Person object at 0x7fd304af5b70>, <truth_tables.Person object at 0x7fd304afcc88>, <truth_tables.Person object at 0x7fd304b00b38>, <truth_tables.Person object at 0x7fd304b00e80>, <truth_tables.Person object at 0x7fd304b00860>, <truth_tables.Person object at 0x7fd304a85438>, <truth_tables.Person object at 0x7fd304a85eb8>, <truth_tables.Person object at 0x7fd304a85908>, <truth_tables.Person object at 0x7fd304a8b3c8>, <truth_tables.Person object at 0x7fd304a85748>, <truth_tables.Person object at 0x7fd304a91588>, <truth_tables.Person object at 0x7fd304a85a20>, <truth_tables.Person object at 0x7fd304a97160>, <truth_tables.Person object at 0x7fd304a973c8>, <truth_tables.Person object at 0x7fd304a975f8>, <truth_tables.Person object at 0x7fd304a9b9e8>, <truth_tables.Person object at 0x7fd304a9bf60>, <truth_tables.Person object at 0x7fd304aa12b0>, <truth_tables.Person object at 0x7fd304aa1a20>, <truth_tables.Person object at 0x7fd304aa71d0>, <truth_tables.Person object at 0x7fd304aa1c88>, <truth_tables.Person object at 0x7fd304aa7f98>, <truth_tables.Person object at 0x7fd304aa1eb8>, <truth_tables.Person object at 0x7fd304aa7fd0>, <truth_tables.Person object at 0x7fd304ab2ba8>, <truth_tables.Person object at 0x7fd304ab6390>, <truth_tables.Person object at 0x7fd304ab6630>, <truth_tables.Person object at 0x7fd304ab6c50>, <truth_tables.Person object at 0x7fd304abc198>, <truth_tables.Person object at 0x7fd304ab6eb8>, <truth_tables.Person object at 0x7fd304abcd68>, <truth_tables.Person object at 0x7fd304abcf98>, <truth_tables.Person object at 0x7fd304ac2ba8>, <truth_tables.Person object at 0x7fd304a48198>, <truth_tables.Person object at 0x7fd304a48320>, <truth_tables.Person object at 0x7fd304a48a20>, <truth_tables.Person object at 0x7fd304a4d828>, <truth_tables.Person object at 0x7fd304a4dd68>, <truth_tables.Person object at 0x7fd304a524e0>, <truth_tables.Person object at 0x7fd304a52cf8>, <truth_tables.Person object at 0x7fd304a52f98>, <truth_tables.Person object at 0x7fd304a57358>, <truth_tables.Person object at 0x7fd304a57b00>, <truth_tables.Person object at 0x7fd304a61048>, <truth_tables.Person object at 0x7fd304a61550>, <truth_tables.Person object at 0x7fd304a61f60>, <truth_tables.Person object at 0x7fd304a6c550>, <truth_tables.Person object at 0x7fd304a6c898>, <truth_tables.Person object at 0x7fd304a726d8>, <truth_tables.Person object at 0x7fd304a72dd8>, <truth_tables.Person object at 0x7fd304a78898>, <truth_tables.Person object at 0x7fd304a7f748>, <truth_tables.Person object at 0x7fd304a04400>, <truth_tables.Person object at 0x7fd304a0a668>, <truth_tables.Person object at 0x7fd304a0a400>, <truth_tables.Person object at 0x7fd304a10080>, <truth_tables.Person object at 0x7fd304a106a0>, <truth_tables.Person object at 0x7fd304a10b00>, <truth_tables.Person object at 0x7fd304a152b0>, <truth_tables.Person object at 0x7fd304a15940>, <truth_tables.Person object at 0x7fd304a155c0>, <truth_tables.Person object at 0x7fd304a1abe0>, <truth_tables.Person object at 0x7fd304a1aef0>, <truth_tables.Person object at 0x7fd304a205c0>, <truth_tables.Person object at 0x7fd304a20cc0>, <truth_tables.Person object at 0x7fd304a244e0>, <truth_tables.Person object at 0x7fd304a247b8>, <truth_tables.Person object at 0x7fd304a24a58>, <truth_tables.Person object at 0x7fd304a24cf8>, <truth_tables.Person object at 0x7fd304a298d0>, <truth_tables.Person object at 0x7fd304a2e668>, <truth_tables.Person object at 0x7fd304a2ebe0>, <truth_tables.Person object at 0x7fd304a34b00>, <truth_tables.Person object at 0x7fd304a2ee80>, <truth_tables.Person object at 0x7fd304a342e8>, <truth_tables.Person object at 0x7fd304a3b5f8>, <truth_tables.Person object at 0x7fd304a3bb70>, <truth_tables.Person object at 0x7fd304a3b898>, <truth_tables.Person object at 0x7fd304a3b7b8>, <truth_tables.Person object at 0x7fd304a40438>, <truth_tables.Person object at 0x7fd304a3bef0>, <truth_tables.Person object at 0x7fd304a409e8>, <truth_tables.Person object at 0x7fd3049c57b8>, <truth_tables.Person object at 0x7fd3049c5c50>, <truth_tables.Person object at 0x7fd3049c5c88>, <truth_tables.Person object at 0x7fd3049d0048>, <truth_tables.Person object at 0x7fd3049cacf8>, <truth_tables.Person object at 0x7fd3049d6198>, <truth_tables.Person object at 0x7fd3049db0b8>, <truth_tables.Person object at 0x7fd3049d6b00>, <truth_tables.Person object at 0x7fd3049db5f8>, <truth_tables.Person object at 0x7fd3049df940>, <truth_tables.Person object at 0x7fd3049db9b0>, <truth_tables.Person object at 0x7fd3049dfd68>, <truth_tables.Person object at 0x7fd3049dfc88>, <truth_tables.Person object at 0x7fd3049dffd0>, <truth_tables.Person object at 0x7fd3049e5ba8>, <truth_tables.Person object at 0x7fd3049ee0f0>, <truth_tables.Person object at 0x7fd3049eec50>, <truth_tables.Person object at 0x7fd3049ee940>, <truth_tables.Person object at 0x7fd3049ee6d8>, <truth_tables.Person object at 0x7fd3049f3358>, <truth_tables.Person object at 0x7fd3049f8588>, <truth_tables.Person object at 0x7fd3049f8748>, <truth_tables.Person object at 0x7fd3049fc0b8>, <truth_tables.Person object at 0x7fd3049fc898>, <truth_tables.Person object at 0x7fd304988198>, <truth_tables.Person object at 0x7fd3049884a8>, <truth_tables.Person object at 0x7fd304988c88>, <truth_tables.Person object at 0x7fd304988d68>, <truth_tables.Person object at 0x7fd30498e0b8>, <truth_tables.Person object at 0x7fd30498ea58>, <truth_tables.Person object at 0x7fd3049932e8>, <truth_tables.Person object at 0x7fd304998240>, <truth_tables.Person object at 0x7fd304993e10>, <truth_tables.Person object at 0x7fd304998cf8>, <truth_tables.Person object at 0x7fd3049989b0>, <truth_tables.Person object at 0x7fd3049a89e8>, <truth_tables.Person object at 0x7fd3049a8e10>, <truth_tables.Person object at 0x7fd3049a8e48>, <truth_tables.Person object at 0x7fd3049ae908>, <truth_tables.Person object at 0x7fd3049b4828>, <truth_tables.Person object at 0x7fd3049b49e8>, <truth_tables.Person object at 0x7fd3049b4898>, <truth_tables.Person object at 0x7fd3049b9400>, <truth_tables.Person object at 0x7fd3049b97f0>, <truth_tables.Person object at 0x7fd3049bf5c0>, <truth_tables.Person object at 0x7fd3049bfd30>, <truth_tables.Person object at 0x7fd3049bffd0>, <truth_tables.Person object at 0x7fd3049bfda0>, <truth_tables.Person object at 0x7fd304943d30>, <truth_tables.Person object at 0x7fd304948320>, <truth_tables.Person object at 0x7fd304948470>, <truth_tables.Person object at 0x7fd304948da0>, <truth_tables.Person object at 0x7fd30494f940>, <truth_tables.Person object at 0x7fd3049535c0>, <truth_tables.Person object at 0x7fd30495a0b8>, <truth_tables.Person object at 0x7fd30495a7b8>, <truth_tables.Person object at 0x7fd30495f0f0>, <truth_tables.Person object at 0x7fd30495f4a8>, <truth_tables.Person object at 0x7fd304965cf8>, <truth_tables.Person object at 0x7fd30496b278>, <truth_tables.Person object at 0x7fd30496b390>, <truth_tables.Person object at 0x7fd30496be80>, <truth_tables.Person object at 0x7fd304976048>, <truth_tables.Person object at 0x7fd3049765f8>, <truth_tables.Person object at 0x7fd304976dd8>, <truth_tables.Person object at 0x7fd30497c630>, <truth_tables.Person object at 0x7fd304980278>, <truth_tables.Person object at 0x7fd304980630>, <truth_tables.Person object at 0x7fd304980b38>, <truth_tables.Person object at 0x7fd3049059b0>, <truth_tables.Person object at 0x7fd304905cc0>, <truth_tables.Person object at 0x7fd304905e48>, <truth_tables.Person object at 0x7fd30490a390>, <truth_tables.Person object at 0x7fd30490a978>, <truth_tables.Person object at 0x7fd30490ae10>, <truth_tables.Person object at 0x7fd30490f400>, <truth_tables.Person object at 0x7fd30491cf28>, <truth_tables.Person object at 0x7fd30491cda0>, <truth_tables.Person object at 0x7fd304920a90>, <truth_tables.Person object at 0x7fd304925ba8>, <truth_tables.Person object at 0x7fd304925eb8>, <truth_tables.Person object at 0x7fd30492b630>, <truth_tables.Person object at 0x7fd30492b978>, <truth_tables.Person object at 0x7fd30492bac8>, <truth_tables.Person object at 0x7fd30492be48>, <truth_tables.Person object at 0x7fd30492f278>, <truth_tables.Person object at 0x7fd30492f518>, <truth_tables.Person object at 0x7fd30492fa20>, <truth_tables.Person object at 0x7fd304934550>, <truth_tables.Person object at 0x7fd30492f3c8>, <truth_tables.Person object at 0x7fd30493a7f0>, <truth_tables.Person object at 0x7fd30493aa90>, <truth_tables.Person object at 0x7fd30493ad30>, <truth_tables.Person object at 0x7fd30493f390>, <truth_tables.Person object at 0x7fd30493f630>, <truth_tables.Person object at 0x7fd30493fda0>, <truth_tables.Person object at 0x7fd3048c7160>, <truth_tables.Person object at 0x7fd3048c7f98>, <truth_tables.Person object at 0x7fd3048cb898>, <truth_tables.Person object at 0x7fd3048d0748>, <truth_tables.Person object at 0x7fd3048cbc18>, <truth_tables.Person object at 0x7fd3048d0278>, <truth_tables.Person object at 0x7fd3048d5128>, <truth_tables.Person object at 0x7fd3048d5390>, <truth_tables.Person object at 0x7fd3048daa90>, <truth_tables.Person object at 0x7fd3048da358>, <truth_tables.Person object at 0x7fd3048da4a8>, <truth_tables.Person object at 0x7fd3048da240>, <truth_tables.Person object at 0x7fd3048da128>, <truth_tables.Person object at 0x7fd3048e1278>, <truth_tables.Person object at 0x7fd3048e74a8>, <truth_tables.Person object at 0x7fd3048e77b8>, <truth_tables.Person object at 0x7fd3048e7ac8>, <truth_tables.Person object at 0x7fd3048ec048>, <truth_tables.Person object at 0x7fd3048ec1d0>, <truth_tables.Person object at 0x7fd3048ec940>, <truth_tables.Person object at 0x7fd3048ecc88>, <truth_tables.Person object at 0x7fd3048f6080>, <truth_tables.Person object at 0x7fd3048f6358>, <truth_tables.Person object at 0x7fd3048f6710>, <truth_tables.Person object at 0x7fd3048fd198>, <truth_tables.Person object at 0x7fd3048fd7f0>, <truth_tables.Person object at 0x7fd3048fdef0>, <truth_tables.Person object at 0x7fd304902470>, <truth_tables.Person object at 0x7fd304902b38>, <truth_tables.Person object at 0x7fd304888a20>, <truth_tables.Person object at 0x7fd304888f98>, <truth_tables.Person object at 0x7fd30488eba8>, <truth_tables.Person object at 0x7fd30488ee48>, <truth_tables.Person object at 0x7fd30488ee80>, <truth_tables.Person object at 0x7fd304898400>, <truth_tables.Person object at 0x7fd30489d080>, <truth_tables.Person object at 0x7fd3048989b0>, <truth_tables.Person object at 0x7fd30489d400>, <truth_tables.Person object at 0x7fd304898cc0>, <truth_tables.Person object at 0x7fd30489d9b0>, <truth_tables.Person object at 0x7fd3048a8278>, <truth_tables.Person object at 0x7fd3048a8898>, <truth_tables.Person object at 0x7fd3048a8cc0>, <truth_tables.Person object at 0x7fd3048a8f28>, <truth_tables.Person object at 0x7fd3048acc50>, <truth_tables.Person object at 0x7fd3048ac908>, <truth_tables.Person object at 0x7fd3048aca20>, <truth_tables.Person object at 0x7fd3048ac9e8>, <truth_tables.Person object at 0x7fd3048b3940>, <truth_tables.Person object at 0x7fd3048b64e0>, <truth_tables.Person object at 0x7fd3048b6ef0>, <truth_tables.Person object at 0x7fd3048bd7f0>, <truth_tables.Person object at 0x7fd3048bdd30>, <truth_tables.Person object at 0x7fd3048bda20>, <truth_tables.Person object at 0x7fd304843588>, <truth_tables.Person object at 0x7fd304843160>, <truth_tables.Person object at 0x7fd304843dd8>, <truth_tables.Person object at 0x7fd30484e5f8>, <truth_tables.Person object at 0x7fd30484ea58>, <truth_tables.Person object at 0x7fd30484eb70>, <truth_tables.Person object at 0x7fd30484ebe0>, <truth_tables.Person object at 0x7fd3048546a0>, <truth_tables.Person object at 0x7fd304859da0>, <truth_tables.Person object at 0x7fd30485e2b0>, <truth_tables.Person object at 0x7fd30485e438>, <truth_tables.Person object at 0x7fd30485eb00>, <truth_tables.Person object at 0x7fd304864710>, <truth_tables.Person object at 0x7fd304864550>, <truth_tables.Person object at 0x7fd304864828>, <truth_tables.Person object at 0x7fd304864cf8>, <truth_tables.Person object at 0x7fd30486aa58>, <truth_tables.Person object at 0x7fd3048716a0>, <truth_tables.Person object at 0x7fd304876470>, <truth_tables.Person object at 0x7fd304876978>, <truth_tables.Person object at 0x7fd30487b6d8>, <truth_tables.Person object at 0x7fd30487f3c8>, <truth_tables.Person object at 0x7fd30487fba8>, <truth_tables.Person object at 0x7fd30487fe80>, <truth_tables.Person object at 0x7fd3048068d0>, <truth_tables.Person object at 0x7fd304806c18>, <truth_tables.Person object at 0x7fd30480be10>, <truth_tables.Person object at 0x7fd304811080>, <truth_tables.Person object at 0x7fd304811ef0>, <truth_tables.Person object at 0x7fd304816ba8>, <truth_tables.Person object at 0x7fd304816eb8>, <truth_tables.Person object at 0x7fd304816fd0>, <truth_tables.Person object at 0x7fd304821748>, <truth_tables.Person object at 0x7fd304821cc0>, <truth_tables.Person object at 0x7fd30482b358>, <truth_tables.Person object at 0x7fd30482b828>, <truth_tables.Person object at 0x7fd3048321d0>, <truth_tables.Person object at 0x7fd3048327b8>, <truth_tables.Person object at 0x7fd304832a20>, <truth_tables.Person object at 0x7fd3048384a8>, <truth_tables.Person object at 0x7fd304838da0>, <truth_tables.Person object at 0x7fd304838f28>, <truth_tables.Person object at 0x7fd3048427b8>, <truth_tables.Person object at 0x7fd3047d1400>, <truth_tables.Person object at 0x7fd3047cbf60>, <truth_tables.Person object at 0x7fd3047d1da0>, <truth_tables.Person object at 0x7fd3047d7908>, <truth_tables.Person object at 0x7fd3047d7438>, <truth_tables.Person object at 0x7fd3047ddc18>, <truth_tables.Person object at 0x7fd3047e2b70>, <truth_tables.Person object at 0x7fd3047e28d0>, <truth_tables.Person object at 0x7fd3047ed198>, <truth_tables.Person object at 0x7fd3047f22e8>, <truth_tables.Person object at 0x7fd3047f28d0>, <truth_tables.Person object at 0x7fd3047f2c18>, <truth_tables.Person object at 0x7fd3047f2e80>, <truth_tables.Person object at 0x7fd3047f8940>, <truth_tables.Person object at 0x7fd3047f8eb8>, <truth_tables.Person object at 0x7fd3047fc198>, <truth_tables.Person object at 0x7fd304802550>, <truth_tables.Person object at 0x7fd3048027f0>, <truth_tables.Person object at 0x7fd304802b70>, <truth_tables.Person object at 0x7fd3047886a0>, <truth_tables.Person object at 0x7fd304788c88>, <truth_tables.Person object at 0x7fd304788f60>, <truth_tables.Person object at 0x7fd30478deb8>, <truth_tables.Person object at 0x7fd304792ac8>, <truth_tables.Person object at 0x7fd3047974a8>, <truth_tables.Person object at 0x7fd304797668>, <truth_tables.Person object at 0x7fd304797278>, <truth_tables.Person object at 0x7fd30479cac8>, <truth_tables.Person object at 0x7fd3047a0828>, <truth_tables.Person object at 0x7fd3047a0a90>, <truth_tables.Person object at 0x7fd3047a7e10>, <truth_tables.Person object at 0x7fd3047b2cc0>, <truth_tables.Person object at 0x7fd3047b2e10>, <truth_tables.Person object at 0x7fd3047b6400>, <truth_tables.Person object at 0x7fd3047b6f98>, <truth_tables.Person object at 0x7fd3047bc278>, <truth_tables.Person object at 0x7fd3047bcb70>, <truth_tables.Person object at 0x7fd3047bc9e8>, <truth_tables.Person object at 0x7fd3047c2630>, <truth_tables.Person object at 0x7fd3047c24a8>, <truth_tables.Person object at 0x7fd304747390>, <truth_tables.Person object at 0x7fd304747940>, <truth_tables.Person object at 0x7fd304747e80>, <truth_tables.Person object at 0x7fd304747eb8>, <truth_tables.Person object at 0x7fd3047534e0>, <truth_tables.Person object at 0x7fd304753a58>, <truth_tables.Person object at 0x7fd30475e080>, <truth_tables.Person object at 0x7fd30475e1d0>, <truth_tables.Person object at 0x7fd30475e470>, <truth_tables.Person object at 0x7fd30475ee48>, <truth_tables.Person object at 0x7fd304762940>, <truth_tables.Person object at 0x7fd304769828>, <truth_tables.Person object at 0x7fd304769470>, <truth_tables.Person object at 0x7fd304769b00>, <truth_tables.Person object at 0x7fd304769b38>, <truth_tables.Person object at 0x7fd304773b00>, <truth_tables.Person object at 0x7fd304773f98>, <truth_tables.Person object at 0x7fd30477ae48>, <truth_tables.Person object at 0x7fd30477a4e0>, <truth_tables.Person object at 0x7fd304773d30>, <truth_tables.Person object at 0x7fd304780470>, <truth_tables.Person object at 0x7fd304780dd8>, <truth_tables.Person object at 0x7fd3047043c8>, <truth_tables.Person object at 0x7fd304780f60>, <truth_tables.Person object at 0x7fd304780e10>, <truth_tables.Person object at 0x7fd304709be0>, <truth_tables.Person object at 0x7fd304709eb8>, <truth_tables.Person object at 0x7fd304714c88>, <truth_tables.Person object at 0x7fd304719438>, <truth_tables.Person object at 0x7fd3047146d8>, <truth_tables.Person object at 0x7fd3047197b8>, <truth_tables.Person object at 0x7fd30471d128>, <truth_tables.Person object at 0x7fd30471d5c0>, <truth_tables.Person object at 0x7fd304719cf8>, <truth_tables.Person object at 0x7fd30471db00>, <truth_tables.Person object at 0x7fd304727828>, <truth_tables.Person object at 0x7fd304723630>, <truth_tables.Person object at 0x7fd304723be0>, <truth_tables.Person object at 0x7fd304727518>, <truth_tables.Person object at 0x7fd304723cc0>, <truth_tables.Person object at 0x7fd30472fc18>, <truth_tables.Person object at 0x7fd304735048>, <truth_tables.Person object at 0x7fd30473ab00>, <truth_tables.Person object at 0x7fd30473f470>, <truth_tables.Person object at 0x7fd30473aef0>, <truth_tables.Person object at 0x7fd30473fa58>, <truth_tables.Person object at 0x7fd30473ffd0>, <truth_tables.Person object at 0x7fd3046cc048>, <truth_tables.Person object at 0x7fd3046ccf60>, <truth_tables.Person object at 0x7fd3046d14a8>, <truth_tables.Person object at 0x7fd3046d52b0>, <truth_tables.Person object at 0x7fd3046d5780>, <truth_tables.Person object at 0x7fd3046d5898>, <truth_tables.Person object at 0x7fd3046db0b8>, <truth_tables.Person object at 0x7fd3046d5da0>, <truth_tables.Person object at 0x7fd3046dbda0>, <truth_tables.Person object at 0x7fd3046dbdd8>, <truth_tables.Person object at 0x7fd3046e2a58>, <truth_tables.Person object at 0x7fd3046e7748>, <truth_tables.Person object at 0x7fd3046e7f28>, <truth_tables.Person object at 0x7fd3046e7c88>, <truth_tables.Person object at 0x7fd3046ed898>, <truth_tables.Person object at 0x7fd3046ed5f8>, <truth_tables.Person object at 0x7fd3046f1390>, <truth_tables.Person object at 0x7fd3046f64a8>, <truth_tables.Person object at 0x7fd3046f6748>, <truth_tables.Person object at 0x7fd3046f6ef0>, <truth_tables.Person object at 0x7fd3046f6f28>, <truth_tables.Person object at 0x7fd3046f6fd0>, <truth_tables.Person object at 0x7fd3046fbcf8>, <truth_tables.Person object at 0x7fd3046fb1d0>, <truth_tables.Person object at 0x7fd304701b38>, <truth_tables.Person object at 0x7fd304701cc0>, <truth_tables.Person object at 0x7fd304686198>, <truth_tables.Person object at 0x7fd304686630>, <truth_tables.Person object at 0x7fd304686dd8>, <truth_tables.Person object at 0x7fd30468d198>, <truth_tables.Person object at 0x7fd30468dfd0>, <truth_tables.Person object at 0x7fd304692f60>, <truth_tables.Person object at 0x7fd304696588>, <truth_tables.Person object at 0x7fd304696f60>, <truth_tables.Person object at 0x7fd30469e4a8>, <truth_tables.Person object at 0x7fd30469efd0>, <truth_tables.Person object at 0x7fd30469e630>, <truth_tables.Person object at 0x7fd30469ebe0>, <truth_tables.Person object at 0x7fd3046a43c8>, <truth_tables.Person object at 0x7fd3046a46a0>, <truth_tables.Person object at 0x7fd3046a4f28>, <truth_tables.Person object at 0x7fd3046ad8d0>, <truth_tables.Person object at 0x7fd3046adba8>, <truth_tables.Person object at 0x7fd3046b22e8>, <truth_tables.Person object at 0x7fd3046b25c0>, <truth_tables.Person object at 0x7fd3046b2860>, <truth_tables.Person object at 0x7fd3046b2b00>, <truth_tables.Person object at 0x7fd3046b2cc0>, <truth_tables.Person object at 0x7fd3046b2ba8>, <truth_tables.Person object at 0x7fd3046b99e8>, <truth_tables.Person object at 0x7fd3046b97b8>, <truth_tables.Person object at 0x7fd3046c1dd8>, <truth_tables.Person object at 0x7fd304645320>, <truth_tables.Person object at 0x7fd3046c18d0>, <truth_tables.Person object at 0x7fd304645f28>, <truth_tables.Person object at 0x7fd30464bd68>, <truth_tables.Person object at 0x7fd30464f470>, <truth_tables.Person object at 0x7fd30464f908>, <truth_tables.Person object at 0x7fd30464fb70>, <truth_tables.Person object at 0x7fd304655550>, <truth_tables.Person object at 0x7fd30465a550>, <truth_tables.Person object at 0x7fd30465ae10>, <truth_tables.Person object at 0x7fd304660a90>, <truth_tables.Person object at 0x7fd3046664e0>, <truth_tables.Person object at 0x7fd3046662b0>, <truth_tables.Person object at 0x7fd30466b470>, <truth_tables.Person object at 0x7fd30466bc18>, <truth_tables.Person object at 0x7fd3046712b0>, <truth_tables.Person object at 0x7fd304671438>, <truth_tables.Person object at 0x7fd3046767b8>, <truth_tables.Person object at 0x7fd304676e10>, <truth_tables.Person object at 0x7fd30467ba58>, <truth_tables.Person object at 0x7fd3046829b0>, <truth_tables.Person object at 0x7fd3046094e0>, <truth_tables.Person object at 0x7fd3046124e0>, <truth_tables.Person object at 0x7fd30460dc88>, <truth_tables.Person object at 0x7fd304612e80>, <truth_tables.Person object at 0x7fd304617a58>, <truth_tables.Person object at 0x7fd304617cf8>, <truth_tables.Person object at 0x7fd30461c518>, <truth_tables.Person object at 0x7fd304617fd0>, <truth_tables.Person object at 0x7fd30461c668>, <truth_tables.Person object at 0x7fd30461cfd0>, <truth_tables.Person object at 0x7fd304623710>, <truth_tables.Person object at 0x7fd304623780>, <truth_tables.Person object at 0x7fd304623f28>, <truth_tables.Person object at 0x7fd3046281d0>, <truth_tables.Person object at 0x7fd3046284a8>, <truth_tables.Person object at 0x7fd304628780>, <truth_tables.Person object at 0x7fd3046289e8>, <truth_tables.Person object at 0x7fd30462ec18>, <truth_tables.Person object at 0x7fd30462e3c8>, <truth_tables.Person object at 0x7fd304635b00>, <truth_tables.Person object at 0x7fd304635d30>, <truth_tables.Person object at 0x7fd30463a160>, <truth_tables.Person object at 0x7fd304635fd0>, <truth_tables.Person object at 0x7fd3046408d0>, <truth_tables.Person object at 0x7fd304640fd0>, <truth_tables.Person object at 0x7fd3045c4f28>, <truth_tables.Person object at 0x7fd3045c49b0>, <truth_tables.Person object at 0x7fd3045ca160>, <truth_tables.Person object at 0x7fd3045ca518>, <truth_tables.Person object at 0x7fd3045ca668>, <truth_tables.Person object at 0x7fd3045cfac8>, <truth_tables.Person object at 0x7fd3045d39e8>, <truth_tables.Person object at 0x7fd3045d38d0>, <truth_tables.Person object at 0x7fd3045dacf8>, <truth_tables.Person object at 0x7fd3045de5f8>, <truth_tables.Person object at 0x7fd3045de898>, <truth_tables.Person object at 0x7fd3045dec50>, <truth_tables.Person object at 0x7fd3045def98>, <truth_tables.Person object at 0x7fd3045e4b38>, <truth_tables.Person object at 0x7fd3045defd0>, <truth_tables.Person object at 0x7fd3045e40f0>, <truth_tables.Person object at 0x7fd3045e8c88>, <truth_tables.Person object at 0x7fd3045e8f98>, <truth_tables.Person object at 0x7fd3045eeba8>, <truth_tables.Person object at 0x7fd3045f50f0>, <truth_tables.Person object at 0x7fd3045f5908>, <truth_tables.Person object at 0x7fd3045f96a0>, <truth_tables.Person object at 0x7fd304602518>, <truth_tables.Person object at 0x7fd304602ac8>, <truth_tables.Person object at 0x7fd304602da0>, <truth_tables.Person object at 0x7fd304602f28>, <truth_tables.Person object at 0x7fd304590160>, <truth_tables.Person object at 0x7fd3045902e8>, <truth_tables.Person object at 0x7fd304590748>, <truth_tables.Person object at 0x7fd304594358>, <truth_tables.Person object at 0x7fd304594f98>, <truth_tables.Person object at 0x7fd304599a90>, <truth_tables.Person object at 0x7fd30459feb8>, <truth_tables.Person object at 0x7fd30459f4e0>, <truth_tables.Person object at 0x7fd3045a4eb8>, <truth_tables.Person object at 0x7fd3045ac128>, <truth_tables.Person object at 0x7fd3045acf28>, <truth_tables.Person object at 0x7fd3045b0b00>, <truth_tables.Person object at 0x7fd3045b7f28>, <truth_tables.Person object at 0x7fd3045bc9b0>, <truth_tables.Person object at 0x7fd3045c0c88>, <truth_tables.Person object at 0x7fd304546128>, <truth_tables.Person object at 0x7fd3045466d8>, <truth_tables.Person object at 0x7fd304546f60>, <truth_tables.Person object at 0x7fd304546c50>, <truth_tables.Person object at 0x7fd30454ca58>, <truth_tables.Person object at 0x7fd3045516d8>, <truth_tables.Person object at 0x7fd304551978>, <truth_tables.Person object at 0x7fd304551ef0>, <truth_tables.Person object at 0x7fd3045564e0>, <truth_tables.Person object at 0x7fd30455b198>, <truth_tables.Person object at 0x7fd304556780>, <truth_tables.Person object at 0x7fd30455bef0>, <truth_tables.Person object at 0x7fd304561358>, <truth_tables.Person object at 0x7fd304567320>, <truth_tables.Person object at 0x7fd304567cc0>, <truth_tables.Person object at 0x7fd30456c710>, <truth_tables.Person object at 0x7fd304571710>, <truth_tables.Person object at 0x7fd304575550>, <truth_tables.Person object at 0x7fd304575780>, <truth_tables.Person object at 0x7fd304575a90>, <truth_tables.Person object at 0x7fd304575ba8>, <truth_tables.Person object at 0x7fd304575f60>, <truth_tables.Person object at 0x7fd30457d9b0>, <truth_tables.Person object at 0x7fd304575d30>, <truth_tables.Person object at 0x7fd304582470>, <truth_tables.Person object at 0x7fd30457def0>, <truth_tables.Person object at 0x7fd3045827f0>, <truth_tables.Person object at 0x7fd304582a20>, <truth_tables.Person object at 0x7fd304582f60>, <truth_tables.Person object at 0x7fd304507128>, <truth_tables.Person object at 0x7fd30450c080>, <truth_tables.Person object at 0x7fd30450c940>, <truth_tables.Person object at 0x7fd30450c860>, <truth_tables.Person object at 0x7fd3045103c8>, <truth_tables.Person object at 0x7fd304510518>, <truth_tables.Person object at 0x7fd3045189e8>, <truth_tables.Person object at 0x7fd304518da0>, <truth_tables.Person object at 0x7fd30451ffd0>, <truth_tables.Person object at 0x7fd304523080>, <truth_tables.Person object at 0x7fd304523eb8>, <truth_tables.Person object at 0x7fd304528198>, <truth_tables.Person object at 0x7fd30452c588>, <truth_tables.Person object at 0x7fd30452c828>, <truth_tables.Person object at 0x7fd304539550>, <truth_tables.Person object at 0x7fd304539d30>, <truth_tables.Person object at 0x7fd30453d3c8>, <truth_tables.Person object at 0x7fd304542320>, <truth_tables.Person object at 0x7fd3045427b8>, <truth_tables.Person object at 0x7fd3044c9908>, <truth_tables.Person object at 0x7fd3044c9b38>, <truth_tables.Person object at 0x7fd3044ce208>, <truth_tables.Person object at 0x7fd3044cef98>, <truth_tables.Person object at 0x7fd3044ce8d0>, <truth_tables.Person object at 0x7fd3044d3a90>, <truth_tables.Person object at 0x7fd3044d3c18>, <truth_tables.Person object at 0x7fd3044da9b0>, <truth_tables.Person object at 0x7fd3044de2b0>, <truth_tables.Person object at 0x7fd3044deac8>, <truth_tables.Person object at 0x7fd3044ded68>, <truth_tables.Person object at 0x7fd3044e44e0>, <truth_tables.Person object at 0x7fd3044ea518>, <truth_tables.Person object at 0x7fd3044ea630>, <truth_tables.Person object at 0x7fd3044eaf60>, <truth_tables.Person object at 0x7fd3044efd30>, <truth_tables.Person object at 0x7fd3044efeb8>, <truth_tables.Person object at 0x7fd3044f3208>, <truth_tables.Person object at 0x7fd3044f3b00>, <truth_tables.Person object at 0x7fd3044f3e48>, <truth_tables.Person object at 0x7fd3044f9160>, <truth_tables.Person object at 0x7fd3044f9550>, <truth_tables.Person object at 0x7fd3044f99e8>, <truth_tables.Person object at 0x7fd3044fe780>, <truth_tables.Person object at 0x7fd3044f9668>, <truth_tables.Person object at 0x7fd3044fe3c8>, <truth_tables.Person object at 0x7fd304484630>, <truth_tables.Person object at 0x7fd3044fef60>, <truth_tables.Person object at 0x7fd304484f98>, <truth_tables.Person object at 0x7fd30448bc18>, <truth_tables.Person object at 0x7fd30448bf98>, <truth_tables.Person object at 0x7fd30448fb00>, <truth_tables.Person object at 0x7fd3044944e0>, <truth_tables.Person object at 0x7fd304494390>, <truth_tables.Person object at 0x7fd304494c88>, <truth_tables.Person object at 0x7fd30449b208>, <truth_tables.Person object at 0x7fd3044a0470>, <truth_tables.Person object at 0x7fd30449bf60>, <truth_tables.Person object at 0x7fd3044a0a90>, <truth_tables.Person object at 0x7fd3044a66a0>, <truth_tables.Person object at 0x7fd3044a6828>, <truth_tables.Person object at 0x7fd3044a6eb8>, <truth_tables.Person object at 0x7fd3044aa320>, <truth_tables.Person object at 0x7fd3044a6cc0>, <truth_tables.Person object at 0x7fd3044aaeb8>, <truth_tables.Person object at 0x7fd3044b0d68>, <truth_tables.Person object at 0x7fd3044b5da0>, <truth_tables.Person object at 0x7fd3044b5828>, <truth_tables.Person object at 0x7fd3044bbd30>, <truth_tables.Person object at 0x7fd3044c22b0>, <truth_tables.Person object at 0x7fd3044c2e10>, <truth_tables.Person object at 0x7fd304446198>, <truth_tables.Person object at 0x7fd3044463c8>, <truth_tables.Person object at 0x7fd304446630>, <truth_tables.Person object at 0x7fd304446b70>, <truth_tables.Person object at 0x7fd304446f98>, <truth_tables.Person object at 0x7fd30444b710>, <truth_tables.Person object at 0x7fd304451be0>, <truth_tables.Person object at 0x7fd30444be80>, <truth_tables.Person object at 0x7fd3044575f8>, <truth_tables.Person object at 0x7fd304457e10>, <truth_tables.Person object at 0x7fd30445c3c8>, <truth_tables.Person object at 0x7fd304457780>, <truth_tables.Person object at 0x7fd304457ac8>, <truth_tables.Person object at 0x7fd30445c710>, <truth_tables.Person object at 0x7fd304467080>, <truth_tables.Person object at 0x7fd304467b00>, <truth_tables.Person object at 0x7fd3044673c8>, <truth_tables.Person object at 0x7fd304471198>, <truth_tables.Person object at 0x7fd30446ce80>, <truth_tables.Person object at 0x7fd304471828>, <truth_tables.Person object at 0x7fd304471b38>, <truth_tables.Person object at 0x7fd304476160>, <truth_tables.Person object at 0x7fd304476278>, <truth_tables.Person object at 0x7fd304476550>, <truth_tables.Person object at 0x7fd3044769b0>, <truth_tables.Person object at 0x7fd304476a20>, <truth_tables.Person object at 0x7fd30447d940>, <truth_tables.Person object at 0x7fd304481a20>, <truth_tables.Person object at 0x7fd304481cc0>, <truth_tables.Person object at 0x7fd3044070b8>, <truth_tables.Person object at 0x7fd3044073c8>, <truth_tables.Person object at 0x7fd304481d30>, <truth_tables.Person object at 0x7fd3044077f0>, <truth_tables.Person object at 0x7fd30440a668>, <truth_tables.Person object at 0x7fd30440aa20>, <truth_tables.Person object at 0x7fd304412198>, <truth_tables.Person object at 0x7fd3044129b0>, <truth_tables.Person object at 0x7fd3044126a0>, <truth_tables.Person object at 0x7fd304417a20>, <truth_tables.Person object at 0x7fd304417c88>, <truth_tables.Person object at 0x7fd304417c50>, <truth_tables.Person object at 0x7fd304422d68>, <truth_tables.Person object at 0x7fd30441cfd0>, <truth_tables.Person object at 0x7fd304422278>, <truth_tables.Person object at 0x7fd304422a58>, <truth_tables.Person object at 0x7fd30442a780>, <truth_tables.Person object at 0x7fd30442a4a8>, <truth_tables.Person object at 0x7fd30442ae80>, <truth_tables.Person object at 0x7fd30442d400>, <truth_tables.Person object at 0x7fd30442dfd0>, <truth_tables.Person object at 0x7fd3044335c0>, <truth_tables.Person object at 0x7fd304433d30>, <truth_tables.Person object at 0x7fd304437ba8>, <truth_tables.Person object at 0x7fd3044371d0>, <truth_tables.Person object at 0x7fd304437550>, <truth_tables.Person object at 0x7fd304437898>, <truth_tables.Person object at 0x7fd30443e1d0>, <truth_tables.Person object at 0x7fd30443e470>, <truth_tables.Person object at 0x7fd30443e5c0>, <truth_tables.Person object at 0x7fd30443ea58>, <truth_tables.Person object at 0x7fd3043c3048>, <truth_tables.Person object at 0x7fd3043c34e0>, <truth_tables.Person object at 0x7fd3043c3f98>, <truth_tables.Person object at 0x7fd3043c3fd0>, <truth_tables.Person object at 0x7fd3043ce320>, <truth_tables.Person object at 0x7fd3043ce588>, <truth_tables.Person object at 0x7fd3043ce3c8>, <truth_tables.Person object at 0x7fd3043ceef0>, <truth_tables.Person object at 0x7fd3043d6320>, <truth_tables.Person object at 0x7fd3043cef28>, <truth_tables.Person object at 0x7fd3043d69e8>, <truth_tables.Person object at 0x7fd3043d6f60>, <truth_tables.Person object at 0x7fd3043db240>, <truth_tables.Person object at 0x7fd3043db4e0>, <truth_tables.Person object at 0x7fd3043dba20>, <truth_tables.Person object at 0x7fd3043dbb38>, <truth_tables.Person object at 0x7fd3043e5b70>, <truth_tables.Person object at 0x7fd3043e5b38>, <truth_tables.Person object at 0x7fd3043f2978>, <truth_tables.Person object at 0x7fd3043f2dd8>, <truth_tables.Person object at 0x7fd3043f7390>, <truth_tables.Person object at 0x7fd3043f7d68>, <truth_tables.Person object at 0x7fd3043f7400>, <truth_tables.Person object at 0x7fd3043fd358>, <truth_tables.Person object at 0x7fd3043fd588>, <truth_tables.Person object at 0x7fd3043fdf98>, <truth_tables.Person object at 0x7fd3043834a8>, <truth_tables.Person object at 0x7fd304388278>, <truth_tables.Person object at 0x7fd304388978>, <truth_tables.Person object at 0x7fd30438d048>, <truth_tables.Person object at 0x7fd30438d278>, <truth_tables.Person object at 0x7fd30438dd30>, <truth_tables.Person object at 0x7fd30438db70>, <truth_tables.Person object at 0x7fd304393588>, <truth_tables.Person object at 0x7fd3043980b8>, <truth_tables.Person object at 0x7fd304398e10>, <truth_tables.Person object at 0x7fd30439f470>, <truth_tables.Person object at 0x7fd30439f6d8>, <truth_tables.Person object at 0x7fd30439fef0>, <truth_tables.Person object at 0x7fd3043a4748>, <truth_tables.Person object at 0x7fd3043a8710>, <truth_tables.Person object at 0x7fd3043a8fd0>, <truth_tables.Person object at 0x7fd3043a85f8>, <truth_tables.Person object at 0x7fd3043a8d30>, <truth_tables.Person object at 0x7fd3043a8d68>, <truth_tables.Person object at 0x7fd3043b3438>, <truth_tables.Person object at 0x7fd3043b3a58>, <truth_tables.Person object at 0x7fd3043b3d68>, <truth_tables.Person object at 0x7fd3043b9dd8>, <truth_tables.Person object at 0x7fd3043b9320>, <truth_tables.Person object at 0x7fd3043b9390>, <truth_tables.Person object at 0x7fd3043bdf60>, <truth_tables.Person object at 0x7fd304343780>, <truth_tables.Person object at 0x7fd304343550>, <truth_tables.Person object at 0x7fd3043497b8>, <truth_tables.Person object at 0x7fd304349e48>, <truth_tables.Person object at 0x7fd304349e80>, <truth_tables.Person object at 0x7fd304349fd0>, <truth_tables.Person object at 0x7fd304353b38>, <truth_tables.Person object at 0x7fd304353b00>, <truth_tables.Person object at 0x7fd30435a438>, <truth_tables.Person object at 0x7fd30435e5f8>, <truth_tables.Person object at 0x7fd304363320>, <truth_tables.Person object at 0x7fd3043638d0>, <truth_tables.Person object at 0x7fd304369080>, <truth_tables.Person object at 0x7fd3043692e8>, <truth_tables.Person object at 0x7fd304369e80>, <truth_tables.Person object at 0x7fd304373978>, <truth_tables.Person object at 0x7fd304373ac8>, <truth_tables.Person object at 0x7fd304380080>, <truth_tables.Person object at 0x7fd304304208>, <truth_tables.Person object at 0x7fd3043047b8>, <truth_tables.Person object at 0x7fd304304da0>, <truth_tables.Person object at 0x7fd304309ac8>, <truth_tables.Person object at 0x7fd304309470>, <truth_tables.Person object at 0x7fd3043095f8>, <truth_tables.Person object at 0x7fd3043145c0>, <truth_tables.Person object at 0x7fd304314d68>, <truth_tables.Person object at 0x7fd3043192e8>, <truth_tables.Person object at 0x7fd304319e10>, <truth_tables.Person object at 0x7fd304319470>, <truth_tables.Person object at 0x7fd304319940>, <truth_tables.Person object at 0x7fd304319780>, <truth_tables.Person object at 0x7fd30431d2b0>, <truth_tables.Person object at 0x7fd30431d9e8>, <truth_tables.Person object at 0x7fd304324780>, <truth_tables.Person object at 0x7fd304324cf8>, <truth_tables.Person object at 0x7fd3043244a8>, <truth_tables.Person object at 0x7fd304324438>, <truth_tables.Person object at 0x7fd30432d390>, <truth_tables.Person object at 0x7fd30432d9e8>, <truth_tables.Person object at 0x7fd30432df28>, <truth_tables.Person object at 0x7fd3043340b8>, <truth_tables.Person object at 0x7fd304337048>, <truth_tables.Person object at 0x7fd3043374e0>, <truth_tables.Person object at 0x7fd304337d30>, <truth_tables.Person object at 0x7fd30433e320>, <truth_tables.Person object at 0x7fd3042c35f8>, <truth_tables.Person object at 0x7fd3042c3748>, <truth_tables.Person object at 0x7fd3042c3ba8>, <truth_tables.Person object at 0x7fd3042c95f8>, <truth_tables.Person object at 0x7fd3042c9b38>, <truth_tables.Person object at 0x7fd3042cf2b0>, <truth_tables.Person object at 0x7fd3042cf588>, <truth_tables.Person object at 0x7fd3042cfba8>, <truth_tables.Person object at 0x7fd3042cff60>, <truth_tables.Person object at 0x7fd3042cfbe0>, <truth_tables.Person object at 0x7fd3042d43c8>, <truth_tables.Person object at 0x7fd3042d4ef0>, <truth_tables.Person object at 0x7fd3042d4f28>, <truth_tables.Person object at 0x7fd3042da208>, <truth_tables.Person object at 0x7fd3042df828>, <truth_tables.Person object at 0x7fd3042eb908>, <truth_tables.Person object at 0x7fd3042eb5f8>, <truth_tables.Person object at 0x7fd3042ebfd0>, <truth_tables.Person object at 0x7fd3042ebcf8>, <truth_tables.Person object at 0x7fd3042f0630>, <truth_tables.Person object at 0x7fd3042f6390>, <truth_tables.Person object at 0x7fd3042f6518>, <truth_tables.Person object at 0x7fd3042fb470>, <truth_tables.Person object at 0x7fd3042ff2b0>, <truth_tables.Person object at 0x7fd304285588>, <truth_tables.Person object at 0x7fd304285be0>, <truth_tables.Person object at 0x7fd30428b160>, <truth_tables.Person object at 0x7fd30428b7b8>, <truth_tables.Person object at 0x7fd30428bb70>, <truth_tables.Person object at 0x7fd30428beb8>, <truth_tables.Person object at 0x7fd304292278>, <truth_tables.Person object at 0x7fd3042972e8>, <truth_tables.Person object at 0x7fd304297b38>, <truth_tables.Person object at 0x7fd30429b588>, <truth_tables.Person object at 0x7fd30429b278>, <truth_tables.Person object at 0x7fd3042a1860>, <truth_tables.Person object at 0x7fd3042a1e48>, <truth_tables.Person object at 0x7fd3042a1d68>, <truth_tables.Person object at 0x7fd3042ac438>, <truth_tables.Person object at 0x7fd3042ac710>, <truth_tables.Person object at 0x7fd3042accc0>, <truth_tables.Person object at 0x7fd3042acb00>, <truth_tables.Person object at 0x7fd3042b2860>, <truth_tables.Person object at 0x7fd3042b67b8>, <truth_tables.Person object at 0x7fd3042b6630>, <truth_tables.Person object at 0x7fd3042bb978>, <truth_tables.Person object at 0x7fd3042bb6d8>, <truth_tables.Person object at 0x7fd3042bba20>, <truth_tables.Person object at 0x7fd3042bbf98>, <truth_tables.Person object at 0x7fd3042c0ba8>, <truth_tables.Person object at 0x7fd3042485f8>, <truth_tables.Person object at 0x7fd304248c88>, <truth_tables.Person object at 0x7fd304248f28>, <truth_tables.Person object at 0x7fd30424d780>, <truth_tables.Person object at 0x7fd304248f60>, <truth_tables.Person object at 0x7fd304248fd0>, <truth_tables.Person object at 0x7fd30424de10>, <truth_tables.Person object at 0x7fd304251160>, <truth_tables.Person object at 0x7fd304251438>, <truth_tables.Person object at 0x7fd304251668>, <truth_tables.Person object at 0x7fd304258240>, <truth_tables.Person object at 0x7fd304258fd0>, <truth_tables.Person object at 0x7fd304258da0>, <truth_tables.Person object at 0x7fd30425cc50>, <truth_tables.Person object at 0x7fd304269400>, <truth_tables.Person object at 0x7fd304269ba8>, <truth_tables.Person object at 0x7fd30426d7f0>, <truth_tables.Person object at 0x7fd30426d630>, <truth_tables.Person object at 0x7fd3042739b0>, <truth_tables.Person object at 0x7fd304273be0>, <truth_tables.Person object at 0x7fd304278668>, <truth_tables.Person object at 0x7fd304278908>, <truth_tables.Person object at 0x7fd304278e48>, <truth_tables.Person object at 0x7fd30427d978>, <truth_tables.Person object at 0x7fd30427dc18>, <truth_tables.Person object at 0x7fd304205518>, <truth_tables.Person object at 0x7fd3042057f0>, <truth_tables.Person object at 0x7fd30420acf8>, <truth_tables.Person object at 0x7fd30420a9e8>, <truth_tables.Person object at 0x7fd30420e0b8>, <truth_tables.Person object at 0x7fd30420e9b0>, <truth_tables.Person object at 0x7fd30420ee80>, <truth_tables.Person object at 0x7fd30420ebe0>, <truth_tables.Person object at 0x7fd30420e9e8>, <truth_tables.Person object at 0x7fd30420ea20>, <truth_tables.Person object at 0x7fd304214128>, <truth_tables.Person object at 0x7fd30421a2b0>, <truth_tables.Person object at 0x7fd304214c18>, <truth_tables.Person object at 0x7fd30421a908>, <truth_tables.Person object at 0x7fd30421f3c8>, <truth_tables.Person object at 0x7fd30421f6d8>, <truth_tables.Person object at 0x7fd30421fe80>, <truth_tables.Person object at 0x7fd304225588>, <truth_tables.Person object at 0x7fd3042250f0>, <truth_tables.Person object at 0x7fd304229da0>, <truth_tables.Person object at 0x7fd30422e2b0>, <truth_tables.Person object at 0x7fd3042334e0>, <truth_tables.Person object at 0x7fd304233358>, <truth_tables.Person object at 0x7fd304233128>, <truth_tables.Person object at 0x7fd304238320>, <truth_tables.Person object at 0x7fd304238a20>, <truth_tables.Person object at 0x7fd30423f1d0>, <truth_tables.Person object at 0x7fd304238cf8>, <truth_tables.Person object at 0x7fd30423fac8>, <truth_tables.Person object at 0x7fd3041c4b70>, <truth_tables.Person object at 0x7fd3041c4eb8>, <truth_tables.Person object at 0x7fd3041ca278>, <truth_tables.Person object at 0x7fd3041cada0>, <truth_tables.Person object at 0x7fd3041d0b00>, <truth_tables.Person object at 0x7fd3041d0518>, <truth_tables.Person object at 0x7fd3041d6908>, <truth_tables.Person object at 0x7fd3041d64a8>, <truth_tables.Person object at 0x7fd3041d6ef0>, <truth_tables.Person object at 0x7fd3041dc240>, <truth_tables.Person object at 0x7fd3041dc940>, <truth_tables.Person object at 0x7fd3041e0518>, <truth_tables.Person object at 0x7fd3041e60f0>, <truth_tables.Person object at 0x7fd3041e6cc0>, <truth_tables.Person object at 0x7fd3041e6828>, <truth_tables.Person object at 0x7fd3041e6a20>, <truth_tables.Person object at 0x7fd3041ece80>, <truth_tables.Person object at 0x7fd3041ec748>, <truth_tables.Person object at 0x7fd3041f0b00>, <truth_tables.Person object at 0x7fd3041f75c0>, <truth_tables.Person object at 0x7fd3041f7e80>, <truth_tables.Person object at 0x7fd3041fb908>, <truth_tables.Person object at 0x7fd3041fb668>, <truth_tables.Person object at 0x7fd304183208>, <truth_tables.Person object at 0x7fd304183978>, <truth_tables.Person object at 0x7fd304183ef0>, <truth_tables.Person object at 0x7fd304183a20>, <truth_tables.Person object at 0x7fd304189400>, <truth_tables.Person object at 0x7fd304189630>, <truth_tables.Person object at 0x7fd3041934a8>, <truth_tables.Person object at 0x7fd30418ff98>, <truth_tables.Person object at 0x7fd304193630>, <truth_tables.Person object at 0x7fd304193898>, <truth_tables.Person object at 0x7fd30419a320>, <truth_tables.Person object at 0x7fd30419a160>, <truth_tables.Person object at 0x7fd30419ae48>, <truth_tables.Person object at 0x7fd30419afd0>, <truth_tables.Person object at 0x7fd3041a1e80>, <truth_tables.Person object at 0x7fd3041a1d68>, <truth_tables.Person object at 0x7fd3041a5438>, <truth_tables.Person object at 0x7fd3041a5588>, <truth_tables.Person object at 0x7fd3041aa208>, <truth_tables.Person object at 0x7fd3041aa4e0>, <truth_tables.Person object at 0x7fd3041a5eb8>, <truth_tables.Person object at 0x7fd3041aa390>, <truth_tables.Person object at 0x7fd3041ae5c0>, <truth_tables.Person object at 0x7fd3041ae780>, <truth_tables.Person object at 0x7fd3041aec88>, <truth_tables.Person object at 0x7fd3041b5208>, <truth_tables.Person object at 0x7fd3041b59e8>, <truth_tables.Person object at 0x7fd3041b56d8>, <truth_tables.Person object at 0x7fd3041ba898>, <truth_tables.Person object at 0x7fd3041c01d0>, <truth_tables.Person object at 0x7fd3041ba748>, <truth_tables.Person object at 0x7fd3041452e8>, <truth_tables.Person object at 0x7fd3041c0b00>, <truth_tables.Person object at 0x7fd3041459e8>, <truth_tables.Person object at 0x7fd304145780>, <truth_tables.Person object at 0x7fd30414af60>, <truth_tables.Person object at 0x7fd30414a9e8>, <truth_tables.Person object at 0x7fd30414f9e8>, <truth_tables.Person object at 0x7fd3041560f0>, <truth_tables.Person object at 0x7fd304156860>, <truth_tables.Person object at 0x7fd304156dd8>, <truth_tables.Person object at 0x7fd30415c0f0>, <truth_tables.Person object at 0x7fd30415c6d8>, <truth_tables.Person object at 0x7fd30415c358>, <truth_tables.Person object at 0x7fd30415cbe0>, <truth_tables.Person object at 0x7fd304161978>, <truth_tables.Person object at 0x7fd3041614e0>, <truth_tables.Person object at 0x7fd304161ef0>, <truth_tables.Person object at 0x7fd304166e48>, <truth_tables.Person object at 0x7fd304166e80>, <truth_tables.Person object at 0x7fd30416be10>, <truth_tables.Person object at 0x7fd30416bb70>, <truth_tables.Person object at 0x7fd304170f60>, <truth_tables.Person object at 0x7fd304177cc0>, <truth_tables.Person object at 0x7fd30417c780>, <truth_tables.Person object at 0x7fd30417ca20>, <truth_tables.Person object at 0x7fd304181278>, <truth_tables.Person object at 0x7fd304181a90>, <truth_tables.Person object at 0x7fd304181fd0>, <truth_tables.Person object at 0x7fd304106be0>, <truth_tables.Person object at 0x7fd304106e80>, <truth_tables.Person object at 0x7fd30410cfd0>, <truth_tables.Person object at 0x7fd304112a58>, <truth_tables.Person object at 0x7fd304112748>, <truth_tables.Person object at 0x7fd3041172b0>, <truth_tables.Person object at 0x7fd304117a20>, <truth_tables.Person object at 0x7fd30411e278>, <truth_tables.Person object at 0x7fd30411edd8>, <truth_tables.Person object at 0x7fd304122748>, <truth_tables.Person object at 0x7fd304126908>, <truth_tables.Person object at 0x7fd304122cf8>, <truth_tables.Person object at 0x7fd30412d518>, <truth_tables.Person object at 0x7fd3041323c8>, <truth_tables.Person object at 0x7fd304132668>, <truth_tables.Person object at 0x7fd3041370b8>]
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


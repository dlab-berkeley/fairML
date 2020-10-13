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

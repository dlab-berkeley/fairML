---
knitr: "bookdown::render_book"
title: "Fairness and Bias in Machine Learning Workshop"
author: ["Jae Yeon Kim", "Aniket Kesari", "Renata Barreto", "Avery Richard"]
date: "2020-10-16"
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

# Fairness and Bias in Machine Learning Workshop

**Overview**

This workshops provides a gentle introduction to the fairness and Bias in machine learning applications with a focus on the ProPublica's Analysis of the COMPAS algorithm. We revised [the ProPublica's original R and Python code](https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb) to increase its code readability, remix it with other references, then published and deployed the revised notebook using bookdown and GitHub page.

![A gif of defendants being put into an algorithm by SELMAN DESIGN](https://wp.technologyreview.com/wp-content/uploads/2019/10/mit-alg-yb-02-7.gif?fit=1444,962)

**Outline** 

1. Bias in the data 
  - Risk of Recidivism Data
  - Risk of Violent Recidivism Data

2. Bias in the algorithm 

**References**

For more information on the ProPublica's Machine Bias project, we encourage to check out the following references.

* [Argument](https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing/) by Julia Angwin, Jeff Larson, Surya Mattu and Lauren Kirchner

* [Counterargument](https://www.washingtonpost.com/news/monkey-cage/wp/2016/10/17/can-an-algorithm-be-racist-our-analysis-is-more-cautious-than-propublicas/) by Sam Corbett-Davies, Emma Pierson, Avi Feller and Sharad Goel

* [Methodology](https://www.propublica.org/article/how-we-analyzed-the-compas-recidivism-algorithm/)

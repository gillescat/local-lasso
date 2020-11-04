
<!-- README.md is generated from README.Rmd. Please edit that file -->
Locally Adaptive Data Analytics for Large Separated Data Source
==========

<!-- badges: start -->
<!-- badges: end -->
This is a resource page for the locallasso package which allow to perform data analysis with separated data. 

Background
------------

_E = mc ^2^_

Consider response variables _Y_{i}^_ and $d$-dimensional set of predictors denoted as $X_{i} = (X_{i1},...,X_{id})$, where observation pairs $(Y_{i},X_{i})$ are assumed to be independent and identically distributed. It is typically of interest to estimate the unknown smooth function, $m(X)$, which relates predictor variables to the response,

![alt text](https://github.com/gillescat/locallasso/blob/main/Eq%20model.jpg?raw=true)

![alt text](https://github.com/gillescat/locallasso/blob/main/Eq%20local%20lasso.jpg?raw=true)


Installation
------------

You can install the released version of locallasso with:

``` r
install_github("gillescat/local-lasso")
```

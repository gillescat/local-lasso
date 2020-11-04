
<!-- README.md is generated from README.Rmd. Please edit that file -->
Locally Adaptive Data Analytics for Large Separated Data Sources
==========

<!-- badges: start -->
<!-- badges: end -->
This is a resource page for the locallasso package which allow to perform data analysis with separated data. 

Background
------------

Consider response variables Y, d-dimensional set of predictors X, and unknown smooth function m(X) which relates predictor variables to the response,

![alt text](https://github.com/gillescat/locallasso/blob/main/Eq%20model.jpg?raw=true). 

Although each observation is following the same data generating process, data are separated in different data sources. 
Our proposition is to gather the neighbouring observation of a given point of interest using ANN method and predict the value of the response variable at this given point.
The optimization function corresponds to a local linear with local bandwitdh where we included a LASSO penalty to improve prediction, 

![alt text](https://github.com/gillescat/locallasso/blob/main/Eq%20local%20lasso.jpg?raw=true)

This minimization is solved using the coordinate descent algorithm.

Installation
------------

You can install the released version of locallasso with:

``` r
install_github("gillescat/local-lasso")
```

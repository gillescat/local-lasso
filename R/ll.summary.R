# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Summary function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Default \code{ll} summary
#'
#' @param ll.object \code{ll} object
#'
#' @return Summary of \code{ll} object
#'
#' @details Summary of results obtained by local lasso method.
#'
#' @export
#'
#' @export
#'
#' @examples
#'
#'library(locallasso)
#'set.seed(1993)
#'
#'# # - - - - - - - - - - - - - - -#
#'# # Numerical Example
#'# # - - - - - - - - - - - - - - -#
#'
#'# Define the data generating process
#'n=1000000   # number of observations
#'nc=20       # number of continuous variables
#'
#'# Continous variables
#'X = mvrnorm(n,rep(0,nc),diag(1,nc))
#'
#'# Categorical variables
#'D1 = factor(as.numeric(runif(n)<=.5))
#'D2 = factor(as.numeric(runif(n)<=.3))
#'F1 = factor(sample(LETTERS[1:4], n, replace=TRUE, prob=c(0.3, 0.3, 0.3, 0.1)))
#'
#'# Response variable
#'y <- -X[,1]^2 - 4*sin((pi/2)*X[,2]) + X[,3]*X[,4] + rnorm(n,0,1)
#'
#'data = data.frame(D1,X,y,D2,F1)
#'
#'# Apply local lasso method to the data frame
#'ll = local.lasso(data,"y", k=500, nrand = 100)
#'
#'ll.summary(ll)
ll.summary = function(ll.object){

  list_cont = ll.object$list_cont
  list_dummy = ll.object$list_dummy
  names = ll.object$names
  sigma = ll.object$sigma
  k = ll.object$k
  eps = ll.object$eps
  Occur = ll.object$Occur
  p = ll.object$p
  factor = ll.object$factor
  DD = ll.object$DD
  response = ll.object$response
  r = ll.object$results

  row.names(r) <- r[,1]
  r = r[,-1]
  names(r) = c("Relevence freq","KS value","Pr(>KS)")

  cat("\nGlobal variable relevence:\n")
  print(r)
  cat("\nKS: Kolmogorov-Smirnov statistic\n")
  cat("P-values correspond to the null hypothesis where relevent point are randomly distributed.\n")

}

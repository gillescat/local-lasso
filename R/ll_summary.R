# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Local lasso summary function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Local lasso summary function
#'
#' @param ll_object Local lasso object produced by the \code{local_lasso} function.
#'
#' @return Summary of local lasso estimation results.
#'
#' @details Display global relevancy scores and KS-statistics for testing uniformity of relevancy points.
#'
#' @export
#'
ll_summary = function(ll_object){

  r = ll_object$results

  row.names(r) <- r[,1]
  r = r[,-1]
  names(r) = c("Relevence freq","KS value","Pr(>KS)")

  cat("\nGlobal variable relevence:\n")
  print(r)
  cat("\nKS: Kolmogorov-Smirnov statistic\n")
  cat("P-values correspond to the null hypothesis where relevent point are randomly distributed.\n")

}

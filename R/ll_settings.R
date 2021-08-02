# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Local lasso setting function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Local lasso setting function
#'
#' @param sample Random sample drawn from full data set with \code{sampling} function.
#' @param response Name of the response variable.
#' @param k Number of approximate nearest neighbours.
#' @param e Number of evaluation points.
#' @param eps Value of \code{epsilon} for the ANN-search (default = 1).
#'
#' @return Settings for local lasso function.
#'
#' @details Compute a robust estimation of standard deviations. Generate random set of evaluation points.
#'
#' @export
#'
ll_settings = function(sample, response, k, e, eps=1) {

  X_full_sample = sample[,which(names(sample) != response)]
  p = ncol(X_full_sample)
  names = names(X_full_sample)

  # If a variable has less than 14 unique values, it is stored as a categorical variable
  number_factor <- apply(X_full_sample, MARGIN = 2, FUN=function(x) length(unique(x)))
  dummy = number_factor<=14
  cont = !dummy
  X_dummy = X_full_sample[,dummy]
  X_cont = X_full_sample[,cont]

  # If there is only one dummy variable
  X_dummy = as.data.frame(X_dummy)

  # Compute the estimation of sigma
  sigma = sapply(1:length(X_cont),
                 function(i) mad(X_cont[,i]))

  # Get max and min values
  max = apply(X_cont[,], MARGIN = 2, FUN = function(x) quantile(x,.975,names = F)) # 0.975 quantile
  min = apply(X_cont[,], MARGIN = 2, FUN = function(x) quantile(x,.025,names = F)) # 0.025 quantile
  # Comment: (1) Maybe min/max are not needed if they are used only to generate points -> can use random sample.

  factor_level = lapply(1:length(X_dummy),
                        function(i) unique(X_dummy[,i]))

  # Generate evaluation points from uniform distribution if we are not using random sample.
  Random = matrix(NA,e,0)
  ncate = 1
  ncont = 1

  for (i in 1:p) {

    if(cont[i]==T) {

      Random_variable = matrix(runif(e,min[ncont],max[ncont]),e,1)
      ncont = ncont + 1

    }

    else {

      Random_variable = factor(sample(factor_level[[ncate]], e, replace=TRUE))
      ncate = ncate + 1

    }

    Random = data.frame(Random,Random_variable)

  }

  Random = data.frame(Random)
  names(Random) = names
  out = NULL
  out$sigma = sigma
  out$cont = cont
  out$dummy = dummy
  out$max = max
  out$min = min
  out$factor = factor_level
  out$p = p
  out$response = response
  out$names = names
  out$k = k
  out$e = e
  out$eps = eps
  out$Random = Random
  return(out)

}

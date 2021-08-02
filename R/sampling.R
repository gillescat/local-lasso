# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Sampling function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Sampling function
#'
#' @param data Data set.
#' @param ss Number of observation to sample from full dataset (default = 1000)
#' @param fixed Sample a fixed number of observation (fixed = T) or a fraction of the data set (fixed = F)
#' @param frac Fraction of observation to sample from dataset (default = 0.01)
#'
#' @details Draw a random sample of observation from data set.
#'
#' @return Random sample of observation
#'
#' @export
#'
sampling = function(data, ss=1000, fixed=T, frac=0.01) {

  if(fixed==T) {

    rand_sample = data[sample(nrow(data), ss),]

  }

  else {

    rand_sample = data[sample(nrow(data), frac*nrow(data)),]

  }

  return(rand_sample)
}

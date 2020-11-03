# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Epanechnikov function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Epanechnikov function
#'
#' @param u numerical variable.
#'
#' @details Evaluate the value of Epanechnikov function at point u.
#'
#' @return Epanechnikov function at point u
#'
Epanechnikov = function(u){

  if (abs(u)<=1){
    k = 3/4*(1-(u)^2)
  }

  else {
    k = 0
  }

  return(k)
}

# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # epanechnikov function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title epanechnikov function
#'
#' @param u numerical variable.
#'
#' @details Evaluate the value of epanechnikov function at point u.
#'
#' @return epanechnikov function at point u
#'
#' @export
#'
epanechnikov = function(u){

  if (abs(u)<=1){
    k = 3/4*(1-(u)^2)
  }

  else {
    k = 0
  }

  return(k)
}

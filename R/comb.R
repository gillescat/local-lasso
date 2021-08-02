# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Combine function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Combine function
#'
#' @return Combined elements.
#'
#' @details Combined different object (used in parallel computing).
#'
#' @export
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}

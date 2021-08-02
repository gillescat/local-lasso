# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # ANN search function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Ann search function
#'
#' @param data Data set.
#' @param ll_set Local lasso settings produced by \code{ll_set} function.
#' @param search "first" or "second".
#' @param grid Query points to find k-ANN.
#' @param ll_object Global variable relevancy produced by \code{local_lasso} function.
#' @param threshold Threshold defining variable relevancy (default = 0.75).
#'
#' @details Find the k-ANN for query points.
#'
#' @return k-ANN observations number and euclidean distance.
#'
#' @export
#'
#' @importFrom RANN nn2
#'
ann_search = function(data, ll_set, search, grid=NULL, ll_object=NULL, threshold=.75){

  response = ll_set$response
  sigma = ll_set$sigma
  cont = ll_set$cont
  k = ll_set$k
  Random = ll_set$Random
  eps = ll_set$eps
  Occur = ll_object$Occur
  relevent = Occur$Freq>=threshold

  if(search=="first") {

    ann = nn2(data = as.matrix(data[,names(data) != response][,cont]) %*% diag(sigma^-1),
              query = as.matrix(Random[,cont]) %*% diag(sigma^-1),
              k = k,
              treetype = "kd",
              searchtype = "priority",
              eps = eps)

  }

  else {

    ann = nn2(data = as.matrix(data[,names(data) != response][,relevent & cont]) %*% diag(na.omit(sigma[relevent])^-1),
              query = as.matrix(grid[,relevent & cont]) %*% diag(na.omit(sigma[relevent])^-1),
              k = k,
              treetype = "kd",
              searchtype = "priority",
              eps = eps)

  }

  return(ann)

}

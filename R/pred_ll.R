# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Predict function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Predict function
#'
#' @param ann Approximate nearest neighbours obtained by \code{ann_search} function.
#' @param newdata Set of points of interest to predict
#' @param ll_object Local lasso object produced by the \code{local_lasso} function.
#' @param threshold Threshold defining variable relevancy (default = 0.75).
#'
#' @return Prediction of response variable for \code{newdata}
#'
#' @details Predict the value of response for new set of point provided.
#'
#' @export
#'
pred_ll= function(ann, newdata, ll_object, threshold = 0.75){

  k = ll_object$k
  response = ll_object$response
  last = ncol(ann[[1]]) # Same for all
  Occur = ll_object$Occur
  relevent = Occur$Freq>=threshold
  nd = nrow(newdata)

  y_hat = matrix(NA,0,0)

  for (j in 1:nd) {

    dmax = sort(ann[[j]][,last])[k]
    ann_sorted = ann[[j]][ann[[j]][,last]<= dmax,]

    # Define Y,X,W
    y = ann_sorted[,response]
    X = ann_sorted[,which(names(ann_sorted) != response)]
    X = X[,-(last-1)]  # Remove dist from predictors
    X = X[,relevent]
    u = ann_sorted[,last]/max(ann_sorted[,last])
    W = as.numeric(lapply(u,epanechnikov))

    # Local regression without variable selection
    reg_data = data.frame(X,y)
    lm = lm(y ~ .,
            weights = W,
            data=reg_data)

    # Prediction Y_hat at point of interest X_0
    y_hat[j] = predict(lm, newdata[j,])

  }

  out=NULL
  out$y_pred = y_hat

  return(out)
}

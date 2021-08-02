# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Plot function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Plot function
#'
#' @param ann Approximate nearest neighbours obtained by \code{ann_search} function.
#' @param grid Grid points to evaluate produced by \code{gen_grid} function.
#' @param ll_object Local lasso object produced by the \code{local_lasso} function.
#' @param x_var1 Name of the first variable of interest.
#' @param x_var2 Name of the second variable of interest.
#' @param threshold Threshold defining variable relevancy (default = 0.75).
#'
#' @return Plot of the conditional expectation of response variable given \code{x_var1} and \code{x_var2} variables.
#'
#' @details Predict the value of response for \code{grid}.
#' Plot of the conditional expectation of response variable given \code{x_var1} and \code{x_var2} variables.
#'
#'
#' @export
#'
#' @importFrom lattice wireframe
#'
#'
plot_ll = function(ann, grid, ll_object, x_var1, x_var2, threshold = 0.75){

  k = ll_object$k
  response = ll_object$response
  last = ncol(ann[[1]])
  Occur = ll_object$Occur
  relevent = Occur$Freq>=threshold
  ng = nrow(grid)

  y_hat = matrix(NA,0,0)

  for (j in 1:ng) {

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
    y_hat[j] = predict(lm, grid[j,])

  }

  data.plot = cbind(grid[,x_var1], grid[,x_var2], y_hat)
  data.plot = as.data.frame(data.plot)
  colnames(data.plot) = c(x_var1,x_var2,'y')
  w = wireframe(data.plot[,'y'] ~ data.plot[,x_var1]*data.plot[,x_var2],
                cex.lab=1.5,
                drape=T,
                colorkey=T,
                scales = list(arrows = FALSE),
                zlab='Y',
                xlab=x_var1,
                ylab=x_var2,
                zoom = 0.8
  )
  w

}

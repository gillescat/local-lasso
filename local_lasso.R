# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Local Lasso function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Local Lasso function
#'
#' @param ann Approximate nearest neighbours obtained by \code{ann_search} function.
#' @param ll_set Object produced by \code{ll_setting} function.
#'
#' @return local_lasso object containing table of global relevence and KS statistic.
#'
#' @details
#' A Local Kernel Weighted Linear Regression with LASSO penalty is then fitted to each evaluation points using ann observations.
#' The method uses local bandwidth such that each local model is fitted only with the k-ANN observations of each evaluation points.
#' The value of lambda is selected with cross-validation using the \code{glmnet} package.
#'
#'
#' @export
#'
#' @importFrom MASS mvrnorm
#' @import Matrix
#' @import glmnet
#' @importFrom KSgeneral disc_ks_test
#' @importFrom plyr ldply
#' @import doParallel
#' @import foreach
#'
#' @examples
#'
#'library(locallasso)
#'
#'# # - - - - - - - - - - - - - - -#
#'# # Numerical Example
#'# # - - - - - - - - - - - - - - -#
#'
#'n=10^6   # number of observations
#'nc=19       # number of continuous variables
#'
#'# Continous variables
#'X = mvrnorm(n,rep(0,nc),diag(1,nc))
#'
#'# Categorical variable
#'D1 = factor(as.numeric(runif(n)<=.5))
#'
#'# Response variable
#'y <- -X[,1]^2 - 2*sin((pi/2)*X[,2]) + X[,3]*X[,4] + rnorm(n,0,1)
#'
#'data = data.frame(D1,X,y)
#'
#'# Randomly spliting data
#'ndata=10    # number of distributed dataset
#'ss <- sample(1:ndata, size=nrow(data),replace=TRUE)
#'DD <- list()
#'for (i in 1:ndata) {
#'  DD[[i]] = data[ss==i,]
#'  }
#'
#'# Random sampling
#'rand_sample = foreach(i = 1:10, .combine = "rbind") %do% {
#'  sample = sampling(DD[[i]], ss=1000)
#'  return(sample)
#'  }
#'
#'# Settings
#'ll_set = ll_settings(rand_sample, response = 'y', k=1000, e=1000, eps = 1)
#'
#'# First ann search
#'  first_ann = foreach(i = 1:ndata, .combine = "comb", .packages=c('RANN')) %do% {
#'    ann1 = ann_search(DD[[i]], ll_set, search='first')
#'    ann_list = lapply(1:ll_set$e,
#'                    function(d) cbind(DD[[i]][ann1$nn.idx[d,],], ann1$nn.dists[d,]))
#'  return(ann_list)
#'}
#'
#'# Local Lasso
#'ll = local_lasso(first_ann, ll_set)
#'
#'ll_summary(ll)
local_lasso = function(ann, ll_set){

  e = ll_set$e
  k = ll_set$k
  p = ll_set$p
  Random = ll_set$Random
  response = ll_set$response
  list_relevent = list()
  last = ncol(ann[[1]])

  for (j in 1:e) {

    dmax = sort(ann[[j]][,last])[k]
    ann_sorted = ann[[j]][ann[[j]][,last]<= dmax,]

    # Define Y,X,W
    y = ann_sorted[,response]
    X = ann_sorted[,which(names(ann_sorted) != response)]
    X = X[,-(last-1)]  # Remove dist from predictors
    u = ann_sorted[,last]/max(ann_sorted[,last])
    W = as.numeric(lapply(u,epanechnikov))

    # Cross-Validation Lasso with weights W
    cv_lasso = cv.glmnet(data.matrix(X),
                         y,
                         weights = W,
                         alpha=1,
                         nfolds=10,
                         standardize=T)

    # Estimation for Lambda = CV-lambda
    lasso.cv = glmnet(data.matrix(X),
                      y,
                      alpha=1,
                      weights = as.numeric(lapply(u,epanechnikov)),
                      lambda=cv_lasso$lambda.min,
                      standardize=T)

    # Store non-zero coefficients
    non_zero_coef = which(lasso.cv$beta!=0)

    if (length(non_zero_coef)==0) {

      non_zero_coef = c(1:p)

    }

    list_relevent[[j]] = non_zero_coef

  }

  # Report frequency of varaible selection
  Variable = unlist(list_relevent)
  Occur = as.data.frame(table(Variable))
  Occur$Freq = Occur$Freq/e

  # KS test
  list_grid = ldply(list_relevent, rbind)
  full_matrix = matrix(rep(1:p,times=e),e,p,byrow=T)

  # Store relevent observation for each predictor
  query_test_mat = lapply(1:nrow(list_grid),
                          function(r) ifelse(full_matrix[r,] %in% list_grid[r,], Random[r,],NA))
  query_test_mat = data.frame(matrix(unlist(query_test_mat), nrow=e, byrow=T),stringsAsFactors=T)

  KS_stat = matrix(NA,p,1)
  p_value = matrix(NA,p,1)

  for (i in 1:p) {

    if(ll_set$dummy[i]==T) {

      query_test_mat[,i] = factor(query_test_mat[,i], labels = levels(Random[,i]))

      ks_test = disc_ks_test(query_test_mat[,i], ecdf(Random[,i]), exact = T)

    }

    else {

      ks_test = cont_ks_test(unique(query_test_mat[,i]), ecdf(unique(Random[,i])))

    }

    KS_stat[i] = ks_test$statistic
    p_value[i] = ks_test$p.value

  }

  var_names = ll_set$names
  occurence = paste(Occur$Freq * 100, "%", sep = "")
  result_ll = data.frame(var_names,occurence,KS_stat,p_value)
  result_ll = result_ll[order(Occur$Freq*100,decreasing = T),]
  result_ll[,3:4] = round(result_ll[, 3:4], digits = 4)


  out = NULL
  out$results = result_ll
  out$Occur = Occur
  out$rel_points = query_test_mat
  out$eval_points = Random
  out$k = k
  out$response = response
  return(out)

}

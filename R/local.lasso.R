# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Local Lasso function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Local Lasso function
#'
#' @param data A data frame containing the response variable and covariates.
#' @param response The name of response variable.
#' @param k The number of nearest neighbors.
#' @param eps The relative error of ANN's search. The default is set to eps = 1
#' @param ndata number of database
#' @param nrand number of generated random points
#'
#' @return local.lasso object containing table of global relevence and KS statistic.
#'
#' @details Divide randomly original data into a distributed database with \code{ndata} (default = 10) databases.
#' For each one of the \code{nrand} (default = 1000) random points generated,
#' the \code{k} Approximated nearest neighbors (ANN) are located in the distributed database.
#' A Local Kernel Weighted Linear Regression with LASSO penalty is then fitted to each random points.
#' (Formula)
#' The method uses local bandwidth such that each local model is fitted only with the k-ANN observations of each random points.
#' The value of lambda is selected with cross-validation.
#'
#'
#' @export
#'
#' @importFrom MASS mvrnorm
#' @importFrom RANN nn2
#' @import Matrix
#' @import glmnet
#' @importFrom KSgeneral disc_ks_test
#' @importFrom plyr ldply
#'
#'
#' @examples
#'
#'library(locallasso)
#'set.seed(1993)
#'
#'# # - - - - - - - - - - - - - - -#
#'# # Numerical Example
#'# # - - - - - - - - - - - - - - -#
#'
#'# Define the data generating process
#'n=1000000   # number of observations
#'nc=20       # number of continuous variables
#'
#'# Continous variables
#'X = mvrnorm(n,rep(0,nc),diag(1,nc))
#'
#'# Categorical variables
#'D1 = factor(as.numeric(runif(n)<=.5))
#'D2 = factor(as.numeric(runif(n)<=.3))
#'F1 = factor(sample(LETTERS[1:4], n, replace=TRUE, prob=c(0.3, 0.3, 0.3, 0.1)))
#'
#'# Response variable
#'y <- -X[,1]^2 - 4*sin((pi/2)*X[,2]) + X[,3]*X[,4] + rnorm(n,0,1)
#'
#'data = data.frame(D1,X,y,D2,F1)
#'
#'# Apply local lasso method to the data frame
#'ll = local.lasso(data,"y", k=500, nrand=100)
#'
#'ll.summary(ll)
local.lasso = function(data, response, k, eps=1, ndata=10, nrand=1000){

  # Randomly spliting data
  ss <- sample(1:ndata, size=nrow(data),replace=TRUE)
  DD <- list()

  for (i in 1:ndata) {

    DD[[i]] = data[ss==i,]

  }

  # Selecting a random sample of 1000 observation in each Database
  X_sample = lapply(1:length(DD),
                    function(d) DD[[d]][sample(nrow(DD[[d]]), 1000),which(names(DD[[d]]) != response)])
  X_sample = do.call("rbind", X_sample)

  p = ncol(X_sample)
  names = names(X_sample)

  # If a variable has less than 14 unique values, it is stored as a categorical variable
  number_factor <- apply(X_sample, MARGIN = 2, FUN=function(x) length(unique(x)))
  list_dummy = number_factor<=14
  list_cont = !list_dummy

  X_sample_cont = X_sample[,list_cont]
  X_sample_cate = X_sample[,list_dummy]

  # Compute the estimation of sigma
  sigma = sapply(1:length(X_sample_cont),
                 function(i) mad(X_sample_cont[,i]))

  # Get max and min values
  max = apply(X_sample_cont[,], MARGIN = 2, FUN = function(x) quantile(x,.99,names = F)) # 0.975 quantile
  min = apply(X_sample_cont[,], MARGIN = 2, FUN = function(x) quantile(x,.01,names = F)) # 0.025 quantile

  factor = lapply(1:length(X_sample_cate),
                  function(i) unique(X_sample_cate[,i]))


  # Generate random grid point from uniform distribution
  Random = matrix(NA,nrand,0)
  cate = 1
  cont = 1

  for (i in 1:p) {

    if(list_cont[i]==T) {

      Random_variable = matrix(runif(nrand,min[cont],max[cont]),nrand,1)
      cont = cont + 1

    }

    else {

      Random_variable = factor(sample(factor[[cate]], nrand, replace=TRUE))
      cate = cate + 1

    }

    Random = data.frame(Random,Random_variable)

  }

  Random = data.frame(Random)
  names(Random) = names

  # ANN search
#  start_time <- Sys.time()

  ANN_Edist = matrix(NA,nrand,0)
  ANN_index = matrix(NA,nrand,0)

  for (i in 1:length(DD)) {

    # Select continuous variables
    X = DD[[i]][,names(DD[[i]]) != response]
    X_c = X[,list_cont]

    # Standardize variables and points of interest (grid use for prediction)
    X.std = as.matrix(X_c) %*% diag(sigma^-1)
    query.std = as.matrix(Random[,list_cont]) %*% diag(sigma^-1)

    # first ANN search
    first_ann = nn2(X.std, query = query.std, k = k , treetype = "kd", searchtype = "standard", radius = 0, eps = eps)

    # Sort Euclidean distance and index of observation
    ANN_Edist = cbind(ANN_Edist,first_ann$nn.dists)
    ANN_index = cbind(ANN_index,first_ann$nn.idx)

    print(paste(k,"ANN found in DD number",paste(i,"/",ndata,sep="")),quote=F)

  }

  cat("\n")

  ANN_list = list(ANN_Edist,ANN_index)

#  end_time <- Sys.time()
#  time = end_time - start_time
#  print(paste("First ANN search finished in",time))

#  start_time = Sys.time()

  #  Y_predicted_no_lasso = matrix(NA,0,0)
  List_relevent_Coeff = list()

  Set = rep(1:10, each=k)

  for (j in 1:nrand) {

    # Merge all ANN observations
    ANN_DD1  = DD[[1]][ANN_list[[2]][j,0:k],]
    ANN_DD2  = DD[[2]][ANN_list[[2]][j,(k+1):(2*k)],]
    ANN_DD3  = DD[[3]][ANN_list[[2]][j,(2*k+1):(3*k)],]
    ANN_DD4  = DD[[4]][ANN_list[[2]][j,(3*k+1):(4*k)],]
    ANN_DD5  = DD[[5]][ANN_list[[2]][j,(4*k+1):(5*k)],]
    ANN_DD6  = DD[[6]][ANN_list[[2]][j,(5*k+1):(6*k)],]
    ANN_DD7  = DD[[7]][ANN_list[[2]][j,(6*k+1):(7*k)],]
    ANN_DD8  = DD[[8]][ANN_list[[2]][j,(7*k+1):(8*k)],]
    ANN_DD9  = DD[[9]][ANN_list[[2]][j,(8*k+1):(9*k)],]
    ANN_DD10 = DD[[10]][ANN_list[[2]][j,(9*k+1):(10*k)],]

    ANN = rbind(ANN_DD1,ANN_DD2,ANN_DD3,ANN_DD4,ANN_DD5,ANN_DD6,ANN_DD7,ANN_DD8,ANN_DD9,ANN_DD10)

    # Among ANN find k closed observations
    Edist = ANN_list[[1]][j,]
    dmax = sort(Edist)[k]
    knn_second = ANN[Edist <= dmax,]
    dist = Edist[Edist <= dmax]

    # Define Y,X,W
    y = knn_second[,response]
    X = knn_second[,which(names(knn_second) != response)]
    u = dist/max(dist)
    W = as.numeric(lapply(u,Epanechnikov))

    # Local regression without variable selection
    reg_data = data.frame(X,y)
    lm = lm(y ~ .,
            weights = as.numeric(lapply(u,Epanechnikov)),
            data=reg_data)

    # Prediction Y_hat at point of interest X_0
    #    reg_data = rbind(reg_data, query[j,])
    #    Y_predicted_no_lasso[j] = predict(lm, query[j,])

    # Standardize befor LASOO
    #    X.std = cbind(as.matrix(X[,out_mad$list_cont]) %*% diag(out_mad$sigma^-1), X[,!out_mad$list_cont])
    #    X.std = data.matrix(X.std)

    # Cross-Validation Lasso with weights W
    cv.lasso = cv.glmnet(data.matrix(X),y, weights = as.numeric(lapply(u,Epanechnikov)),alpha=1,nfolds=10,standardize=T)
    #cv.lasso = cv.glmnet(X.std,y, alpha=1,nfolds=10,standardize=T) # CRoss-validation without Weights W

    # Estimation for Lambda = CV-labmda
    lasso.cv = glmnet(data.matrix(X),y, alpha=1, weights = as.numeric(lapply(u,Epanechnikov)), lambda=cv.lasso$lambda.min, standardize=F)

    # Store non-zero coefficients
    non_zero_coef = which(lasso.cv$beta!=0)

    if (length(non_zero_coef)==0) {

      non_zero_coef = c(1:p)

    }

    List_relevent_Coeff[[j]] = non_zero_coef
    print(paste(paste(j, "/", nrand,sep = ""), "Points of interest Done"),quote=F)

  }

  # Report frequency of varaible selection
  Variable = unlist(List_relevent_Coeff)
  Occur = as.data.frame(table(Variable))
  Occur$Freq = Occur$Freq/nrand

  # Define as relevant a predictor which is 75% of the time selected as relevent
  #  Relevent = Occur$Freq>=.75

#  end_time = Sys.time()
#  time = end_time - start_time

#  print(paste("Local lasso finished in",time))

  # KS test
  List_grid = ldply(List_relevent_Coeff, rbind)
  full_matrix = matrix(rep(1:p,times=nrand),nrand,p,byrow=T)

  # Store relevent observation for each predictor
  query_test_mat = lapply(1:nrow(List_grid),
                          function(r) ifelse(full_matrix[r,] %in% List_grid[r,], Random[r,],NA))
  query_test_mat = data.frame(matrix(unlist(query_test_mat), nrow=nrand, byrow=T),stringsAsFactors=T)

  KS_stat = matrix(NA,p,1)
  p_value = matrix(NA,p,1)

  for (i in 1:p) {

    if(list_dummy[i]==T) {

      query_test_mat[,i] = factor(query_test_mat[,i], labels = levels(Random[,i]))

      ks_test = disc_ks_test(query_test_mat[,i],ecdf(Random[,i]), exact = T)

    }

    else {

      ks_test = cont_ks_test(query_test_mat[,i],ecdf(Random[,i]))

    }

    KS_stat[i] = ks_test$statistic
    p_value[i] = ks_test$p.value

  }

  Variables = names
  Occurence = paste(Occur$Freq * 100, "%", sep = "")
  result_ll = data.frame(Variables,Occurence,KS_stat,p_value)
  result_ll = result_ll[order(Occur$Freq*100,decreasing = T),]
  result_ll[,3:4] = round(result_ll[, 3:4], digits = 4)
#  print(result_ll)

#  print("All random points have been evaluated by local lasso method")

  out = NULL
  out$sigma = sigma
  out$list_cont = list_cont
  out$list_dummy = list_dummy
  out$max = max
  out$min = min
  out$factor = factor
  out$p = p
  out$DD = DD
  out$response = response
  out$names = names
  out$k = k
  out$eps = eps
  out$Occur= Occur
  out$DD = DD
  out$results = result_ll
  out$ndata = ndata
  out$nrand = nrand
  return(out)

}

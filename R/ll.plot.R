# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Ploting function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Default \code{ll} plotting
#'
#' @param ll.object \code{ll} object
#' @param x.var1 variable to be ploted
#' @param x.var2 variable to be ploted
#' @param ngrid number of grid values for each variable
#' @param threshold threshold to define global variables (default = 0.75)
#'
#' @return None
#'
#' @details Produces default plot of conditional distribution of the response variable given covariates
#' x.var1 and x.var2.
#'
#' The response variable is predicted for an expanded grid with ngrid*ngrid rows containing all combinations
#' of ngrid value of covariates x.var1 and x.var2. All others numerical covariates entries are set to 0 and
#' categorical covariates' set to random factor's level.
#'
#' The threshold of global relevence is set by default to 0.75.
#'
#' @importFrom lattice wireframe
#' @export
#'
#' @examples
#'
#'#'library(locallasso)
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
#'ll = local.lasso(data,"y", k=500, nrand = 100)
#'
#'# Plot of the conditional distribution of y given covariates X1 and X2.
#'ll.plot(ll,"X1","X2",ngrid=20,threshold = .75)
#'
ll.plot = function(ll.object,x.var1,x.var2,ngrid,threshold=.75){

  npoints = ngrid*ngrid
  max = ll.object$max
  min = ll.object$min
  list_cont = ll.object$list_cont
  list_dummy = ll.object$list_dummy
  names = ll.object$names
  sigma = ll.object$sigma
  k = ll.object$k
  eps = ll.object$eps
  Occur = ll.object$Occur
  p = ll.object$p
  factor = ll.object$factor
  DD = ll.object$DD
  response = ll.object$response
  relevent = Occur$Freq>=threshold
  ndata = ll.object$ndata

  max_var1 = max[names[list_cont]==x.var1]
  min_var1 = min[names[list_cont]==x.var1]
  max_var2 = max[names[list_cont]==x.var2]
  min_var2 = min[names[list_cont]==x.var2]

  var1 = seq(min_var1,max_var1,length.out=ngrid)
  var2 = seq(min_var2,max_var2,length.out=ngrid)
  grid = expand.grid(var1,var2)
  names(grid) = c(x.var1,x.var2)

  full_grid = matrix(0,npoints,p)
  full_grid = as.data.frame(full_grid)
  names(full_grid) = names

  full_grid[,x.var1] = grid[,x.var1]
  full_grid[,x.var2] = grid[,x.var2]

  nrowgrid = nrow(full_grid)

  cate = 1

  for (i in 1:p) {

    if(list_dummy[i]==T) {

      full_grid[,i] = factor[[cate]][1] # Take "randomly" the first level observed.
      cate = cate + 1

    }

    else {}

  }

  # Second ANN search with Relevent variables
#  start_time = Sys.time()

  ANN_Edist = matrix(NA,npoints,0)
  ANN_index = matrix(NA,npoints,0)


  for (i in 1:length(DD)) {

    # Select continuous variables and Relevant ones
    X = DD[[i]][,names(DD[[i]]) != response]
    X = X[,relevent & list_cont]

    # Standardize variables and points of interest (grid use for prediction)
    X.std = as.matrix(X) %*% diag(na.omit(sigma[relevent])^-1)
    query.std = as.matrix(full_grid[,relevent & list_cont]) %*% diag(na.omit(sigma[relevent])^-1)

    # SECOND ANN search
    first_ann = nn2(X.std, query = query.std, k = k , treetype = "kd", searchtype = "standard", radius = 0, eps = eps)

    # Sort Euclidean distance and index of observation
    ANN_Edist = cbind(ANN_Edist,first_ann$nn.dists)
    ANN_index = cbind(ANN_index,first_ann$nn.idx)

    print(paste(k,"ANN found in DD number",paste(i,"/",ndata,sep="")),quote=F)

  }

  ANN_list = list(ANN_Edist,ANN_index)

#  end_time <- Sys.time()
#  time = end_time - start_time
#  print(paste("Second ANN search finished in",time))

  # Prediction of grid points
#  start_time = Sys.time()

  cat("\n")

  y_hat = matrix(NA,0,0)
  Set = rep(1:10, each=k)

  for (j in 1:npoints) {

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
    X = X[,relevent]
    u = dist/max(dist)
    W = as.numeric(lapply(u,Epanechnikov))

    # Local regression without variable selection
    reg_data = data.frame(X,y)
    lm = lm(y ~ .,
            weights = as.numeric(lapply(u,Epanechnikov)),
            data=reg_data)

    # Prediction Y_hat at point of interest X_0
    y_hat[j] = predict(lm, full_grid[j,])
    #    reg_data$y = NULL
    #    reg_data = rbind(reg_data, as.numeric(query[j,Relevent]))

    #    Y_predicted_after_lasso[j] = predict(lm, reg_data[k+1,])

    print(paste(paste(j, "/", nrowgrid,sep = ""), "Points of interest Done"),quote=F)

  }

#  end_time = Sys.time()
#  time = end_time - start_time

#  print(paste("Final estimation finished in",time))

  data.plot = cbind(grid[,x.var1], grid[,x.var2], y_hat)
  data.plot = as.data.frame(data.plot)
  colnames(data.plot) = c(x.var1,x.var2,'y')
  w = wireframe(data.plot[,'y'] ~ data.plot[,x.var1]*data.plot[,x.var2],
                cex.lab=1.5,
                drape=T,
                colorkey=T,
                scales = list(arrows = FALSE),
                zlab='Y',
                xlab=x.var1,
                ylab=x.var2,
                zoom = 0.8
  )
  w

}

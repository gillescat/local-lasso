# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# # Generate grid function
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#' @title Generate grid function
#'
#' @param ng Sequential length of \code{x_var1} and \code{x_var2} variables. The resulting grid has \code{ng*ng} rows.
#' @param ll_set Object produced by \code{ll_setting} function.
#' @param x_var1 Name of the first variable of interest.
#' @param x_var2 Name of the second variable of interest.
#'
#' @return Generated grid.
#'
#' @details Generate a grid of length \code{ng*ng}. Where variables of interest \code{x_var1} and \code{x_var2} variables
#' have a sequence of \code{ng} values and the remaining continuous variables are set to 0. The categorical variable are set to
#' one of the level observed.
#'
#'
#' @export
#'
#' @import Matrix
#'
#'
gen_grid = function(ng, ll_set, x_var1, x_var2){

  np = ng*ng
  max = ll_set$max
  min = ll_set$min
  names = ll_set$names
  cont = ll_set$cont
  dummy = ll_set$dummy
  p = ll_set$p
  factor = ll_set$factor

  # Generate grid
  max_var1 = max[names[cont]==x_var1]
  min_var1 = min[names[cont]==x_var1]
  max_var2 = max[names[cont]==x_var2]
  min_var2 = min[names[cont]==x_var2]

  var1 = seq(min_var1,max_var1,length.out=ng)
  var2 = seq(min_var2,max_var2,length.out=ng)
  grid = expand.grid(var1,var2)
  names(grid) = c(x_var1,x_var2)

  full_grid = matrix(0,np,p)   # Value of remaining variables = 0.
  full_grid = as.data.frame(full_grid)
  names(full_grid) = names

  full_grid[,x_var1] = grid[,x_var1]
  full_grid[,x_var2] = grid[,x_var2]

  cate = 1

  for (i in 1:p) {

    if(dummy[i]==T) {

      full_grid[,i] = factor[[cate]][1] # Take first level observed.
      cate = cate + 1

    }

    else {}

  }

  return(full_grid)
}

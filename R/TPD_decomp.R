#' Decomposition of Fixed-Effects index
#'
#' Take (mostly) the same inputs as the FEWS function and return the decomposition
#' on the multilateral scale.
#'
#' @param times vector of the times at which price observations were made. \cr
#' NOTE: \code{times} must be of class Date or numeric.
#' @param logprice vector of log of prices at the given time
#' @param id vector of distinct identification number of consumer goods
#' @param weight vector of expenditure weights used in the regressions
#' @param custom_time either empty (will assume latest periods) or a vector of length
#' two relating to times compared where [1] is the 'from' time and [2] is the 'to' time
#' e.g Comparing contribution from "1973-04-01" to "1973-05-01" would be c("1973-04-01", "1973-05-01")
#' @param window_length optional. Single number for length of window for the data that
#' regressions are fit on. Note if window_length is present it assumes custom time is latest two periods
#' in window.
#' @param big If vector size becomes too large it will fail to calculate. Setting big to true will calculate
#' it in an alternate way, however much slower.
#' @return A dataframe containing the numeric contribution (\code{contrib}) of every price observation
#' in the window, the product of which will equal the index movement between the two time periods. \code{p_contrib} is
#' the standardised percentage contribution of the observation, the sum of which will equal 1. \code{id_p_contrib} is
#' the standardised percentage contribution of the total id, the sum of which will equal 1.
#' @examples
#' library(TPDdecomp)
#'
#' load("Turvey.RData")
#'
#' contributions <- with(
#'     turvey,
#'     TPD_decomp(times = month,
#'                logprice = log(price),
#'                id = commodity,
#'                weight = price*quantity,
#'                custom_time = c("1973-04-30","1973-05-31"),
#'                window_length = NULL)
#'  )
#' #Comparing change from 1973-04-30 to 1973-05-31
#'
#' str(contributions)
#' Classes ‘data.table’ and 'data.frame':	176 obs. of  8 variables:
#' $ times       : Date, format: "1973-07-31" "1973-06-30" "1973-05-31" ...
#' $ price       : num  5.19 4.53 7.17 4.3 3.98 6.21 3.78 3.72 5.68 3.27 ...
#' $ weight      : num  14932 18505 7407 10892 14495 ...
#' $ id          : chr  "Strawberries" "Strawberries" "Strawberries" "Strawberries" ...
#' $ exp_share   : num  0.36 0.413 0.238 0.342 0.41 ...
#' $ contrib     : num  0.965 0.967 1.545 0.97 0.97 ...
#' $ p_contrib   : num  -0.208 -0.201 2.58 -0.181 -0.184 ...
#' $ id_p_contrib: num  0.663 0.663 0.663 0.663 0.663 ...
#'
#' @import Matrix
#' @import MatrixModels
#' @import data.table
#' @import MASS
#' @export
TPD_decomp <- function(times,
                       logprice,
                       id,
                       weight,
                       custom_time = c(),
                       window_length = NULL,
                       big = FALSE,
                       verbose = FALSE){
  
  #Check input variables are as required
  c(times, logprice, id, weight, custom_time, window_length) %=%
    check_inputs (times = times,
                  logprice = logprice,
                  id = id,
                  weight = weight,
                  custom_time = custom_time,
                  window_length = window_length)
  
  #Check if custom_times are in the times vector
  if(length(custom_time)!=0 & !all(custom_time %in% as.character(times))){
    stop("Custom_time is not in times?")
  }
  
  # Make a data frame from all of the inputs
  prices_df <- data.table(times = times,
                          price = exp(logprice),
                          logprice = logprice,
                          weight = weight,
                          id = as.character(id),
                          key = "times")
  
  # It is essential that the data frame is sorted by date and ID
  # use an if because sorting is slow, but testing is fast
  setorderv(prices_df, c("times", "id"), c(-1, 1))
  
  #If the input vectors contain more time periods than the window length
  #we will remove what wouldn't be included in the FEWS calculation
  if(!is.null(window_length)){
    
    if(length(custom_time)!=0){
      #Find position of end of window based on custom_time[2]
      end_time <- which(custom_time[2]==unique(prices_df$times))
      
    }else{
      end_time <-  1
    }
    
    #return times of interest based off end_time index
    times_of_interest <- unique(prices_df$times)[(end_time):(end_time+window_length-1)]
    
    #check there is enough data for the window
    if (any(is.na(times_of_interest))){
      stop("There are NA's in the times of interest. If you have a custom time",
           " and a window length, make sure there is enough back data from custom_time[2]")
    }
    # Filter data.table by times of interest
    prices_df <- prices_df[times %in% times_of_interest]
  }
  
  #Create seperate times and id index variable which glm will use
  prices_df[,"times_index":=as.factor(times)]
  prices_df[,"id_index" := as.factor(id)]
  
  #length of subsets used to seperate design matrix
  times_index_length <- nlevels(prices_df$times_index)
  id_index_length <- nlevels(prices_df$id_index)
  
  #fixed effect regression model formula
  glm_formula <- prices_df$logprice ~ prices_df$times_index + prices_df$id_index
  
  #extract design matrix
  d <- MatrixModels::model.Matrix(glm_formula, sparse =T)
  
  #Design matrix size, (number of times) + (number of ids) - (1 date) - (1 commodity) + (1 intercept)
  vcat("The dimensions of the design matrix:", dim(d),'\n')
  assumed_col_length <-  length(unique(prices_df$times))+length(unique(prices_df$id))-1-1+1
  
  #check it matches
  if(assumed_col_length == dim(d)[2]){
    vcat("It matches\n")
  }else{
    warning("Design matrix is not the expected shape?")
  }
  
  #dtau is the design matrix subset to just the intercept + time elements
  dtau <- d[,1:times_index_length]
  vcat("design tau shape:",dim(dtau),'\n')
  
  #dpi is the design matrix subset to just the
  #commodities (which is everything right of times + intercept)
  dpi <- d[,(times_index_length+1):dim(d)[2]]
  vcat("design pi shape:",dim(dpi),'\n')
  
  #p is the log price vector
  p <- logprice
  vcat("price vector length:",length(p),'\n')
  
  #calculate the expenditure share of each ID for every time
  prices_df[,"exp_share" := weight/sum(weight),by = "times"]
  exp_share <- prices_df$exp_share
  
  #create sparse weight matrix
  wgt <- Matrix::.sparseDiagonal(x = exp_share)
  vcat("weight matrix shape:",dim(wgt),'\n')
  
  #calculate sub components of weight calculation
  vcat("Calculating subcomponents\n")
  wtau <- Matrix::crossprod(dtau,wgt)%*%dtau
  wpi <- Matrix::crossprod(dpi,wgt)%*%dpi
  wtaupi <- Matrix::crossprod(dtau,wgt)%*%dpi
  # wpi_solve <- solve(wpi)
  wpi_solve <- Matrix::chol2inv(Matrix::chol(wpi))
  w_common <- wtaupi%*%wpi_solve
  
  #Calculate weights (This multiplied by logprice vector will equal the estimates of parameters for each time)
  vcat("Calculating contribution matrix\n")

  #Alternate calc (more verbose)
  # contribution_matrix <-
  #   solve(wtau - (wtaupi %*% Matrix::tcrossprod(solve(wpi),wtaupi)),
  #   ((Matrix::t(dtau) - wtaupi %*% Matrix::tcrossprod(solve(wpi), dpi)) %*% wgt))

  left_solver <- list(wtau,wtaupi)
  right_solver <- list(Matrix::t(dtau),dpi)

  #Tidy up
  rm(dtau,
     dpi,
     wtau,
     wtaupi,
     wpi,
     wpi_solve,
     d)

  weight_calc <- function(inputs){
    inputs[[1]]-(Matrix::tcrossprod(w_common,inputs[[2]]))
  }

  if(big){
  #Instead of trying to do the large matrix substraction in one call
  #Break it down by row so max vector allocation is size of  row.

  cat("Calculating alternate way as big = TRUE\n")

  contribution_matrix <- vector("list",2)
  contribution_matrix[[1]] <- weight_calc(left_solver)

  temp_right <- (Matrix::tcrossprod(w_common,right_solver[[2]]))

  nrows <- dim(right_solver[[1]])[1]
  ncols <- dim(right_solver[[1]])[2] #BIG
  contribution_matrix[[2]] <- sparseMatrix(dims=c(nrows,ncols), i={}, j={})
  contribution_matrix[[2]] <- as(contribution_matrix[[2]], "dgCMatrix")

  for(i in 1:nrows){
    cat(i,'/',nrows,'\r')
    contribution_matrix[[2]][i,] <- right_solver[[1]][i,]-temp_right[i,]
  }

  }else{

  contribution_matrix <- lapply(list(left_solver,right_solver),
                                weight_calc)
  }

  contribution_matrix <- solve(contribution_matrix[[1]])%*%contribution_matrix[[2]]%*%wgt
  
  
  #rename rows to times
  dimnames(contribution_matrix) <- list(c("intercept",levels(prices_df$times_index)[-1]),NULL)
  
  #if user input custom times change this
  if (length(custom_time)!=0){
    
    previous <- custom_time[1]
    current <-  custom_time[2]
    
  }else{
    #Get the latest 2 times
    current <- levels(prices_df$times_index)[times_index_length]
    previous <- levels(prices_df$times_index)[times_index_length-1]
    
  }
  
  #Change rowname for given current and previous to literally 'current' and 'previous'
  dimnames(contribution_matrix)[[1]][which(dimnames(contribution_matrix)[[1]]==previous)] <- "previous"
  dimnames(contribution_matrix)[[1]][which(dimnames(contribution_matrix)[[1]]==current)] <- "current"
  
  #Let the user know this is whats being compared. Can investigate if not what is expected
  cat("Comparing change from",as.character(previous),"to",as.character(current),"\n")
  
  #Extract the relevant contribution rows i.e. what periods are being compared
  contrib <- contribution_matrix[c(which(dimnames(contribution_matrix)[[1]]=="previous"),
                                   which(dimnames(contribution_matrix)[[1]]=="current")),]
  
  #Convert the transpose to a data.table
  vcat("Creating contrib table from matrix\n")
  contrib <- as.data.table(as.matrix(Matrix::t(contrib)))
  
  #Merge back with prices_df, as simple as cbind as it should align already
  prices_df <- cbind(prices_df,contrib)
  
  #Calculate contribution of each price
  vcat("Calculating contribution for each price observation\n")
  prices_df[,"contrib":= (price^(current-previous))]
  
  #Remove irrelevant columns
  prices_df <- prices_df[,!c("logprice","id_index","times_index","previous","current")]
  
  #Calculate total contribution for each id (product of contrib over times)
  vcat("Calculating percentage contribution by product\n")
  prices_df[,"p_contrib" := percentage_contrib(contrib)]
  prices_df[,"id_p_contrib" := sum(p_contrib), id]
  
  #sort by descending total contribution
  setorderv(prices_df,"id_p_contrib",-1)
  
  vcat("Complete.\n")
  return(prices_df)
}

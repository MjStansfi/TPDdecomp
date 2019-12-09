check_inputs <- function (times = times, logprice = logprice, id = id,
                          weight = weight, custom_time = custom_time,
                          window_length = window_length) {
  # Function to confirm that all inputs are correct shape and class
  # Returns all inputs, but some may be modified to the correct data type
  
  if (missing(weight) | is.null(weight)){
    weight <- rep(1, length(times))
    cat("\nNo weighting assigned...All weights set to 1\n")
  }
  
  
  if (anyNA(times) | anyNA(logprice) | anyNA(id) | anyNA(weight)){
    stop("Data contains NA values")
  }
  
  if (any(c(is.infinite(times),
            is.infinite(logprice),
            is.infinite(weight)))){
    stop("Data contains Infinite values")
  }
  
  
  if (length(times) != length(logprice)){
    stop("times and logprice should be vectors of the same length")
  }else if (length(times) != length(id)){
    stop("times and id should be vectors of the same length")
  }else if (length(times) != length(weight)){
    stop("times and weight should be vectors of the same length")
  }
  
  # Times must be in a type which can be ordered - otherwise the windows
  # are meaningless
  if (!(class(times) %in% c("Date", "numeric", "integer"))){
    stop("times must be either a Date or numeric type")
  }
  
  if(length(custom_time)!=0  & length(custom_time)!=2){
    stop("custom_time must be a vector with length of either 0 or 2")
  }

  return (list(times = times, logprice = logprice, id = id, weight = weight, custom_time = custom_time, window_length = window_length))
}




"%=%" <- function(lhs, rhs) {
  # Special equals to assign multiple entries at once - like python tuples
  # taken from here:
  # https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
  # %=% is used opposed to := because := is used by data.table package
  # Example usage:
  # c(a, b) %=% functionReturningAListWithTwoValues()
  
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir = frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, "formula"))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir = frame)
  return(invisible(NULL))
}


#' cat function with built in verbosity
#'
#' Depending on the parent frame's "verbose" logical object state (T/F), cat R object(s).
#'
#' Outputs the objects, concatenating the representations. cat performs much less conversion than print.
#'
#' @param ... Things to cat
#' @param sep a character string to separate each element.
#' @param bypass logical. If bypass == T print regardless of other factors.
#' @seealso \link{vprint}
#' @examples \dontrun{
#' dog <- ("dog")
#' 
#' # With no verbose object in the env, and bypass not true, vcat does nothing
#' vcat(dog)
#' 
#' # Dummy function with verbose as an argument to show "local" environment interactions
#' foo <- function(verbose = FALSE){
#'   vcat(dog)
#' }
#' 
#' # Create a global variable verbose set to TRUE
#' verbose <-  T
#' # vcat and foo() now both print the dog object, even though foo's local verbose variable is false. 
#' # i.e. a global verbose veriable will take precedence over a local one
#' vcat(dog)
#' foo(verbose = FALSE)
#' 
#' # With the global verbose object set to FALSE, a direct vcat call does nothing, and if the foo function
#' # is given TRUE for it's local verbose object, then the 'dog' object is still not printed. As above, 
#' the global variable takes precedence.
#' verbose <- F
#' vcat(dog)
#' foo()
#' foo(verbose = TRUE)
#' 
#' # However if there is no global verbose variable, a local variable will cause it to print
#' rm(verbose)
#' foo(verbose = TRUE) 
#' }
#'
#' @export

vcat <- function(..., sep = " ", bypass = FALSE) {
  
  # if the object verbose does exist as a logical (its a function so would always be true if not specific)
  # or bypass is TRUE then cat the input object
  # otherwise check the ... passed arguments for verbose and cat if true
  if(!is.null(get0("verbose", envir = globalenv(), mode = "logical"))){
    if(get0("verbose", envir = globalenv(), mode = "logical")){
      cat(..., sep = sep)
    }
  }else if(!is.null(parent.frame()$verbose)){
    if(mode(parent.frame()$verbose) == "logical"){
      if(parent.frame()$verbose){
        cat(..., sep = sep)
      }
    }
  }else if(bypass){
    cat(..., sep = sep)
  }
}


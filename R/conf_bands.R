#' @title Confidence bands for forecast comparisons.
#'
#' @description Estimate the skill scores, expected scores, relative accuracy or score differences and generate simultaneous confidence bands of level `1-alpha`.
#'
#'
#' @param scores A nested list containing the scores.The structure has to be the same for all forecasting methods:
#' \itemize{
#' \item{First dimension: Forecasting method m=1,...,M. Even if M=1, use a list structure.}
#' \item{Second dimension: Time points of the evaluation sample t=1,...,N.}
#' \item{Additional dimensions: The amount and the order of additional dimensions can be chosen arbitrarily, for example variables, locations, forecast horizons.}
#' }
#'
#' @param metric A character object that determines which metric should be estimated from the scores. Allowed are `"ES"` (expected scores), `"SS"` (skill scores), `"RA"` (relative accuracy), and `"D"` (difference in expected scores). Default is `"SS"`. Except for `"ES"`, the metric is estimated given the benchmark method `m_bench`.
#' @param type Type of confidence band: `c("bonferroni", "sup-t", "pointwise")`. Default is `"bonferroni"`.
#' @param m_bench Integer between `1` and `M` that indicates the benchmark method. Default is `M`. Parameter has no effect if `metric = "ES"`.
#' @param B Number of bootstrap replications. Default is 1000.
#' @param l Block length used in the moving block bootstrap. Default is `3*floor(N^(1/4))`, where `N` is the size of the evaluation sample.
#' @param alpha Significance level. Default is `0.05`.
#' 
#' @return A nested list containing a vector of length 3 (lower confidence bound, the estimated metric, and the upper bound). The list has the same structure as the input except for:
#' \itemize{
#' \item{The first dimension only includes the remaining forecasting methods, i.e. it is of length `M` if `metric = "ES"`, and of length `M-1` otherwise.}
#' \item{There is no time dimension. Thus, the second dimension of the output list is equal to the third dimension of the input list.}
#' \item{A vector is given at the last level instead of a single number.}
#' }
#' 
#'  
#'
#' @export
#' 



conf_bands <- function(scores, metric = "SS" , m_bench = length(scores), type = "bonferroni", B = 1000, l = 3*floor(length(scores[[1]])^(1/4)), alpha = 0.05){
  
  # Check if inputs are appropriate
  if(typeof(scores) != "list") stop( "scores must be a list of length 1 at least." )
  rapply(scores, function(s){if(length(s)!=1) stop("scores must be a nested list containing a single number for each possible combination.")})
  if(!metric %in% c("ES", "SS", "RA", "D")) stop("Please choose a suitable metric.")
  if(!type %in% c("sup-t", "bonferroni", "pointwise")) stop("Please choose a suitable type.")
  if(length(scores) == 1) if(metric!="ES") stop("The chosen metric is not suitable for M=1.")
  if(!m_bench %in% 1:length(scores)) stop("The index of the benchmark forecast m_bench must be in {1, 2, ..., length(scores)}.")
  if(alpha <= 0 | alpha >= 1) stop("Significance level alpha must be in (0,1).")
  if (l <= 0) stop( "Block length l must be positive." )
  if (l != as.integer(l)) warning( "Block length l is rounded to floor(l)." )
  if (B <= 0) stop( "Number of bootstrap replications B must be positive." )
  if (B != as.integer(B)) warning( "Number of bootstrap replications B is rounded to floor(B)." )
  
  # Check if the list structure is the same for all forecasting methods.
  dim_list <- lapply(scores, function(scores_m) get_dim(scores_m)) # Get dimensions for each forecasting method.
  if(length(scores) > 1){ # if more than one forecasting method
  for(m in 2:length(scores)){ # check if dimensions are the same
    if(setequal(dim_list[[m]], dim_list[[m-1]]) != TRUE)
      stop("The lists of scores don't have the same structure for each forecasting method.")
  }
  }
  
  # Housekeeping
  M <- length(scores) # number of forecasting methods
  N <- length(scores[[1]]) # size of evaluation sample
  m_all <- setNames(1:M, names(scores)) # vector containing the indices of all forecasting methods
  m_rest <-setdiff(m_all, m_bench) # vector containing the indices of all forecasting methods without the benchmark
  names(m_rest) <- names(m_all[c(m_rest)]) # name m_rest
  
  # Vectorize except for the first dimension (forecasting methods)
  s <- lapply(m_all, function(m) matrix(unlist(scores[[m]]), byrow=TRUE, nrow=N)) 
  
  # Define function to estimate the metric
  estimate_ss <- function(s_vectorized, metric){
    
    # Take the average score over time for all dimensions
    sbar <- lapply(m_all, function(m) colMeans(s_vectorized[[m]], na.rm = TRUE))
    
    # Expected Scores
    if(metric == "ES")
      ss <- sbar
    
    # Skill
    if(metric == "SS")
      ss <- lapply(m_rest, function(m) 1 - sbar[[m]]/sbar[[m_bench]])
    
    # Relative Accuracy
    if(metric == "RA")
      ss <- lapply(m_rest, function(m) sbar[[m]]/sbar[[m_bench]])
    
    # Difference in expected scores
    if(metric == "D")
      ss <- lapply(m_rest, function(m) sbar[[m_bench]]-sbar[[m]])
    
    # Output: Vectorize the dimension of forecasting methods as well
    out <- unlist(ss)

  }
  
  # Estimate metric from the original sample
  ss <- estimate_ss(s, metric)
  
  # Moving-block bootstrap (overlapping blocks)
  boot <- lapply(1:B, function(b){
    
    # Draw the ID for setting up N/l blocks with block length l
    id <- sample(1:(N - l + 1), N / l, replace = TRUE) # draw the starting observation for each block
    id <- c(t(sapply(0:(l - 1), function(i) id + i))) # add the subsequent (l-1) observations
    
    # Draw the scores
    s_b <- lapply(m_all, function(m) matrix(s[[m]][id, ], nrow = length(id)))
    
    # Estimate metric in the bootstrap sample
    ss_b <- estimate_ss(s_b, metric)
    
  })
  
  # Convert the bootstrap results into a matrix (B X no. of combined dimensions)
  ss_star <- matrix(unlist(boot), nrow = B, byrow = TRUE)
  
  # Compute empirical standard deviation of skill, i.e. across columns
  sigma_star <- apply(ss_star, 2, sd, na.rm = TRUE)
  
  # Compute maximum of the absolute t-statistics
  max_star <- sapply(1:B, function(b) max(abs(ss_star[b, ] - ss)/sigma_star))
  
  # Determine c: Sup-t
  if(type == "sup-t"){
    # Compute the empirical quantile of max_star (Sup-t)
    c_bs <- quantile(max_star, probs = (1-alpha), na.rm = TRUE) 
  }

  # Determine c: Bonferroni
  if(type == "bonferroni"){
    P <- length(ss)
    c_bs <- qnorm(p = 1-alpha/(2*P))
  }
  
  # Determine c: pointwise
  if(type == "pointwise"){
    c_bs <- qnorm(p = (1-alpha/2))
  }
  
  # Construct confidence bands
  lb_bs <- ss - sigma_star*c_bs # lower bound
  ub_bs <- ss + sigma_star*c_bs # upper bound
  
  # Combine metric with confidence bands
  ss <- rbind(lb_bs, ss, ub_bs)
  
  # Create an empty list with the desired output structure, i.e. a nested list.
  # Select structure w.r.t. how many forecasting methods remain in the output and
  # get rid of the time dimension by choosing t=1.
  if(metric == "ES"){ # M methods
    scores_sel <- lapply(m_all, function(m) scores[[m]][[1]])
  
    } else{ # M-1 methods
    scores_sel <- lapply(m_rest, function(m) scores[[m]][[1]])
    }
  
  # Add placeholders for the confidence bands
  out_structure <- rapply(scores_sel, function(s) setNames(c(NA, NA , NA), c("LB_bs", metric, "UB_bs")), how="list")
  
  # Output
  out <- relist(ss, out_structure)
  
}

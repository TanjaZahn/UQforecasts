######## Helper function: Get dimensions of a (nested) list.
#
# Input: a (nested) list.
#
# Output: A vector containing the length of each of the nested lists.

get_dim <- function(list){
  
  # Create empty vector to collect the dimensions.
  dim <- c()
  
  # Loop to get the dimensions
  list_remain <- list # remaining list
  while(length(list_remain) > 1){ # If there are still dimensions left
    dim <- c(dim, length(list_remain)) # add the length of the list to the dimensions vector
    list_remain <- list_remain[[1]] # disregard the analyzed dimension
  }
  
  # Output
  dim
}
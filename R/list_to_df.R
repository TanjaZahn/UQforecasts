#' @title Convert list to data frame.
#'
#' @description Convert a named list that is possibly nested to a data frame. Can be applied to the output of `conf_bands`, given that the input list had names.
#'
#'
#' @param list A *named* list that may be nested.
#' @param dimnames A character string or a vector of character strings stating the desired column names. The order has to align with the structure of the nested list.
#' @param dims_numeric Optional. A character string or a vector of character strings that contains the columns specified in `dimnames` that should be turned into a numeric column.
#' @param last_level If equal to `"vector"` (default), the very last level of the nested list is a vector. The default is suitable when applying the function to the output of `conf_bands`. Specifying `last_level = "value"` is appropriate when the nested list contains a numeric value at the last level.
#' 
#' 
#' @return A data frame containing all columns specified 
#' 
#'  
#'
#' @export
#' 


list_to_df <- function(list, dimnames, dims_numeric = NULL, last_level = "vector"){
  
  # Unnest list
  unnested <- unlist(list)
  
  # Into data frame (save the names of the unnested elements in a row called "rownames")
  df <- data.frame(unnested) %>% rownames_to_column(.,var = "rowname")
  
  # Note that nested list produced by "conf_bands" of our package contains a vector of length 3 at the very last level, e.g. (LB_bs, SS, UB_bs), 
  # that produces the last part of the element name, for which we will assign the column name "temp" below.
  
  if(last_level == "vector"){
    
    # Split the rowname into multiple columns (assign "temp" for the last part of the element names):
    df <- df %>% separate(rowname, into = c(dimnames, "temp"), sep = "[.]")
    
    # Convert "temp" column into wide format:
    df <- df %>% pivot_wider(names_from = temp, values_from = unnested)
    
  }
  
  if(last_level == "value"){
    
    # Split the rowname into multiple columns
    df <- df %>% separate(rowname, into = c(dimnames), sep = "[.]")
    
  }
  
    # For the columns listed under "dims_numeric". Remove all characters and turn into numeric. 
    if(is.null(dims_numeric) == FALSE)
      for(d in dims_numeric){
        df[[d]] <- as.numeric(gsub("[A-z]", "", df[[d]])) # as.numeric(sub("^.", "", df[[p]]))
      }
    
  # Output
  return(df)
  
}



library(tidyverse)

# Set seed for replication
set.seed(42)

# Parameters
N <- 100 # size of the evaluation sample
H <- 4 # maximum forecast horizons
V <- 3 # number of variables
M <- 2 # number of methods

# Make a data frame
df <- expand.grid(t = 1:N, h = 1:H, v = 1:V)

# Now, we will create some artificial scores for each time period. These could be the squared error, for example. In this case, we simply draw absolute values from a normal distribution.
# The mean is equal to "v + h" for the first method and it is equal to "2 + h" for the second method.
# Thus, the squared error will increase for both forecasting method with the forecast horizon on average. 
# However, method 1 will be better for forecasting variable 1 and method 2 will be better in forecasting variable 3 on average. The performance for variable 2 should be comparable.
df <- df %>% 
  group_by(h, v) %>% 
  mutate(se1 = abs(rnorm(N, v + h, 1)),
         se2 = abs(rnorm(N, 2 + h, 1))) %>% 
  ungroup()

# For convenience, we will transform this into the long format:
df <- df %>% pivot_longer(starts_with("se"),
                          names_to = "m", names_prefix = "se",
                          values_to = "se") %>% 
  mutate(m = as.integer(m))

# For later convenience, we will create named vectors.
methods <- setNames(1:M, c("m1", "m2")) # forecasting methods
t_eval <- setNames(1:N, paste0("t", 1:N)) # periods in evaluation sample
vars <- setNames(1:V, paste0("v", 1:V)) # variables
horizons <- setNames(1:H, paste0("h", 1:H)) # forecast horizon






# # Create some artificial scores, e.g. the squared error of two forecasting methods.
# # The first method produces squared errors that tend to increase with the forecast horizon and the index of the variables.
# # The second method produces, on average, higher squared errors for the first two forecast horizons compared to the other method. It should be better for variable one and worse for variable 3.
# df <- df %>% 
#   group_by(h, v) %>% 
#   mutate(se1 = abs(rnorm(N, v, h)),
#          se2 = abs(rnorm(N, 2, 3))) %>% 
#   ungroup()

# Bring this into the long format
df <- df %>% pivot_longer(starts_with("se"),
                    names_to = "m", names_prefix = "se",
                    values_to = "se") %>% 
  mutate(m = as.integer(m))

# Now, make a nested list containing the scores. First, iterate over methods, then over time periods. The order of the other dimensions (horizons and variables) does not matter.
scores <- lapply(methods, function(mm) lapply(t_eval, function(tt) lapply(vars, function(vv) lapply(horizons, function(hh){
  
  df %>% filter(m == mm, t == tt, v == vv, h == hh) %>% pull(se)
  
}))))

# Estimate the skill score of method 1 with method 2 as benchmark and construct confidence bands
skill <- conf_bands(scores, metric = "SS" , m_bench = length(scores), type = "bonferroni", B = 1000, l = 3*floor(length(scores[[1]])^(1/4)), alpha = 0.05)

# Optional: If the input list 'scores' was named, the output list 'bands' will be named, too. Then, we can use the function 'list_to_df' to transform the output list to a data frame.
# Remember the order of our input list: c("m", "t", "v", "h"). The order of the names specified under 'dimnames' should match this order, without the time periods "t". 
df_skill <- list_to_df(skill, dimnames = c("m", "v", "h"))
df_skill

# To get rid of the prefixes, e.g, in the columns c("v", "h") and turn them into numeric columns, use the argument "dims_numeric" when creating the data frame
df_skill <- list_to_df(skill, dimnames = c("m", "v", "h"), dims_numeric = c("v", "h"))
df_skill

# Now, let's plot the skill score over forecast horizons and fact over variables:
plot_horizontal(df_skill, xvar = h, facet_by = v)





# Create some articifical realizations
df$y <- rnorm(N, 0, 1)

# Create some artificial forecasts
df$f1 <- rnorm(N, 0, 1)
df$f2 <- rnorm(N, 5, 1) # biased

# Calculate the squared error
df$se1 <- 
  
paste0("lapply(", "x", "function(," "xx"
       
       c("t_eval", "h_vec"))

# Turn data frame into a nested list
paste0("lapply(")

lapply(t_vec, function(t) t^2)
       
       
       
       # Creation of sample data frame
       df<-data.frame(
         h=c(1,2,3,4,5),
         variables=c('a','b','c','d','e'),
         value
       )
       
       nested_list<-list()
       for(i in df){
         nested_list<-append(nested_list,list(i))
       }
       
       # What we want
       age_vec <- 1:5
       
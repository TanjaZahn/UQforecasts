library(roxygen2) # In-Line Documentation for R 
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(usethis)  # Automate Package and Project Setup


# Naming the package ---------------------------------------------------------

library(available) # Check if the Title of a Package is Available,
# Appropriate and Interesting
# Check for potential names
available::suggest("Uncertainty quantification in forecast comparisons")

# Check whether it's available
available::available("ACFbands", browse = FALSE)
available::available("UQforecasts", browse = FALSE)

# Don't show specific files ----------------------------------------------------

use_git_ignore("notebook*", directory = ".")

# Importing packages ----------------------------------------------------------

# library(usethis)
use_import_from(package = "tibble", "rownames_to_column") # used in list_to_df
use_package("ggplot2") # for the plots

# library(usethis)  # Automate Package and Project Setup
# use_package("ggplot2")
# use_import_from(package = "mvtnorm", "qmvnorm")
# use_import_from(package = "dplyr", "lead")
# use_import_from(package = "dplyr", "lag")
# use_import_from(package = "stats", "qchisq")
# use_import_from(package = "stats", "qnorm")


# Create descriptions for your functions ----------------------------------------

library(roxygen2) # Read in the roxygen2 R package
roxygenise()      # Builds the help files


# Attach toy data --------------------------------------------------------------

library(tidyverse)

# Set seed for replication
set.seed(42)

# Parameters
N <- 100 # size of the evaluation sample
H <- 4 # maximum forecast horizons
V <- 3 # number of variables
M <- 2 # number of methods

# Make a data frame
df_toy <- expand.grid(t = 1:N, h = 1:H, v = 1:V)

# Create some artificial scores like, e.g., the squared error, for each time period.
df_toy <- df_toy %>% 
  group_by(h, v) %>% 
  mutate(score1 = abs(rnorm(N, v + h, 1)),
         score2 = abs(rnorm(N, 2 + h, 1))) %>% 
  ungroup()

# For convenience, we will transform this into the long format:
df_toy <- df_toy %>% pivot_longer(starts_with("score"),
                                  names_to = "m", names_prefix = "score",
                                  values_to = "score") %>% 
  mutate(m = as.integer(m))

usethis::use_data(df_toy)




# Load the package -------------------------------------------------------------

# Load the package
library(devtools)
load_all(".")


# Check the package ------------------------------------------------------------

# The following function runs a local R CMD check
devtools::check()

# # Check for CRAN specific requirements
# rhub::check_for_cran()
# 
# # Check for win-builder
# devtools::check_win_devel()


# Create a readme file --------------------------------------------------------

usethis::use_readme_rmd()

devtools::build_readme()





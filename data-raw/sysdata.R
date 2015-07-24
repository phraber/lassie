#' Source this from the package directory to build R/sysdata.rda 

source('data-raw/CH505.R')
source('data-raw/aa_colors.R')
source('data-raw/features.R')

devtools::use_data(CH505, aa.col, env.features, internal=T, overwrite=T)

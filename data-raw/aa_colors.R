#' Color Look-up-Table
#' 
#' Used internally by plot_variant_frequency to depict aas or nts by category.
#' Not currently in use but included for completeness.  
aa_colors <- read.csv("color-lut.csv", header=T, stringsAsFactors=F)
devtools::use_data(aa_colors, "lassie", internal=T) #overwrite=T)


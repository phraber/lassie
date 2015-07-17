#' Color Look-up-Table
#' 
#' Used internally by plot_variant_frequency to depict aas or nts by category.
aa_colors <- system.file("extdata", "color-lut.csv", package="lassie")
aa.col <-read.csv(aa_colors, header=T, stringsAsFactors=F, strip.white=T)
devtools::use_data(aa.col, internal=T, overwrite=T)

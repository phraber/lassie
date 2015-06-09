#' @keywords internal
if.verbose.print <- function(output_string, is_verbose) {
    if (is_verbose) 
	cat(paste0(output_string, '\n'))
}

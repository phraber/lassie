#' verify that the input alignment consists of amino acids, using relative frequency of iupac nts
#' this logic is adapted from Will Fischer's pixel routine... but were did he get it?
#'
#' @keywords internal
is.protein <- function(alignment_matrix) {

    if (any(is.na(c(alignment_matrix))))
        stop("ERROR in is_protein(): missing values are not permitted") 

    if (!is.matrix(alignment_matrix))
        stop("ERROR in is_protein(): Matrix required")

    if (!is.character(alignment_matrix))
        stop("ERROR in is_protein(): Character matrix required") 

    digits = 0
    total = 0
    nts = 0
    aas = 0
#    us = 0
#    ts = 0

    for (i in 1:nrow(alignment_matrix)) {
        total = total + length(which(grepl("[A-Z0-9]", alignment_matrix[i, ], ignore.case=T)))
        digits = digits + length(which(grepl("[0-9]", alignment_matrix[i, ])))
        nts = nts + length(which(grepl("[ACGTUNRYWSMKDHBV]", alignment_matrix[i, ], ignore.case=T)))
        aas = aas + length(which(grepl("[BFLIMVSPTAYHQNKDECWRGOXZ*$]", alignment_matrix[i, ], ignore.case=T)))
#        us = us + length(which(grepl("[U]", alignment_matrix[i, ], ignore.case=T)))
#        ts = ts + length(which(grepl("[T]", alignment_matrix[i, ], ignore.case=T)))
     }

    what_is_it = ifelse(digits/total > 0.3, "std",
	ifelse(aas/nts > 1.2, "pro" , "nuc"))

#if (what_is_it == "nuc") # could distinguish between dna and rna but it's not needed here.

    what_is_it == "pro"
}

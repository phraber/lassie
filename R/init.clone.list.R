

#' @keywords internal
init.clone.list <- function(sequence_matrix) {
# returns named vector of F values that corresponds to rows in sequence_matrix
    is_included <- rep(F, nrow(sequence_matrix))
    names(is_included) <- rownames(sequence_matrix)

    is_included
}

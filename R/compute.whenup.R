#' @keywords internal
compute.whenup <- function(tf_loss_matrix, how.far.up = 33) {

# in earlier versions, whenup is defined as when a mutant is more
# common than the TF form.  this cannot be determined from
# tf_loss_matrix alone, as multiple mutants may be present so
# f(mutant) + f(TF) < 1

## here we simply report when the mutant first exceeds some cutoff
## percentage, by default 33% BUT
### in fact, we are not considering when a particular mutant is "up"
### but rather when the tf loss is down to 100 - the cutoff percentage

# cf. compute.tf.loss():
# tf_loss matrix has one row per site and one column per timepoint sampled

    whenup = rep(NA, nrow(tf_loss_matrix))

    for (i in 1:nrow(tf_loss_matrix))
        if (any(tf_loss_matrix[i, ] >= how.far.up))
            whenup[i] = 
                colnames(tf_loss_matrix)[ min(which(tf_loss_matrix[i, ] 
                                                   >= how.far.up), na.rm=T)]

    as.numeric(gsub("[A-Z]", "", whenup, ignore.case=T))
}

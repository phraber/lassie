context("report.variant.frequencies")

# Example alignment with a single varying site
seqs <- c("0.0"="MRKPIH",
          "1.0"="MMKPIH", "1.1"="MMKPIH", "1.2"="MRKPIH", "1.3"="MRKPIH",
          "2.0"="MMKPIH", "2.1"="MMKPIH", "2.2"="MKKPIH", "2.3"="MKKPIH",
          "3.0"="MMKPIH", "3.1"="MMKPIH", "3.2"="MKKPIH", "3.3"="MKKPIH")
test_aln <- matrix(unlist(sapply(seqs, strsplit, split="")),
              nrow=length(seqs),
              byrow=TRUE)
rownames(test_aln) <- names(seqs)
rm(seqs)

test_that("plots with no errors", {
    A <- lassie::swarmtools(aas_aln = test_aln, tf_loss_cutoff=80)
    # Don't save plot image (https://stackoverflow.com/a/24762900)
    pdf(file = NULL)
    report.variant.frequencies(A)
    dev.off()
})

test_that("plots with no errors for single-variant case", {
    # replace one site with a single AA, but different from the TF
    test_aln[2:nrow(test_aln), 2] <- "M"
    A <- lassie::swarmtools(aas_aln = test_aln, tf_loss_cutoff=80)
    pdf(file = NULL)
    report.variant.frequencies(A)
    dev.off()
})
context("swarmtools")

test_that("gives expected results for number of selected sites", {

# identical?
    A <- lassie::swarmtools(tf_loss_cutoff=80,
	aas_file=system.file("extdata", "CH505-gp160.fasta", 
	    package="lassie"))

    expect_equal(nrow(A$selected_sites), 35)

    rm (A)
})

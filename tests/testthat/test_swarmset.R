context("swarmset")

test_that("gives expected results for selected sequences", {

    expect_equal(seqinr::read.fasta(lassie::export.fasta.swarmset(
        suppressMessages(lassie::swarmset(
	    lassie::swarmtools(tf_loss_cutoff=80,
		aas_file=system.file("extdata", "CH505-gp160.fasta", 
		    package="lassie")))), tempfile())),
        seqinr::read.fasta(system.file("extdata", "CH505-swarmset.fasta", 
            package="lassie")))
})
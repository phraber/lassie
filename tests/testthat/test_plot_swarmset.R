context("plot.swarmset")

test_that("plot.swarmset creates image file output", {
    
    # This is only a partial test since it just checks if the expected output 
    # file exists.  But, if weblogo doesn't get called as expected or crashes
    # the test will at least detect that.
    ss <- lassie::swarmset(lassie::swarmtools(tf_loss_cutoff=80,
            aas_file=system.file("extdata",
                                 "CH505-gp160.fasta",
                                 package="lassie")))
    file_path <- lassie::plot.swarmset(ss)
    expect_true(file.exists(file_path))
})
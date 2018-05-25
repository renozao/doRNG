# Unit tests for the package doRepro
# 
# Author: renaud gaujoux
# Creation: 30 Jun 2011
###############################################################################

context("Reproducibility")

test_that('xapply', {
	
	skip("Development is not finished yet")
	
	.seed <- 1:6
	n <- 3
	
	expect_identical(xapply(1:n, .seed, function(i) i), 1:n, "Main argument is correctly passed")
	expect_identical(xapply(1:n, .seed, function(i, b){b}, b='a'), rep('a',n), "Other arguments are correctly passed (1)")
	expect_identical(xapply(1:n, .seed, function(i, b, c){c}, c='a'), rep('a',n), "Other arguments are correctly passed (2)")
	
	rngs <- sapply(RNGseq(n, .seed), RNGdigest)
	expect_identical(xapply(1:n, .seed, function(i){ RNGdigest() }), rngs, "RNG are correctly set")
	
	# check that the stream seed is restored
	rngs <- sapply(RNGseq(n-1, .seed), RNGdigest)
	res <- xapply(1:n, .seed, function(i){ RNGdigest() })
	rngs <- cbind(rngs, RNGdigest())
	expect_identical(res, rngs, "RNG are correctly set")
	
	# results are reproducible
	orng <- getRNG()
	res <- xapply(1:n, .seed, function(i) runif(i) )
	expect_true( rng.equal(orng), "RNG is restored")
	expect_identical(res, xapply(1:n, .seed, function(i) runif(i) ), "Results are reproducible")
	expect_identical(sapply(res, length), 1:n, "Test results have correct dimension")
	
})

test_that("reproduce", {
	
	skip("Development is not finished yet")
	
	.seed <- 1:6
	n <- 3
	p <- 5
	
	rngs <- sapply(RNGseq(3, .seed), RNGdigest)
	expect_identical(reproduce(n, .seed, RNGdigest()), rngs, "RNG are correctly set")
	
	# results are reproducible
	orng <- getRNG()
	res <- reproduce(n, .seed, runif(p) )
	expect_true( rng.equal(orng), "RNG is restored")
	expect_identical(res, reproduce(n, .seed, runif(p) ), "Results are reproducible")
	expect_identical(dim(res), as.integer(c(p,n)), "Test results have correct dimension")
  
})


# Unit test for doRNG
# 
# Author: Renaud Gaujoux
# Creation: 28 Mar 2012
###############################################################################

context('dorng')
#test.CMRGseed <- function(){
#	
#	msg <- function(...) paste(.msg, ':', ...)
#	
#	# Unit tests
#	.msg <- "Call CMRGseed without argument"
#	rs <- .Random.seed
#	expect_identical( length(CMRGseed()), 7L, msg("Seed is of length 7") )
#	expect_identical(rs, .Random.seed, msg("does not change .Random.seed"))
#	
#	.msg <- "Call CMRGseed with a single argument"
#	rs <- .Random.seed
#	expect_identical( length(CMRGseed(1)), 7L, msg("Seed is of length 7") )
#	expect_identical(rs, .Random.seed, msg("does not change .Random.seed"))
#	expect_true( all(!is.na(CMRGseed(1))), msg("No NA in the returned seed") )
#	
#}

checkRNG <- function(x, y, msg = NULL, ...){
  expect_true(rng.equal(x, y), info = msg, ...)
}

# 1-length loop
test_that("dorng1", {
    
    set.seed(1234) # needed to avoid weird behaviors in checks
    rng_seed <- RNGseq(n = 1, seed = 123, simplify = FALSE)[[1L]]
    set.seed(123)
    x <- foreach(i=1) %dorng% { runif(1) }
    y <- foreach(i=1, .options.RNG = 123) %dorng% { runif(1) }
    expect_identical(x, y)
    # check attributes on results
    result_attributes <- attributes(x)
    expect_true(setequal(names(result_attributes), c("rng", "doRNG_version")),
                info = "Result has all the expected attributes")
    expect_identical(result_attributes[["rng"]][[1L]], rng_seed, 
                     info = "Attribute 'rng' does not have the expected value")
    expect_identical(result_attributes[["doRNG_version"]], doRNGversion(),
                     info = "Attribute 'doRNG_version' does not have the expected value")
    
})

test_that("dorng", {
	
		test_dopar <- function(.msg, s.seq){
			
			orng <- RNGseed()
			on.exit({ doRNGversion(NULL); RNGseed(orng); registerDoSEQ()} )
			
			msg <- function(...) paste(.msg, ':', ...)
			noattr <- function(x){ attributes(x) <- NULL; x}
			
            # RNG restoration after %dorng%
            rng0 <- getRNG()
            foreach(i=1:4, .options.RNG = 123) %dorng% { runif(1) }
            checkRNG(rng0, msg = "RNG is restored after seeded %dorng%")
            #
            foreach(i=1:4) %dorng% { runif(1) }
            expect_identical(RNGtype(), RNGtype(rng0), info = "RNG kind is restored after unseeded %dorng%") 
        
			# standard %dopar% loops are _not_ reproducible
			set.seed(1234)
			s1 <- foreach(i=1:4) %dopar% { runif(1) }
			set.seed(1234)
			s2 <- foreach(i=1:4) %dopar% { runif(1) }
			if( !missing(s.seq) ) expect_true( !identical(s1, s2), msg("Standard %dopar% loop is not reproducible"))
			
			# %dorng% loops ensure reproducibility
			local({
				set.seed(1234)
				s1 <- foreach(i=1:4) %dorng% { runif(1) }
				runif(10)
				set.seed(1234)
				s2 <- foreach(i=1:4) %dorng% { runif(1) }
				expect_identical(s1, s2, msg("%dorng% loop is reproducible with set.seed"))
				# check RNG settings in result
				set.seed(1234)
				ref <- RNGseq(4)
				rngs <- attr(s1, 'rng')
				expect_true(!is.null(rngs), msg("Results contains RNG data"))
				expect_identical(rngs, ref, msg("Results contains whole sequence of RNG seeds"))
			})
			
			# or 
			local({
				s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
				s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
				expect_identical(s1, s2, msg("%dorng% loop is reproducible with .options.RNG"))
				ref <- RNGseq(4, 1234)
				rngs <- attr(s1, 'rng')
				expect_true(!is.null(rngs), msg("Results of seed loop contains RNG data"))
				expect_identical(rngs, ref, msg("Results of seeded loop contains whole sequence of RNG seeds"))
			})
			
			# check with unamed foreach arguments (issue #8)
			local({
			  on.exit( registerDoSEQ() )
			  registerDoRNG()
			  set.seed(567)
			  res <- foreach(a = 1:4, .combine = 'c') %dopar% {rnorm(1, mean = 0, sd = 1)}
			  set.seed(567)
			  res2 <- foreach(1:4, .combine = 'c') %dopar% {rnorm(1, mean = 0, sd = 1)}
			  expect_identical(res, res2, info = "First argument named or unamed is equivalent")
			  #
			  set.seed(567)
			  res <- foreach(a = 1:4, 1:2, .combine = 'c') %dopar% {rnorm(1, mean = 0, sd = 1)}
			  set.seed(567)
			  res2 <- foreach(1:4, 1:2, .combine = 'c') %dopar% {rnorm(1, mean = 0, sd = 1)}
			  expect_identical(res, res2, info = "First argument named or unamed, with second unamed argument is equivalent")
			  
			})
			##
			
			## check extra arguments to .options.RNG
			# Normal RNG parameter is taken into account
			s.unif.noNk <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(5) }
			s.unif.wNk <- foreach(i=1:4, .options.RNG=list(1234, normal.kind="Ahrens-Dieter")) %dorng% { runif(5) }
			expect_identical(noattr(s.unif.noNk), noattr(s.unif.wNk)
				, msg("%dorng% loop (runif) with normal.kind in .options.RNG is identical as without"))
		
			s.norm.noNk <- foreach(i=1:4, .options.RNG=1234) %dorng% { rnorm(5) }
			s.norm.wNk <- foreach(i=1:4, .options.RNG=list(1234, normal.kind="Ahrens-Dieter")) %dorng% { rnorm(5) }
			expect_true(!isTRUE(all.equal(noattr(s.norm.noNk), noattr(s.norm.wNk)))
				, msg("%dorng% loop (rnorm) with normal.kind in .options.RNG is different as without"))			

			# reproduce previous loop
			runif(10)
			s1 <- foreach(i=1:4) %dorng% { runif(5) }
			s2 <- foreach(i=1:4, .options.RNG=s1) %dorng% { runif(5) }
			expect_identical(s1, s2, "Seeding using .options.RNG={result from other loop} give identical results")
			# directly set sequence of seeds
			sL <- list(c(407,1:6), c(407,11:16), c(407,21:26), c(407, 31:36))
			s1.L <- foreach(i=1:4, .options.RNG=sL) %dorng% { runif(5) }
			runif(10)
			s2.L <- foreach(i=1:4, .options.RNG=sL) %dorng% { runif(5) }
			expect_identical(s1.L, s2.L, "Seeding using .options.RNG=list twice give identical results")
			# directly set sequence of seeds as a matrix
			sM <- sapply(sL, identity)
			s1.M <- foreach(i=1:4, .options.RNG=sM) %dorng% { runif(5) }
			runif(10)
			s2.M <- foreach(i=1:4, .options.RNG=sM) %dorng% { runif(5) }
			expect_identical(s1.M, s2.M, "Seeding using .options.RNG=matrix twice give identical results")
			expect_identical(s1.M, s1.L, "Seeding using .options.RNG=matrix gives identical results as the same seed in list")
			
			# separate %dorng% loops are different
			set.seed(1234)
			rs1 <- .Random.seed
			s1 <- foreach(i=1:4) %dorng% { runif(1) }
			rs1_2 <- .Random.seed
			s2 <- foreach(i=1:4) %dorng% { runif(1) }
			expect_true( !identical(rs1, rs1_2), msg("unseed %dorng% loop changes .Random.seed"))
			expect_true( !identical(s1, s2), msg("two consecutive (unseeded) %dorng% loops are not identical"))
			expect_true( !identical(unlist(s1), unlist(s2)), msg("two consecutive (unseeded) %dorng% loops are not identical (values)"))
			
			# But the whole sequence of loops is reproducible
			set.seed(1234)
			s1.2 <- foreach(i=1:4) %dorng% { runif(1) }
			s2.2 <- foreach(i=1:4) %dorng% { runif(1) }
			expect_true( identical(s1, s1.2) && identical(s2, s2.2), msg("set.seed + two consecutive %dorng% loops are reproducible"))
			
			s <- list(s1, s2)
			if( !missing(s.seq) )
				expect_identical(s, s.seq, msg("result is identical to sequential computation"))
			
			# check behaviour with set.seed
			set.seed(789)
			s1 <- foreach(i=1:6) %dorng%{ runif(1) }
			s2 <- foreach(i=1:6, .options.RNG=789) %dorng%{ runif(1) }
			expect_identical(s1, s2, "set.seed before %dorng% is identical to using .options.RNG")
			# current RNG is CRMG
			set.seed(789, "L'Ec")
			s1 <- foreach(i=1:6) %dorng%{ runif(1) }
			s2 <- foreach(i=1:6, .options.RNG=789) %dorng%{ runif(1) }
			expect_identical(s1, s2, "set.seed before %dorng% is identical to using .options.RNG, if current RNG is CRMG")

			s
		}
		
		orng <- RNGseed()
		on.exit({ doRNGversion(NULL); RNGseed(orng); registerDoSEQ()} )
		
		library(doParallel)
		
		# Sequential computation
		registerDoSEQ()
		s.seq <- test_dopar("Sequential")
		
		# Multicore cluster
    if( .Platform$OS.type != 'windows'){
      # Note: for some reason, running this test in RStudio fails when checking that the standard
      # %dopar% loop is not reproducible
		  registerDoParallel(cores=2)
		  s <- test_dopar("Multicore", s.seq)
    }
		
		# SNOW-like cluster
		cl <- makeCluster(2)
		on.exit( if( !is.null(cl) ) stopCluster(cl), add = TRUE)
		registerDoParallel(cl)
		test_dopar("SNOW-like cluster", s.seq)
		stopCluster(cl); cl <- NULL
		
    skip("doMPI test because doMPI::startMPIcluster hangs inexplicably")
		# Works with doMPI
		if( require(doMPI) ){
			cl_mpi <- startMPIcluster(2)
			on.exit( if( !is.null(cl_mpi) ) closeCluster(cl_mpi), add = TRUE)
			registerDoMPI(cl_mpi)
			test_dopar("MPI cluster", s.seq) 
			closeCluster(cl_mpi); cl_mpi <- NULL
		}
})

test_that("registerDoRNG", {
	
  orng <- RNGseed()
	on.exit({ doRNGversion(NULL); RNGseed(orng); registerDoSEQ()} )
		
    # RNG restoration after %dorng% over doSEQ
    registerDoSEQ()
    registerDoRNG()
    set.seed(123)
    rng0 <- getRNG()
    res1 <- foreach(i=1:4) %dorng% { runif(1) }
    expect_identical(RNGtype(), RNGtype(rng0), "RNG kind is restored after unseeded %dorng%")
    set.seed(123)
    res2 <- foreach(i=1:4) %dorng% { runif(1) } 
    expect_identical(res1, res2, "%dorng% loop over doSEQ are reproducible")
    
    
	on.exit( if( !is.null(cl) ) stopCluster(cl), add = TRUE)
	library(doParallel)
	cl <- makeCluster(2)
	registerDoParallel(cl)
	
	# One can make existing %dopar% loops reproducible using %dorng% loops or registerDoRNG  
	set.seed(1234)
	r1 <- foreach(i=1:4) %dorng% { runif(1) }
	registerDoRNG()
	set.seed(1234)
	r2 <- foreach(i=1:4) %dopar% { runif(1) }
	expect_identical(r1, r2, "registerDoRNG + set.seed: makes a %dopar% loop behave like a set.seed + %dorng% loop")
	stopCluster(cl); cl <- NULL
	
	# Registering another foreach backend disables doRNG
	cl2 <- makeCluster(2)
	on.exit( if( !is.null(cl2) ) stopCluster(cl2), add = TRUE)
	registerDoParallel(cl2)
	set.seed(1234)
	s1 <- foreach(i=1:4) %dopar% { runif(1) }
	set.seed(1234)
	s2 <- foreach(i=1:4) %dorng% { runif(1) }
	expect_true( !identical(s1, s2), "Registering another foreach backend disables doRNG")
	
	# doRNG is re-nabled by re-registering it 
	registerDoRNG()
	set.seed(1234)
	r3 <- foreach(i=1:4) %dopar% { runif(1) }
	expect_identical(r2, r3, "doRNG is re-nabled by re-registering it")
	r4 <- foreach(i=1:4) %dopar% { runif(1) }
	# NB: the results are identical independently of the task scheduling
	# (r2 used 2 nodes, while r3 used 3 nodes)
	
	# Reproducibility of sequences of loops
	# pass seed to registerDoRNG 
	runif(10)
	registerDoRNG(1234)
	r1 <- foreach(i=1:4) %dopar% { runif(1) }
	r2 <- foreach(i=1:4) %dopar% { runif(1) }
	registerDoRNG(1234)
	r3 <- foreach(i=1:4) %dopar% { runif(1) }
	r4 <- foreach(i=1:4) %dopar% { runif(1) }
	expect_identical(r3, r1, "registerDoRNG(1234) allow reproducing sequences of %dopar% loops (1)")
	expect_identical(r4, r2, "registerDoRNG(1234) allow reproducing sequences of %dopar% loops (2)")	
	# use set.seed
	runif(10)
	registerDoRNG()
	set.seed(1234)
	s1 <- foreach(i=1:4) %dopar% { runif(1) }
	s2 <- foreach(i=1:4) %dopar% { runif(1) }
	set.seed(1234)
	s3 <- foreach(i=1:4) %dopar% { runif(1) }
	s4 <- foreach(i=1:4) %dopar% { runif(1) }
	expect_identical(s3, s1, "registerDoRNG + set.seed(1234) allow reproducing sequences of %dopar% loops (1)")
	expect_identical(s4, s2, "registerDoRNG + set.seed(1234) allow reproducing sequences of %dopar% loops (2)")
	
	runif(5)
	registerDoRNG()
	set.seed(1234)
	s5 <- foreach(i=1:4) %dopar% { runif(1) }
	s6 <- foreach(i=1:4) %dopar% { runif(1) }
	expect_identical(s5, r3, "registerDoRNG() + set.seed give same results as registerDoRNG(1234) (1)")
	expect_identical(s6, r4, "registerDoRNG() + set.seed give same results as registerDoRNG(1234) (2)")
	
	# argument `once=FALSE` reseed doRNG's seed at the beginning of each loop 
	registerDoRNG(1234, once=FALSE)
	r1 <- foreach(i=1:4) %dopar% { runif(1) }
	r2 <- foreach(i=1:4) %dopar% { runif(1) }
	r3 <- foreach(i=1:4, .options.RNG=1234) %dopar% { runif(1) }
	expect_identical(r1, r2, "argument `once=FALSE` reseed doRNG's seed at the beginning of each loop")
	expect_identical(r1, r3, "argument `once=FALSE` reseed %dorng% loop as .options.RNG")
	
	# Once doRNG is registered the seed can also be passed as an option to %dopar%
	r1.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
	r2.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }	
	expect_identical(r1.2, r2.2, "Once doRNG is registered the seed can also be passed as an option to %dopar%")
	expect_true(!identical(r1.2, r1), "The seed passed as an option is really taken into account")
	
})

# Test the use-case discussed in https://github.com/renozao/doRNG/issues/12
# Note: when run under RStudio, this
test_that("Initial RNG state is properly handled", {
  
  # write script that loads the package being tested
  .run_test_script <- function(version){
    pkg_path <- path.package("doRNG")
    lib_path <- dirname(pkg_path)
    # determine if the package is a development or installed package
    if( dir.exists(file.path(pkg_path, "Meta")) ) load_cmd <- sprintf("library(doRNG, lib = '%s')", lib_path)
    else load_cmd <- sprintf("devtools::load_all('%s')", pkg_path)
    # results are saved in a temporary .rds file (substitute backslashes with forward slash for Windows)
    tmp_res <- gsub("\\", "/", tempfile("rscript_res_", fileext = ".rds"), fixed = TRUE)
    on.exit( unlink(tmp_res) )
    
    r_code <- paste0(collapse = "; ", 
                    c(load_cmd,
                    if( !is.null(version) ) sprintf("doRNGversion('%s')", version),
                    "r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }",
                    "r2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }",
                    "r3 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }",
                    sprintf("saveRDS(list(r1 = r1, r2 = r2, r3 = r3), '%s')", tmp_res)
                    )
    )
    
    # run code in an independent fresh session 
    rscript <- file.path(R.home("bin"), "Rscript")
    system(sprintf('"%s" -e "%s"', rscript, r_code), ignore.stdout = TRUE, ignore.stderr = TRUE)
    # load result
    readRDS(tmp_res)
    
  }
  # pre-1.7.4: results are not-reproducible
  res <- .run_test_script("1.7.3")
  expect_true(is.list(res) && identical(names(res), paste0("r", 1:3)))
  expect_true(all(sapply(res, attr, 'doRNG_version') == "1.7.3"), 
              info = "doRNG version is correctly stored")
  expect_true(!identical(res[["r1"]], res[["r2"]]), info = "Version pre-1.7.3: results 1 & 2 are not identical")
  expect_identical(res[["r3"]], res[["r2"]], info = "Version pre-1.7.3: results 2 & 3 are identical")
  
  # post-1.7.4: results are reproducible
  res <- .run_test_script("1.7.4")
  expect_true(is.list(res) && identical(names(res), paste0("r", 1:3)))
  expect_true(all(sapply(res, attr, 'doRNG_version') == "1.7.4"), 
              info = "doRNG version is correctly stored")
  expect_identical(res[["r1"]], res[["r2"]], info = "Version 1.7.4: results 1 & 2 are identical")
  expect_identical(res[["r3"]], res[["r2"]], info = "Version 1.7.4: results 3 & 3 are identical")
  
  # current version: results are reproducible
  res <- .run_test_script(NULL)
  expect_true(is.list(res) && identical(names(res), paste0("r", 1:3)))
  expect_true(all(sapply(res, attr, 'doRNG_version') == doRNGversion()), 
              info = "doRNG version is correctly stored")
  expect_identical(res[["r1"]], res[["r2"]], info = "Current version: results 1 & 2 are identical")
  expect_identical(res[["r3"]], res[["r2"]], info = "Current version: results 2 & 3 are identical")
  
})

test_that("RNG warnings", {
  
  .local <- function(){
    
    orng <- RNGseed()
    oo <- options()
		on.exit({ options(oo); doRNGversion(NULL); RNGseed(orng); registerDoSEQ()} )
			
    registerDoSEQ()
    registerDoRNG()
    expect_warning(y <- foreach(x = 1:2) %dorng% { rnorm(1); x }, NA)
    options(doRNG.rng_change_warning_force = TRUE)
    expect_warning(y <- foreach(x = 1:2) %dorng% { rnorm(1); x }, 
                   "Foreach loop \\(doSEQ\\) .* changed .* RNG type")
    options(doRNG.rng_change_warning_force = NULL)
    
    on.exit( if( !is.null(cl) ) stopCluster(cl), add = TRUE)
  	library(doParallel)
  	cl <- makeCluster(2)
  	registerDoParallel(cl)
  	
    options(doRNG.rng_change_warning_force = TRUE)
    options(doRNG.rng_change_warning_skip = "doParallelSNOW")
    expect_warning(y <- foreach(x = 1:2) %dorng% { rnorm(1); x },
                  "Foreach loop \\(doParallelSNOW\\) .* changed .* RNG type")
    options(doRNG.rng_change_warning_force = NULL)
    expect_warning(y <- foreach(x = 1:2) %dorng% { rnorm(1); x }, NA)
    
    options(doRNG.rng_change_warning_skip = NULL)
  }
  .local()
  
})

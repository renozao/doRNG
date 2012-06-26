# Development of a dorng equivalent to dopar for reproducible loops
# 
# Author: Renaud Gaujoux
# Creation: 17 Aug 2011
###############################################################################

# Check if an object is NA
isNA <- function(x) identical(x, NA) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))

#' Local RNG scope
#' 
#' Save and restore RNG settings enabling their local changes. 
#' 
#' @param seed numeric (integer) vector used to restore the RNG settings.
#' Typically the result of a call to \code{RNGscope()}.
#' @return a numeric (integer) vector  containing the current value of 
#' \code{\link{.Random.seed}} or \code{NULL}. 
#' 
#' @export
#' @seealso \code{\link{.Random.seed}}
#' 
#' @examples 
#' 
#' f <- function(){
#' 	orng <- RNGscope()
#'  on.exit(RNGscope(orng))
#' 	RNGkind('Marsaglia')
#' 	runif(10)
#' }
#' 
#' sample(NA)
#' s <- .Random.seed
#' f()
#' identical(s, .Random.seed)
#' \dontshow{ stopifnot(identical(s, .Random.seed)) }
#'  
RNGscope <- function(seed){
	
	res <- if( missing(seed) ){
				if( exists('.Random.seed', where = .GlobalEnv) )
					get('.Random.seed', .GlobalEnv)
			}else if( is.null(seed) ){
				if( exists('.Random.seed', where = .GlobalEnv) )
					rm('.Random.seed', envir = .GlobalEnv)
			}else{
				assign('.Random.seed', seed, .GlobalEnv)
			}
	invisible(res)
}

.collapse <- function(x, n=length(x), sep=', '){
	
	res <- paste(if( missing(n) ) x else head(x, n), collapse=', ')
	if( length(x) > n )
		res <- paste(res, '...', sep=', ')
	res
}

#' Setting the Random Seed
#' 
#' Directly sets the value of \code{\link{.Random.seed}} in the global 
#' environment, and returns its old value -- invisible.
#' 
#' @param seed integer vector that is a suitable value for \code{.Random.seed}, or 
#' a single numeric value passed to \code{set.seed}.
#' @param ... extra arguments passed to \code{\link{set.seed}} if seed is a 
#' single numeric value.   
#' 
#' @export
setRNG <- function(seed, ...){
			
	# get/restore .Random.seed on.exit in case of errors
	orseed <- RNGscope()
	on.exit({
		message("Restoring RNG settings probably due to an error in setRNG")
		RNGscope(orseed) 
	})
	
	seed <- as.integer(seed)
	if( length(seed) == 1L ){
		set.seed(seed, ...)
	}else{			
		assign('.Random.seed', seed, envir=.GlobalEnv)
		# check validity of the seed
		tryCatch(runif(1)
		, error=function(err){					
			stop("setRNG - Invalid value for .Random.seed ["
					, .collapse(seed, n=5), "]: ", err$message, '.'
					, call.=FALSE)
		})
		assign('.Random.seed', seed, envir=.GlobalEnv)			
	}
	
	# cancel RNG restoration
	on.exit()
				
	# return old RNG as invisible		
	invisible(orseed)
}

#' Back Compatibility Option for doRNG
#' 
#' Sets the behaviour of \%dorng\% foreach loops from a
#' given version number.
#' 
#' @section Behaviour changes in versions:
#' 
#' \describe{
#' \item{1.4}{ The behaviour of \code{doRNGseed}, and therefore of 
#' \code{\%dorng\%} loops, changed in the case where the current RNG was 
#' L'Ecuyer-CMRG.
#' Using \code{set.seed} before a non-seeded loop used not to be identical 
#' to seeding via \code{.options.RNG}.
#' Another bug was that non-seeded loops would share most of their RNG seed!
#' }
#' }
#' 
#' @param x version number to switch to, or missing to get the currently 
#' active version number, or \code{NULL} to reset to the default behaviour, 
#' i.e. of the latest version.
#' 
#' @return a character string
#' If \code{x} is missing this function returns the version number from the 
#' current behaviour.
#' If \code{x} is specified, the function returns the old value of the 
#' version number (invisible).
#' 
#' @importFrom utils packageVersion
#' @export
#' @examples 
#'
#' \dontshow{ registerDoSEQ() }
#' 
#' ## Seeding when current RNG is L'Ecuyer-CMRG
#' RNGkind("L'Ecuyer")
#' 
#' # in version >= 1.4 seeding behaviour changed to fix a bug
#' set.seed(123)
#' res <- foreach(i=1:3) %dorng% runif(1)
#' res2 <- foreach(i=1:3) %dorng% runif(1)
#' stopifnot( !identical(attr(res, 'rng')[2:3], attr(res2, 'rng')[1:2]) )
#' res3 <- foreach(i=1:3, .options.RNG=123) %dorng% runif(1)
#' stopifnot( identical(res, res3) )
#' 
#' # buggy behaviour in version < 1.4
#' doRNGversion("1.3")
#' res <- foreach(i=1:3) %dorng% runif(1)
#' res2 <- foreach(i=1:3) %dorng% runif(1)
#' stopifnot( identical(attr(res, 'rng')[2:3], attr(res2, 'rng')[1:2]) )
#' res3 <- foreach(i=1:3, .options.RNG=123) %dorng% runif(1)
#' stopifnot( !identical(res, res3) )
#' 
#' # restore default RNG  
#' RNGkind("default")
#' 
doRNGversion <- local({

	currentV <- "1.4" #as.character(packageVersion('doRNG')) 
	cache <- currentV
	function(x){
		if( missing(x) ) return(cache)
		if( is.null(x) ) x <- currentV 
		
		# update cache and return old value
		old <- cache
		cache <<- x
		invisible(old)
	}
})

#' @importFrom utils compareVersion
checkRNGversion <- function(x){
	compareVersion(doRNGversion(), x)
}

#' Creating Initial Seed for Random Streams
#' 
#' This function generate the next random seed used to set the RNG of first 
#' %dorng% loop iteration. 
#' The seeds for the subsequent iterations are then obtained with the 
#' \code{\link{nextRNGStream}} function.
#' 
#' @param seed a single or 6-length numeric. If missing then the current seed is 
#' returned.
#' @param normal.kind Type of Normal random generator. See \code{\link{RNG}}.
#' @param verbose logical to toggle verbose messages
#' 
#' @return a 7-length numeric vector.
#' @seealso \code{\link{RNGseq}}
#' 
#' @export
#' @examples 
#' 
#' ## generate a seed for %dorng% loops
#' # random  
#' doRNGseed() 
#' doRNGseed()
#' doRNGseed(NULL)
#' # fixed
#' doRNGseed(1)
#' doRNGseed(1:6)
#' 
#' # `doRNGseed(1)` is identical to 
#' set.seed(1)
#' doRNGseed()
#'  
doRNGseed <- function(seed=NULL, normal.kind=NULL, verbose=FALSE){
		
	# retrieve current seed
	orng <- RNGscope()
	# setup RNG restoration in case of an error
	on.exit({
		RNGscope(orng)
		if( verbose ) message("# Current RNGkind was reset to: ", paste(RNGkind(), collapse=' - '), ' [', .collapse(orng), ']')
	})

	rkind_not_CMRG <- RNGkind()[1L] != "L'Ecuyer-CMRG"
	
	if( verbose ) message("# Original RNGkind is: ", paste(RNGkind(), collapse=' - '), ' [', .collapse(orng, 7), ']')
	# seed with numeric seed
	if( is.numeric(seed) ){
		if( length(seed) == 1L ){
			
			if( verbose ) message("# Generate RNGstream random seed from ", seed, " ... ", appendLF=FALSE)
			if( checkRNGversion("1.4") < 0 || rkind_not_CMRG ){ # behaviour prior 1.4
				set.seed(seed)
				RNGkind(kind="L'Ecuyer-CMRG", normal.kind=normal.kind)
			}else{ # fix seed after switching to CMRG: ensure result independence from the current RNG
				set.seed(seed, kind="L'Ecuyer-CMRG", normal.kind=normal.kind)
			}
			if( verbose ) message("OK")
		}
		else if( length(seed) == 6L ){
			if( verbose ) message("# Directly use numeric seed: ",  paste(seed, collapse=', '), " ... ", appendLF=FALSE)
			RNGkind("L'Ecuyer-CMRG", normal.kind=normal.kind)
			s <- RNGscope()
			s[2:7] <- as.integer(seed)
			RNGscope(s)
			if( verbose ) message("OK")
		}else if ( length(seed) == 7L ){
			if( seed[1] %% 100 != 7L )
				stop("doRNGseed - Invalid 7-length numeric seed: RNG code should be '7', i.e. of type \"L'Ecuyer-CMRG\"")
			if( verbose ) message("# Directly use numeric .Random.seed: ",  paste(seed, collapse=', '), " ... ", appendLF=FALSE)
			RNGscope(seed)
			if( verbose ) message("OK")
		}else
			stop("doRNGseed - Invalid numeric seed: should be a numeric of length 1, 6 or 7")		
	}else if( is.null(seed) ){
		if( rkind_not_CMRG ){ # seed with random seed
			
			# draw once from the current calling RNG to ensure different seeds
			# for separate loops, but to ensure identical results as with set.seed
			# one must still use the current RNG before changing RNG kind
			runif(1)
			orng1 <- RNGscope()
			RNGscope(orng)
			orng <- orng1
			
			if( verbose ) message("# Generate random RNGstream seed: ", appendLF=FALSE)
			RNGkind(kind="L'Ecuyer", normal.kind=normal.kind)
			if( verbose ) message("OK")
		}else{ # seed with next RNG stream
			if( checkRNGversion('1.4') < 0 ){
				on.exit() # cancel RNG restoration
				s <- nextRNGStream(orng)
				if( verbose ) message("# Use next active RNG stream: ", .collapse(s[2:7]))
				RNGscope(s)	
			}else{
				# only change normal kind if necessary and use current stream state
				if( !is.null(normal.kind) ) RNGkind(normal.kind=normal.kind)
				if( verbose ) message("# Use current active RNG stream: ", .collapse(RNGscope()[2:7]))
			}
		}
	}else
		stop("doRNGseed - Invalid seed value: should be a numeric or NULL")

	s <- RNGscope()
	if( verbose ) message("# Seed RNGkind is: ", paste(RNGkind(), collapse=' - '), ' [', .collapse(s), ']')	
	s	
}

#' Generate Sequence of Random Streams
#' 
#' Create a given number of seeds for L'Ecuyer's RNG, that can be used to seed 
#' parallel computation, making them fully reproducible.
#' 
#' This ensures complete reproducibility of the set of run. 
#' The streams are created using L'Ecuyer's RNG, implemented in R core since
#' version 2.14.0 under the name \code{"L'Ecuyer-CMRG"} (see \code{\link{RNG}}).
#' 
#' Generating a sequence without specifying a seed uses a single draw of the 
#' current RNG. The generation of a sequence using seed (a single or 6-length 
#' numeric) a should not affect the current RNG state. 
#' 
#' @param n Number of streams to be created
#' @param seed seed used to initialise the set of streams using \code{\link{doRNGseed}}.
#' @param unlist a logical that specifies if sequences of length 1 should be
#' unlisted and returned as a single vector.
#' @param ... extra arguments passed to \code{\link{doRNGseed}}.  
#' 
#' @return a list of integer vectors (or a single integer  vector if 
#' \code{n=1} and \code{unlist=TRUE}).
#' 
#' @export 
#' @examples
#' 
#' RNGseq(3)
#' RNGseq(3)
#' RNGseq(3, seed=123)
#' # or identically
#' set.seed(123)
#' identical(RNGseq(3), RNGseq(3, seed=123))
#' \dontshow{
#' set.seed(123)
#' stopifnot( identical(RNGseq(3), RNGseq(3, seed=123)) ) 
#' }
#' 
#' RNGseq(3, seed=1:6, verbose=TRUE)
#' # select Normal kind
#' RNGseq(3, seed=123, normal.kind="Ahrens")
#' 
RNGseq <- function(n, seed=NULL, unlist=TRUE, ...){
	
	# check parameters
	if( n <= 0 )
		stop("NMF::createStream - invalid value for 'n' [positive value expected]")
	
	# extract RNG setting from object if possible
	if( !is.null(attr(seed, 'rng')) ) 
		seed <- attr(seed, 'rng')
	
	# convert matrix into a list of seed
	if( is.matrix(seed) )
		seed <- lapply(seq(ncol(seed)), function(i) seed[,i])
	
	# if already a sequence of seeds: use directly
	#print(seed)
	if( is.list(seed) ){
		# check length
		if( length(seed) > n ){
			warning("Reference seed sequence is longer than the required number of seed: only using the ", n, " first seeds.")
			seed <- seed[1:n]
		}else if( length(seed) < n )
			stop("Reference seed sequence is shorter [",length(seed),"] than the required number of seed [", n, "].")
		
		res <- lapply(seed, as.integer)
	}else{ # otherwise: get initial seed for the CMRG stream sequence
		
		orng <- RNGscope()
		.s <- doRNGseed(seed, ...)
	
		res <- lapply(1:n, function(i){
			if( i == 1 ) .s
			else .s <<- nextRNGStream(.s)				
		})

		# if not seeded and current RNG is L'Ecuyer-CMRG => move to stream after last stream
		if( is.null(seed) && RNGkind()[1L] == "L'Ecuyer-CMRG" && checkRNGversion("1.4") >= 0 ){
			# ensure old normal kind is used 
			RNGscope(c(orng[1L], nextRNGStream(.s)[2:7]))
		}
	}
	
	# return list or single RNG
	if( n==1 && unlist )
		res[[1]]
	else
		res
	
}


#' Reproducible Parallel Foreach Backend
#' 
#' \code{infoDoRNG} returns information about the doRNG backend.
#' 
#' @param data a list of data used by the backend
#' @param item the data item requested, as a character string 
#' (e.g. 'name', 'workers', 'version')
#' 
#' @return \code{infoDoRNG} returns the requested info (usually as a character 
#' string or a numeric value).
#' 
#' @export
#' @name doRNG
#' @aliases infoDoRNG
#' @rdname doRNG
#' @author Renaud Gaujoux
#' 
infoDoRNG <- function (data, item) 
{	
	switch(item
			, workers = data$backend$info(data$backend$data, "workers")
			, name = "doRNG"
			, version = "doRNG 1.1" 
			, NULL)
}

##% \code{doRNG} implements the generic reproducible foreach backend. It should 
##% not be called directly by the user.
##% 
##% @param obj a foreach description of the loop arguments
##% @param ex the lopp expression
##% @param envir the loop's evaluation environment
##% @param data configuration data of the doRNG backend
##% 
##% @rdname doRNG
doRNG <- function (obj, ex, envir, data){
		
			
	if( is.null(obj$options) )
		obj$options <- list()
	
	if( !'RNG' %in% names(obj$options) ){
		obj$options$RNG <- if( !data$once || data$nseed==0 ){
			#message("doRNG backend - use seed ", if( data$once ) "only once" else "for every loop", ":")
			data$seed
		}
		else
			NULL
	}
	
	data$nseed <- data$nseed + 1					
	assign('data', data, pos=foreach:::.foreachGlobals)	
	
	# directly register (temporarly) the computing backend
	rngBackend <- getDoBackend()
	on.exit({setDoBackend(rngBackend)}, add=TRUE)
	setDoBackend(rngBackend$data$backend)
	do.call('%dorng%', list(obj, ex), envir=parent.frame())
}

##% Get/Sets the registered foreach backend's data
getDoBackend <- function(){	
	c(foreach:::getDoPar()
	, info= if( exists("info", where = foreach:::.foreachGlobals, inherits = FALSE) ) foreach:::.foreachGlobals$info else function(data, item) NULL)
}
setDoBackend <- function(backend){
	ob <- getDoBackend()
	do.call('setDoPar', backend)
	invisible(ob)
}

#' \code{\%dorng\%} provides an alternative operator \code{\%dopar\%}, that ensures reproducible 
#' foreach loops.
#' 
#' @param obj a foreach object as returned by a call to \code{\link{foreach}}.
#' @param ex the \code{R} expression to evaluate.
#' 
#' @return \code{\%dorng\%} returns the result of the foreach loop. See \code{\link{\%dopar\%}}.
#' The whole sequence of RNG seeds is stored in the result object as an attribute.
#' Use \code{attr(res, 'rng')} to retrieve it. 
#' 
#' @importFrom iterators iter
#' @export 
#' @rdname doRNG
#' @aliases doRNG
#' @usage obj \%dorng\% ex
#' @seealso \code{\link{foreach}}, \code{\link[doParallel]{doParallel}}
#' , \code{\link[doParallel]{registerDoParallel}}, \code{\link[doMPI]{doMPI}}
#' @examples 
#' 
#' library(doParallel)
#' registerDoParallel(cores=2)
#' 
#' # standard %dopar% loops are _not_ reproducible
#' set.seed(1234)
#' s1 <- foreach(i=1:4) %dopar% { runif(1) }
#' set.seed(1234)
#' s2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(s1, s2)
#' 
#' # single %dorng% loops are reproducible
#' r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' r2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(r1, r2)
#' # the sequence os RNG seed is stored as an attribute
#' attr(r1, 'rng')
#' 
#' # sequences of %dorng% loops are reproducible
#' set.seed(1234)
#' s1 <- foreach(i=1:4) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4) %dorng% { runif(1) }
#' # two consecutive (unseed) %dorng% loops are not identical
#' identical(s1, s2)
#'
#' # But the whole sequence of loops is reproducible
#' set.seed(1234)
#' s1.2 <- foreach(i=1:4) %dorng% { runif(1) }
#' s2.2 <- foreach(i=1:4) %dorng% { runif(1) }
#' identical(s1, s1.2) && identical(s2, s2.2)
#' # it gives the same result as with .options.RNG
#' identical(r1, s1) 
#' 
#' # Works with SNOW-like and MPI clusters
#' # SNOW-like cluster
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#' 
#' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' 
#' stopCluster(cl)
#' registerDoSEQ()
#' 
#' # MPI cluster
#' if( require(doMPI) ){
#' cl <- startMPIcluster(2)
#' registerDoMPI(cl)
#' 
#' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' 
#' closeCluster(cl)
#' registerDoSEQ()
#' }
#' 
#' 
`%dorng%` <- function(obj, ex){
	
	# exit if nested or conditional loop
	if( any(c('xforeach', 'filteredforeach') %in% class(obj)) )
		stop("nested/conditional foreach loops are not supported yet.\nSee the package's vignette for a work around.")
	
	# if an RNG seed is provided then setup random streams 
	# and add the list of RNGs to use as an iterated arguments for %dopar%
	library(parallel)
	obj$argnames <- c(obj$argnames, '.doRNG.stream')
	it <- iter(obj)
	argList <- as.list(it)
	
	# restore current RNG  on exit if a seed is passed
	rngSeed <- 
	if( !is.null(obj$options$RNG) ){
		
		# setup current RNG restoration
		RNG.old <- RNGscope()
		on.exit({RNGscope(RNG.old)}, add=TRUE)
		
		# extract RNG setting from object if possible
		rngSeed <- 
		if( !is.null(attr(obj$options$RNG, 'rng')) ) attr(obj$options$RNG, 'rng')
		else obj$options$RNG

		# ensure it is a list
		# NB: unnamed lists are sequences of seeds
		if( !is.list(rngSeed) || is.null(names(rngSeed)) ){
			rngSeed <- list(rngSeed)
		}
		rngSeed
	}
	
	# generate a sequence of streams
#	print("before RNGseq")
#	print(head(RNGscope()))
	obj$args$.doRNG.stream <- do.call("RNGseq", c(list(n=length(argList), verbose=obj$verbose), rngSeed))
#	print("after RNGseq")
#	print(head(RNGscope()))
	#print(obj$args$.doRNG.stream)
	## SEPCIAL CASE FOR doSEQ or doMPI
	# TODO: figure out why doMPI draw once from the current RNG (must be linked
	# to using own code to setup L'Ecuyer RNG)
	# restore RNG settings as after RNGseq if doSEQ is the backend and no seed was passed
	dp <- getDoParName()
#	print(dp)
	if( is.null(obj$options$RNG) && (is.null(dp) || dp=='doSEQ' || dp=='doMPI') ){
#		print("reset as after RNGseq")
		RNG.old <- RNGscope()
		on.exit({RNGscope(RNG.old)}, add=TRUE)
	}
	##
	
	# export package doRNG if not already exported
	if( is.null(obj$packages) || !('doRNG' %in% obj$packages) )
		obj$packages <- c(obj$packages, 'doRNG')
	
	# append code to the loop expression to set the RNG
	ex <- as.call(list(as.name('{'),
					quote({RNGscope(.doRNG.stream);}),
					substitute(ex)))
	
	# directly register (temporarly) the computing backend
	if( !is.null(dp) && dp == 'doRNG' ){
		rngBackend <- getDoBackend()
		on.exit({setDoBackend(rngBackend)}, add=TRUE)
		setDoBackend(rngBackend$data$backend)
	}
	
	# call the standard %dopar% operator
	res <- do.call('%dopar%', list(obj, ex), envir=parent.frame())
	# add seed sequence as an attribute
	attr(res, 'rng') <- obj$args$.doRNG.stream
	# return result
	res
}

#' \code{registerDoRNG} registers the doRNG foreach backend.
#' Subsequent \%dopar\% loops are actually performed using the previously 
#' registered foreach backend, but the RNG is set, before each iteration, 
#' with seeds, that generate a reproducible sequence of statistically 
#' independent random streams.
#' 
#' Note that (re-)registering a foreach backend other than doRNG, after a call 
#' to \code{registerDoRNG} disables doRNG -- which then needs to be registered.
#' 
#' @param seed a numerical seed to use (as a single or 6-length numerical value)
#' @param once a logical to indicate if the RNG sequence should be seeded at the 
#' beginning of each loop or only at the first loop. 
#' 
#' @export
#' @rdname doRNG
#' 
#' @examples 
#' 
#' library(doParallel)
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#' 
#' # One can make reproducible loops using the %dorng% operator
#' r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' # or convert %dopar% loops using registerDoRNG
#' registerDoRNG(1234)
#' r2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r1, r2)
#' stopCluster(cl)
#'
#' # Registering another foreach backend disables doRNG
#' cl <- makeCluster(3)
#' registerDoParallel(cl)
#' set.seed(1234)
#' s1 <- foreach(i=1:4) %dopar% { runif(1) }
#' set.seed(1234)
#' s2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot(!identical(s1, s2)) }
#' 
#' # doRNG is re-nabled by re-registering it 
#' registerDoRNG()
#' set.seed(1234)
#' r3 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r2, r3)
#' # NB: the results are identical independently of the task scheduling
#' # (r2 used 2 nodes, while r3 used 3 nodes)
#' 
#' # argument `once=FALSE` reseeds doRNG's seed at the beginning of each loop 
#' registerDoRNG(1234, once=FALSE)
#' r1 <- foreach(i=1:4) %dopar% { runif(1) }
#' r2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r1, r2)
#' 
#' # Once doRNG is registered the seed can also be passed as an option to %dopar%
#' r1.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
#' r2.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
#' identical(r1.2, r2.2) && !identical(r1.2, r1)
#' \dontshow{ stopifnot(identical(r1.2, r2.2) && !identical(r1.2, r1)) }
#' 
#' stopCluster(cl)
#' 
registerDoRNG <- function(seed=NULL, once=TRUE){
	
	backend <- getDoBackend()
	
	# use stored backend if registerDoRNG was called repeatedly
	if( getDoParName() == 'doRNG' )
		backend <- backend$data$backend
	
	# set the current RNG with seed immediately if only used once
	if( once && !is.null(seed) ){
		if( !is.numeric(seed) || length(seed)!=1L )
			stop("Invalid seed: must be a single numeric value.")
		set.seed(seed)
		seed <- NULL
	}
	
	setDoPar(doRNG, list(seed=seed, once=once, nseed=0, backend=backend), infoDoRNG)	
	
}

###% Reproducibly Apply a Function over a List or Vector
###% 
###% @aliases xapply reproduce
###% 
###% \code{reproduce} and \code{xapply} are a reproducible versions 
###% of \code{\link{replicate}} and \code{\link{sapply}} respectively, 
###% that ensures the reproducibility of the results, when stochastic computations
###% are involved.
###% 
###% The reproducibility is achieved by using LEcuyer's RNG provided by R core
###% since R-2.14.0, to generate independent random streams 
###% that are used as the random number generator for each replicate.
###% 
###% @param n the number of replication as a single numeric (integer)
###% @param seed the main numerical seed used to initialize the sequence of random 
###% streams
###% @param expr the expression (language object, usually a call) to evaluate repeatedly
###% @param simplify logical; should the result be simplified to a vector or 
###% matrix if possible?
###% 
###% 
###%  
#reproduce <- function (n, expr, seed=NULL, simplify = TRUE){
#	f <- eval.parent(substitute(function(...) expr))
#	xapply(integer(n), seed, f, simplify = simplify)
#}
#
#xapply <- function (X, FUN, seed=NULL, ..., simplify = TRUE, USE.NAMES = TRUE){
#	
#	# generate a sequence of streams
#	.RNG.stream <- RNGseq(length(X), seed, packed=TRUE)
#	
#	# keep current RNG and restore it on exit (useful for the sequential backend doSEQ)
#	RNG.old <- rstream.RNG()
#	on.exit(rstream.RNG(RNG.old), add=TRUE)
#	
#	# append code to the loop expression to set the RNG	
#	expr <- as.call(list(as.name('{'),
#					quote({doRNGseed(.rng);}),
#					quote(do.call(FUN, list(...)))))
#	
#	env <- environment(FUN)
#	f <- eval(substitute(function(.rng, ..., FUN) expr), env)	
#	mapply(f, .RNG.stream, X, MoreArgs=c(list(...), FUN=FUN), 
#			SIMPLIFY = simplify, USE.NAMES= USE.NAMES)
#}

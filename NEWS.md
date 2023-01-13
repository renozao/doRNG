# Changes in version 1.8.4

There is no changes in this version, which was published to 
reclaim ownership and take the package out of ORPHANED state (issue #23).

# Changes in version 1.8

## Changes
    o Unit tests are now run through testthat
    o Minor fixes in man pages and README file
    o Now depends on rngtools >= 1.3
    o The result list gains an attribute 'doRNG_version' that contains
    the version of doRNG that was used, based on doRNGversion(). 
    NB: this is not necessarily the same as the version of the installed 
    package. 
    o Added the following global option 'doRNG.rng_change_warning_skip'.
    See ?`%dorng%` (issue #14).
    o Moved dependency on pkgmaker to Suggests to make installation lighter 
    (issue #10).
    
## Bug fixes
    o Enabled running %dorng% loops within a package (incorporating 
    the solution proposed by Elizabeth Byerly in PR#3) 
    o Fixed error with NULL iteration results when setting 'rng' 
    attribute (issue #9)
    o Fixed error when using unamed foreach arguments (issue #8)
    o Fixed non-reproducibility issue when the .Random.seed is not yet 
    initialized, e.g., when the session starts and the RNG has not been 
    used yet (issue #12)
    o Fixed runtime error when package is not attached (issue #13)


# Changes in version 1.6.2


## Bug fixes
    o Non reproducible %dorng% loop when doRNG is registered over doSEQ 
    (Issue #1 reported by Brenton Kenkel). Actually due to %dorng% not 
    restoring the RNG (to state + 1) when doRNG is registered over doSEQ.
    o %dorng% was not working properly on loops of length one (Issue #2)  


# Changes in version 1.6


## Changes
    o doRNG now depends on the package pkgmaker (>= 0.20)
    
## Bug fixes
    o Check error due number of cores used. Now limited to 2 in examples,
    vignette and unit test.


# Changes in version 1.5


## Changes
    o doRNG now depends on the package pkgmaker (>= 0.9)
    o improved vignette
    o most of the general RNG utilities have been incorporated in a new
    package called rngtools.
    


# Changes in version 1.4.1


## Changes
    o when the current RNG was L'Ecuyer-CMRG, unseeded loops now use 
    the current RNG stream as for the first stream in the RNG sequence 
    and # Changes the current RNG to the next RNG stream of the last stream 
    in the sequence. 

## Bug fixes
    o fix error "'iter' not found" due to # Changes in foreach package 
    dependencies -- that was announced by Rich Calaway.
    o loops seeded with set.seed and .options.RNG were not reproducible
    when current RNG was L'Ecuyer-CMRG (reported by Zhang Peng)
    o separate unseeded loops were sharing most of their streams, 
    when current RNG was L'Ecuyer-CMRG the RNG seed.
    o nested/conditional loops were crashing with a bad error. 
    They are still not supported but the error message is nicer and a 
    work around has been added to the vignette (reported by Chanhee Yi 
    and Zhang Peng).


# Changes in version 1.2.3


## Bug fixes
    o fixed error when running a %dorng% loop on a fresh session, with no  
    parallel backend registered.  

## Changes
    o improved vignette
    o added more unit tests
    o changed the name of the RNG attribute on result of %dorng% looops 
    from 'RNG' to 'rng'. It now contains the whole sequence of RNG seeds, 
    instead of only the first one.
    o RNGseq now accepts a list or a matrix describing the whole sequence 
    of seeds. See vignette for more details.
    o %dorng% loops can be seeded with a complete sequence of seeds passed 
    as a list, a matrix, or an object with attribute 'rng', e.g. the 
    results of %dorng% loops. See vignette for more details.
    

# Changes in version 1.2.2


## Bug fixes
    o separate %dorng% loops were using the same seed.

## New features
    o add unit tests
    o first seed is set as an attribute of the loop's result

## Changes
    o function doRNGseed now returns the seed to use for the first 
    iteration.
    o RNGseq now change the current RNG state if called with no seed 
    specific.  
    
## Defunct
    o removed function CMRGseed


# Changes in version 1.2


## Bug fixes
    o An error was thrown if using %dorng% loops before using any random
    generator. Thanks to Eric Lehmann for reporting this.

## Changes
    o add vignette
    o use package doParallel in examples


# Changes in version 1.1


## Changes
    o use R core RNG "L'Ecuyer-CMRG" and the parallel package, 
    instead of the implementation provided by the rstream package.


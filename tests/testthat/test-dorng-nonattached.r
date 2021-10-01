context('dorng - foreach not attached')

test_that("%dorng% works also when foreach is not attached", {
  `%dorng%` <- doRNG::`%dorng%`
  y <- foreach::foreach(x = 1:2) %dorng% { x }
  stopifnot(all.equal(y, list(1, 2), check.attributes = FALSE))
})

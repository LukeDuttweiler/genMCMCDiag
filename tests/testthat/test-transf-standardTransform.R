test_that('standard transform returns as expected', {
  tstObj <- list(list(val = as.list(1:5), Posterior = NA),
                 list(val = as.list(1:5), Posterior = NA),
                 list(val = as.list(2:6), Posterior = NA))

  ############Will remove soon###########
  tstObj <- lapply(tstObj, as.mcmcChain)
  #######################################

  #ALSO, NEED TO FIX THE FACT THAT WE NEED as.list(), thats absurd.

  expectObj <- list(data.frame('val.1' = 1:5,'Posterior' = NA, 't' = 1:5),
                    data.frame('val.1' = 1:5,'Posterior' = NA, 't' = 1:5),
                    data.frame('val.1' = 2:6,'Posterior' = NA, 't' = 1:5))

  expect_identical(standardTransform(tstObj), expectObj)
})

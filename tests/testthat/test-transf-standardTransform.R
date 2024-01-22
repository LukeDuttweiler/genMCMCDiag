test_that('standard transform returns as expected', {
  tstObj <- list(1:5,
                 1:5,
                 2:6)

  expectObj <- list(data.frame('val' = 1:5, 't' = 1:5),
                    data.frame('val' = 1:5, 't' = 1:5),
                    data.frame('val' = 2:6, 't' = 1:5))

  expect_identical(standardTransform(tstObj), expectObj)
})

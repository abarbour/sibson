context('Utility functions...')

test_that('cotan is correct',{
  expect_equal(cotan(0), Inf)
  expect_equal(cotan(pi/4), 1)
  expect_equal(cotan(pi/2), 1/tan(pi/2))
})

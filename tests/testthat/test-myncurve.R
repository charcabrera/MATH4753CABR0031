test_that("mu is correct", {
  expect_equal(10, myncurve(mu=10,sigma=5, a=6)$mu)
})

test_that("area is correct", {
  expect_equal(0.2118554, myncurve(mu=10,sigma=5, a=6)$area)
})

test_that("sigma is correct", {
  expect_equal(5, myncurve(mu=10,sigma=5, a=6)$sigma)
})

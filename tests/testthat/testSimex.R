#
# Unit test
#


rm(list = ls())
require(SIMEXGLM)

data("simexExample")

mySIMEX <- SIMEXGLM("y ~ distanceToConspecific", # the formula
                    "CLogLog", # the link function
                    simexExample, # the data
                    "distanceToConspecific", # variable with measurement error
                    "variance",
                    nbThreads = 3) # variance of the measurement error

shutdownClient()

#summary(mySIMEX)
#plot(mySIMEX)
coefficients <- coef(mySIMEX)
#mySIMEX$getEffectNames()

test_that("Checking parameter estimates", {
  expect_true(abs(coefficients[1] - -0.1584360) < 1E-2)
  expect_true(abs(coefficients[2] - -0.0557473) < 1E-3)
})

vcovMat <- vcov(mySIMEX)

test_that("Checking variance estimates", {
  expect_true(abs(vcovMat[1,1] - 0.0046055043) < 1E-4)
  expect_true(abs(vcovMat[2,1] - -2.266612e-04) < 1E-5)
  expect_true(abs(vcovMat[2,2] - 1.976268e-05) < 1E-6)
})

predictions <- fitted(mySIMEX)

test_that("Checking fitted values", {
  expect_true(abs(predictions[1] - 0.053331652) < 3E-3)
  expect_true(abs(predictions[2] - 0.436177757) < 3E-3)
  expect_true(abs(predictions[3] - 0.003563088) < 1E-3)
})

newPredictions <- predict(mySIMEX, simexExample)
test_that("Checking predictions", {
  expect_true(  all(abs(newPredictions$meanPred - predictions) < 1E-8 ))
})


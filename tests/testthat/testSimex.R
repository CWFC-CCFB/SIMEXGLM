#
# Unit test
#


rm(list = ls())
require(SIMEXGLM)

data("simexExample")

mySIMEX <- SIMEXGLM("y ~ distanceToConspecific", # the formula
                    "Bernoulli", # the distribution
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
  expect_true(abs(vcovMat[1,1] - 0.0046055043) < 2E-4)
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


mySIMEX <- SIMEXGLM("y ~ distanceToConspecific + sqr(distanceToConspecific)", # the formula
                    "Bernoulli", # the distribution
                    "CLogLog", # the link function
                    simexExample, # the data
                    "distanceToConspecific", # variable with measurement error
                    "variance",
                    nbThreads = 3) # variance of the measurement error

summary(mySIMEX)

coefficients <- coef(mySIMEX)

test_that("Checking parameter estimates", {
  expect_true(abs(coefficients[3]) < 1E-3)
})

test_that("Expecting error with variable transformation", {
  expect_error(mySIMEX <- SIMEXGLM("y ~ distanceToConspecific + log(1+distanceToConspecific)", # the formula
                      "Bernoulli", # the distribution
                      "CLogLog", # the link function
                      simexExample, # the data
                      "distanceToConspecific", # variable with measurement error
                      "variance",
                      nbThreads = 3) # variance of the measurement error
  )
})

summary(mySIMEX)

coefficients <- coef(mySIMEX)


shutdownClient()

#### Example with negative binomial ####

rm(list=ls())
data("simexExampleNegBinomial")

o <- SIMEXGLM::createDataSet("y ~ TotalPrcp + G_F + G_R + occIndex10km + timeSince1970", simexExampleNegBinomial, "occIndex10kmVar")

elapTime <- system.time({
mySIMEX <- SIMEXGLM("y ~ TotalPrcp + G_F + G_R + occIndex10km + timeSince1970", # the formula
                    "NegativeBinomial", # the distribution
                    "Log", # the link function
                    o, # the data
                    "occIndex10km", # variable with measurement error
                    "occIndex10kmVar",
                    nbThreads = 3) # variance of the measurement error
})[3]
shutdownClient()


#summary(mySIMEX)
#plot(mySIMEX)
coefficients <- coef(mySIMEX)
#mySIMEX$getEffectNames()

test_that("Checking parameter estimates", {
  expect_true(abs(coefficients[1] - -2.86) < 2E-1)
  expect_true(abs(coefficients[2] - 0.00155) < 3E-4)
  expect_true(abs(coefficients[3] - -0.1165) < 3E-3)
  expect_true(abs(coefficients[4] - -0.0713) < 2E-3)
  expect_true(abs(coefficients[5] - 3.37) < 2E-1)
  expect_true(abs(coefficients[6] - 0.0450) < 2E-3)
  expect_true(abs(coefficients[7] - 1.83) < .1)
})

predictions <- fitted(mySIMEX)

test_that("Checking fitted values", {
  expect_true(abs(predictions[1] - 0.639) < 5E-2)
  expect_true(abs(predictions[2] - 0.791) < 5E-2)
  expect_true(abs(predictions[3] - 0.393) < 5E-2)
})

newPredictions <- predict(mySIMEX, simexExampleNegBinomial)
test_that("Checking predictions", {
  expect_true(  all(abs(newPredictions$meanPred - predictions) < 1E-8 ))
})

# require(MASS)
# elapTimeMass <- system.time({
#   for (i in 1:1001) {
#     glm.nb("y ~ TotalPrcp + G_F + G_R + occIndex10km + timeSince1970", simexExampleNegBinomial)
#   }
# })[3]
#
# test_that("Checking that computing time is half that of the MASS package", {
#   expect_true(elapTime < elapTimeMass * .5)
# })


test_that("Expecting error for non convergence", {
  expect_error(mySIMEX <- SIMEXGLM("y ~ TotalPrcp + G_F + G_R + occIndex10km + log(1+occIndex10km) + timeSince1970", # the formula
                      "NegativeBinomial", # the distribution
                      "Log", # the link function
                      simexExampleNegBinomial, # the data
                      "occIndex10km", # variable with measurement error
                      "occIndex10kmVar",
                      nbThreads = 3) # variance of the measurement error
  )
})

shutdownClient()



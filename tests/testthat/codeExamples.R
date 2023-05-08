#
# Unit test
#


#### Example with logistic regresion ####

rm(list = ls())
require(SIMEXGLM)

data("simexExample")

mySIMEX <- SIMEXGLM("y ~ distanceToConspecific + sqr(distanceToConspecific)", # the formula
                    "Bernoulli", # the distribution
                    "CLogLog", # the link function
                    simexExample, # the data
                    "distanceToConspecific", # variable with measurement error
                    "variance",
                    nbThreads = 3) # variance of the measurement error
summary(mySIMEX)
plot(mySIMEX)

shutdownClient()


#### Example with negative binomial ####

rm(list=ls())
data("simexExampleNegBinomial")

mySIMEX <- SIMEXGLM("y ~ TotalPrcp + G_F + G_R + occIndex10km + timeSince1970", # the formula
                    "NegativeBinomial", # the distribution
                    "Log", # the link function
                    simexExampleNegBinomial, # the data
                    "occIndex10km", # variable with measurement error
                    "occIndex10kmVar",
                    nbThreads = 3) # variance of the measurement error
summary(mySIMEX)
plot(mySIMEX)


shutdownClient()

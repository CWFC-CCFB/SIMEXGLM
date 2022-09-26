#
# Unit test
#


rm(list = ls())
data("simexExample")

mySIMEX <- SIMEXGLM("y ~ distanceToConspecific", # the formula
                    "CLogLog", # the link function
                    simexExample, # the data
                    "distanceToConspecific", # variable with measurement error
                    "variance") # variance of the measurement error

summary(mySIMEX)
coef(mySIMEX)
vcov(mySIMEX)
plot(mySIMEX)


shutdownClient()

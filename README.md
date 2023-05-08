The SIMEXGLM package
===============

<!-- badges: start -->
[![R-CMD-check](https://github.com/CWFC-CCFB/SIMEXGLM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CWFC-CCFB/SIMEXGLM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package implementing the SIMEX technique for classical additive measurement errors in generalized linear models.

## Table of content

1. [Introduction](#Introduction)
2. [How to install the package](#how-to-install-the-package)
3. [Examples of code](#examples-of-code)
4. [References](#references)


## Introduction

The SIMEX technique accounts for additive measurement errors in explanatory variables when fitting a statistical model. This technique was initially developed by [Cook and Stefanski 1994](https://doi.org/10.1080/01621459.1994.10476871) and [Stefanski and Cook 1995](https://doi.org/10.1080/01621459.1995.10476629). [Caroll et al. 2006](https://www.taylorfrancis.com/books/mono/10.1201/9781420010138/measurement-error-nonlinear-models-ciprian-crainiceanu-raymond-carroll-leonard-stefanski-david-ruppert) also includes a complete chapter on this technique.  

The SIMEXGLM package implements the SIMEX technique with the following statistical models:

* the logistic regression
* the negative binomial regression

Compared to other existing packages implementing the SIMEX technique, the SIMEXGLM package allows the specification of multiple terms involving the linear term plus some transformation of the variables with measurement errors. For instance, if W is the variable with measurement error, the package allows the fit of the model E[Y] = g(W, W<sup>2</sup>), with g being the link function. An example of code is shown below. 

The source code of the SIMEXGLM package is freely available at https://github.com/CWFC-CCFB/SIMEXGLM .

The SIMEXGLM package is licensed under the GNU Lesser General Public License v3.0 (LGPL-3).

Tickets can be created at https://github.com/CWFC-CCFB/SIMEXGLM/issues .

Mathieu Fortin
e-mail: mathieu.fortin.re@gmail.com


## How to install the package

The SIMEXGLM package depends on J4R. You must copy and paste these two lines of code in your R console:

~~~R
install.packages("https://sourceforge.net/projects/repiceasource/files/latest/download", repos = NULL,  type="source") ### To install J4R (dependency)
library(remotes)
install_github("CWFC-CCFB/SIMEXGLM") ### install SIMEXGLM directly from GitHub
~~~

## Examples of code

Here is an example with the logistic regression with the complementary log-log link function:

~~~R
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
~~~

The <code>sqr</code> function embedded in the formula means that the square of the distanceToConspecific variable is specified in the model. Regular <code>summary</code> and <code>plot</code> functions allow to visualize the results. In its current version, the SIMEXGLM package allows the following transformation in model formula:

* sqr(): the square of the argument
* log(): the natural logarithm of the argument
* exp(): the exponential of the argument (beta)

It is rather difficult to obtain convergence with the log transformation as the variance inflation may lead to negative values. So far, the sqr function has proven much more stable.

Here is an example of negative binomial regression with a log link function:

~~~R
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
~~~

The call to the <code>shutdownClient</code> function shuts down the client and the Java server as well avoiding having an idle process. 


## References

[Cook, J.R., and L.A. Stefanski. 1994. Simulation-extrapolation estimation in parametric measurement error models. Journal of the American Statistical Association 89: 1314-1328. doi: 10.1080/01621459.1994.10476871](https://doi.org/10.1080/01621459.1994.10476871)

[Stefanski, L.A., and J.R. Cook. 1995. Simulation-extrapolation: the measurement error jacknnife. Journal of the American Statistical Association 90: 1247-1256. doi: 10.1080/01621459.1995.10476629](https://doi.org/10.1080/01621459.1995.10476629)

[Carroll, R.J., D. Ruppert, L.A. Stefanski, and C.M. Crainiceanu. 2006. Measurement error in nonlinear models. A modern perspective, Second Edition. Chapman & Hall/CRC. London.](https://www.taylorfrancis.com/books/mono/10.1201/9781420010138/measurement-error-nonlinear-models-ciprian-crainiceanu-raymond-carroll-leonard-stefanski-david-ruppert)
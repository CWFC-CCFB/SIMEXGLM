########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################


jarFilenames <- c("repicea-1.9.4.jar", "repicea-mathstats-1.1.3.jar")

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to SIMEXGLM!")
  packageStartupMessage("The SIMEXGLM package implements the SIMEX procedure for logistic and")
  packageStartupMessage("negative binomial models.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/SIMEXGLM .")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  shutdownClient()
}

.onDetach <- function(libpath) {
  shutdownClient()
}


#'
#' A fake data.frame object for an example of the SIMEX method
#' with logistic regression.
#'
#' @docType data
#'
#' @usage data(simexExample)
#'
#' @keywords datasets
#'
#' @examples
#' data(simexExample)
"simexExample"

#'
#' A fake data.frame object for an example of the SIMEX method
#' with negative binomial regression.
#'
#' @docType data
#'
#' @usage data(simexExampleNegBinomial)
#'
#' @keywords datasets
#'
#' @examples
#' data(simexExampleNegBinomial)
"simexExampleNegBinomial"


#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownClient <- function() {
  if (J4R::isConnectedToJava()) {
    J4R::shutdownClient()
  }
}

.addToArray <- function(refArray, array) {
  if (length(refArray) != length(array)) {
    stop("Incompatible array length!")
  } else {
    for (i in 1:length(array)) {
      refArray[[i]] <- c(refArray[[i]], array[[i]])
    }
  }
  return(refArray)
}

.convertJavaDataSetIntoDataFrame <- function(dataSetObject) {
  dataFrameObj <- NULL
  fieldNames <- J4R::getAllValuesFromListObject(dataSetObject$getFieldNames())
  for (i in 0:(length(fieldNames) - 1)) {
    if (is.null(dataFrameObj)) {
      dataFrameObj <- data.frame(J4R::getAllValuesFromListObject(dataSetObject$getFieldValues(i)))
    } else {
      dataFrameObj <- cbind(dataFrameObj, J4R::getAllValuesFromListObject(dataSetObject$getFieldValues(i)))
    }
  }
  colnames(dataFrameObj) <- fieldNames
  return(dataFrameObj)
}



#' Create a Data Structure
#'
#' Create a data structure on the Java end that will be later used with
#' the GLM or SIMEX methods.
#'
#' @param formula a formula (e.g. "y ~ x")
#' @param data a data.frame object
#' @param varianceFieldName the field that contains the variance of the measurement error in the data argument
#'
#' @export
createDataSet <- function(formula, data, varianceFieldName = NULL) {
  .loadLibrary()
  formattedString <- J4R::callJavaMethod(formula, "replace", "\n", "")
  formattedString <- J4R::callJavaMethod(formattedString, "replace", " ", "")
  firstSplit <- J4R::getAllValuesFromListObject(J4R::callJavaMethod("repicea.util.ObjectUtility", "decomposeUsingToken", formattedString, "~"))
  secondSplit <- J4R::getAllValuesFromListObject(J4R::callJavaMethod("repicea.util.ObjectUtility", "decomposeUsingToken", firstSplit[2], "+"))
  uncorrectedFieldNames <- c(firstSplit[1], secondSplit)
  fieldNames <- unique(unlist(strsplit(uncorrectedFieldNames, ":")))

  data.tmp <- data
  if (is.logical(data.tmp[, firstSplit[1]])) { # the response is a logical
    data.tmp[, firstSplit[1]] <- as.numeric(data.tmp[, firstSplit[1]])
  }

  myDataSet <- J4R::createJavaObject("repicea.stats.data.DataSet")
  if (is.null(varianceFieldName)) {
    completeFieldNames <- fieldNames
  } else {
    completeFieldNames <- c(fieldNames, varianceFieldName)
  }

  for (f in completeFieldNames) {
    myObjectArray <- J4R::createJavaObject("java.lang.Object", length(data.tmp[,1]), isArray = TRUE)
    if (f %in% colnames(data.tmp)) {
      J4R::setValueInArray(myObjectArray, data.tmp[, f])
      myDataSet$addField(f, myObjectArray)
    }
  }
  return(myDataSet)
}

.convertDataIfNeeded <- function(formula, data, varianceFieldName = NULL) {
  if ("java.object" %in% class(data) && data$.class == "repicea.stats.data.DataSet") {
    dataSet <- data
  } else {
    dataSet <- createDataSet(formula, data, varianceFieldName)
  }
  return(dataSet)
}


#'
#' Correct Inference for Generalized Linear Models with Measurement Errors
#' using the SIMEX Method
#'
#' First, it create a data structure on the Java end that will be later used with
#' the SIMEX method. Secondly, it fits a naive model. Thirdly, it implements the
#' SIMEX method.
#'
#' @param formula a formula (e.g. "y ~ x")
#' @param dist the distribution of the reponse variable (either "Bernoulli" or "NegativeBinomial")
#' @param linkFunction the link function (either "Logit" or "CLogLog" for the Bernoulli distribution
#' or "Log" for the negative binomial)
#' @param data a data.frame object
#' @param fieldWithMeasError the field with measurement error in the data argument
#' @param varianceFieldName the field that contains the variance of the measurement error in the data argument
#' @param factors a sequence of inflation factors (from 0 to 2 by .2 by default)
#' @param nbBootstrapRealizations the number of bootstrap realizations for each level of inflated variance (is
#' set to 100 by default )
#' @param nbThreads the number of threads to process the bootstrap realizations (is set to 2 by default)
#'
#' @return an instance of the S3 SIMEXResult class
#'
#' @export
SIMEXGLM <- function(formula,
                     dist = c("Bernoulli", "NegativeBinomial"),
                     linkFunction = c("Logit", "CLogLog","Log"),
                     data,
                     fieldWithMeasError,
                     varianceFieldName,
                     factors = seq(0, 2, by=.2),
                     nbBootstrapRealizations = 100,
                     nbThreads = 2) {
  .loadLibrary()
  message("SIMEX: Converting data.frame instance to Java object...")
  simexDataSet <- .convertDataIfNeeded(formula, data, varianceFieldName)
  linkFunctionType <- J4R::createJavaObject("repicea.stats.model.glm.LinkFunction$Type", linkFunction)
  message("SIMEX: Fitting preliminary model (without consideration for measurement errors)...")
  distribution <- J4R::createJavaObject("repicea.stats.model.glm.Family$GLMDistribution", dist)
  genLinMod <- J4R::createJavaObject("repicea.stats.model.glm.GeneralizedLinearModel",
                                     simexDataSet,
                                     distribution,
                                     linkFunctionType,
                                     formula)
  genLinMod$doEstimation()
  if (!genLinMod$getEstimator()$isConvergenceAchieved()) {
    stop("Convergence could not be achieved!")
  }
  message("SIMEX: Running SIMEX procedure. This may take a while...")
  simexMod <- J4R::createJavaObject("repicea.stats.model.glm.measerr.SIMEXModel", genLinMod, fieldWithMeasError, varianceFieldName)
  simexMod$setNumberOfBootstrapRealizations(as.integer(nbBootstrapRealizations))
  simexMod$setNbThreads(as.integer(nbThreads))
  simexMod$setFactors(J4R::as.JavaArray(factors))
  simexMod$doEstimation()
  if (!simexMod$getEstimator()$isConvergenceAchieved()) {
    stop("Convergence could not be achieved!")
  }
  simexResult <- new_SIMEXResult(genLinMod,
                                 simexMod,
                                 formula,
                                 dist,
                                 linkFunction,
                                 fieldWithMeasError,
                                 varianceFieldName,
                                 simexMod$getNumberOfBootstrapRealizations())
  return(simexResult)
}


.loadLibrary <- function(memSize = NULL) {
  if (J4R::isConnectedToJava()) {
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems J4R is running but the class path does not contain this library: ", jarName, ". Shut down J4R using the shutdownClient function first and then re-run your code."))
      }
    }
  } else {
    path <- system.file(jarFilenames, package = "SIMEXGLM", mustWork = T)
    J4R::connectToJava(extensionPath = path, memorySize = memSize)
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems J4R has not been able to load the", jarName, "library."))
      }
    }
    loggerName <- J4R::getJavaField("repicea.stats.estimators.MaximumLikelihoodEstimator", "LOGGER_NAME")
    logger <- J4R::callJavaMethod("repicea.util.REpiceaLogManager", "getLogger", loggerName)
    level <- J4R::getJavaField("java.util.logging.Level", "WARNING")
    logger$setLevel(level)
  }
}

#'
#' Fit Generalized Linear Models
#'
#' First, it create a data structure on the Java end. Then,
#' it fits a generalized linear model.
#'
#' @param formula a formula (e.g. "y ~ x")
#' @param dist the distribution of the reponse variable (either "Bernoulli" or "NegativeBinomial")
#' @param linkFunction the link function (either "Logit" or "CLogLog" for the Bernoulli distribution
#' or "Log" for the negative binomial)
#' @param data a data.frame object
#'
#' @return a java.object instance
#'
GLM <- function(formula,
                     dist = c("Bernoulli", "NegativeBinomial"),
                     linkFunction = c("Logit", "CLogLog","Log"),
                     data) {
  .loadLibrary()
  message("GLM: Converting data.frame instance to Java object...")
  glmDataSet <- .convertDataIfNeeded(formula, data)
  linkFunctionType <- J4R::createJavaObject("repicea.stats.model.glm.LinkFunction$Type", linkFunction)
  message("GLM: Fitting model...")
  distribution <- J4R::createJavaObject("repicea.stats.model.glm.Family$GLMDistribution", dist)
  genLinMod <- J4R::createJavaObject("repicea.stats.model.glm.GeneralizedLinearModel",
                                     glmDataSet,
                                     distribution,
                                     linkFunctionType,
                                     formula)
  genLinMod$doEstimation()
  return(genLinMod)
}


.convertJavaMatrixToR <- function(jObject) {
  jMatrixClass <- J4R::callJavaMethod("java.lang.Class", "forName", "repicea.math.Matrix")
  cl <- jObject$getClass()
  if (!jMatrixClass$isAssignableFrom(cl)) {
    stop("The jObject argument should be a jObject pointing to a repicea.math.Matrix instance")
  }
  nrows <- jObject$m_iRows
  ncols <- jObject$m_iCols
  m <- matrix(nrow = nrows, ncol = ncols)
  for (i in 0:(jObject$m_iCols - 1)) {
    index <- 0:(jObject$m_iRows - 1)
    m[index + 1, i + 1] <- jObject$getValueAt(index , i)
  }
  if (nrows == 1 || ncols == 1) {
    return(as.vector(m))
  } else {
    return(m)
  }
}






########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################


repiceaFilename <- "repicea-1.4.2.jar"

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to SIMEXGLM!")
  packageStartupMessage("The SIMEXGLM package implements the SIMEX procedure for logistic model!")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/SIMEXGLM .")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  shutdownJava()
}

.onDetach <- function(libpath) {
  shutdownJava()
}


#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownJava <- function() {
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
  refArray <- NULL
  observations <- J4R::callJavaMethod(dataSetObject, "getObservations")
  observations <- J4R::getAllValuesFromListObject(observations)
  for (obs in observations) {
    array <- J4R::callJavaMethod(obs, "toArray")
    array <- as.list(J4R::getAllValuesFromArray(array))
    if (is.null(refArray)) {
      refArray <- array
    } else {
      refArray <- .addToArray(refArray, array)
    }
  }
  dataFrame <- NULL
  for (i in 1:length(refArray)) {
    dataFrame <- as.data.frame(cbind(dataFrame, refArray[[i]]))
  }
  colnames(dataFrame) <- J4R::getAllValuesFromListObject(J4R::callJavaMethod(dataSetObject, "getFieldNames"))
  return(dataFrame)
}


#' @export
connectToJava <- function() {
  if (J4R::isConnectedToJava()) {
    if (!J4R::checkIfClasspathContains(repiceaFilename)) {
      stop("The package is connect to Java but the required Java library is not part of the classpath! Please shutDownJava() first!")
    }
  } else {
    repiceaPath <- normalizePath(paste(find.package("SIMEXGLM"), repiceaFilename, sep="/"))
    J4R::connectToJava(extensionPath = repiceaPath)
  }
}

#' @export
SIMEXGLM <- function(formula, linkFunction=c("logit", "CLogLog"), data, fieldWithMeasError, varianceFieldName) {
  message("SIMEX: Converting data.frame instance to Java object...")
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
  for (f in c(fieldNames, varianceFieldName)) {
    myObjectArray <- J4R::createJavaObject("java.lang.Object", length(data.tmp[,1]), isArray = TRUE)
    J4R::setValueInArray(myObjectArray, data.tmp[, f])
    myDataSet$addField(f, myObjectArray)
  }
  message("SIMEX: Done.")
  linkFunctionType <- J4R::createJavaObject("repicea.stats.model.glm.LinkFunction$Type", linkFunction)
  message("SIMEX: Fitting preliminary model (without considering measurement errors)...")
  genLinMod <- J4R::createJavaObject("repicea.stats.model.glm.GeneralizedLinearModel",
                                     myDataSet,
                                     linkFunctionType,
                                     formula)
  genLinMod$doEstimation()
  message("SIMEX: Done.")
  message("SIMEX: Running SIMEX procedure. This may take a while...")
  simexMod <- J4R::createJavaObject("repicea.stats.model.glm.measerr.SIMEXModel", genLinMod, fieldWithMeasError, varianceFieldName)
  simexMod$doEstimation()
  message("SIMEX: Done.")
  return(simexMod)
}








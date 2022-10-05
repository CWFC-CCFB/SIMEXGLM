#
# Class for SIMEX
#

#'
#' Provide a Summary Adapted to the SIMEX Method
#'
#' @param obj an instance of the S3 class SIMEXResult
#'
#' @export
summary.SIMEXResult <- function(object) {
  cat(object$summary)
}



.convertToMatrix <- function(formula, newdata, includeIntercept = T) {
  effects <- strsplit(formula, "~")[[1]][2]
  effects <- gsub("\n", "", effects)
  effects <- gsub(" ", "", effects)
  individualEffects <- strsplit(effects, "+", fixed = T)[[1]]
  matX <- NULL
  eff <- individualEffects[3]
  for (eff in individualEffects) {
    variables <- strsplit(eff, ":")[[1]]
    vector <- NULL
    for (v in variables) {
      if (is.null(vector)) {
        vector <- newdata[,v]
      } else {
        vector <- vector * newdata[,v]
      }
    }
    if (is.null(matX)) {
      matX <- as.matrix(vector)
    } else {
      matX <- cbind(matX, vector)
    }
  }
  if (includeIntercept) {
    matX <- cbind(rep(1, nrow(matX)), matX)
  }
  return(matX)
}


#'
#' Make Predictions Based on the SIMEX Parameter Estimate
#'
#' @param object a SIMEXResult instance
#' @param newdata a data.frame with new data
#' @param originalFormula a string representing the original formula
#'
#' @return a data.frame containing the predictions and their 0.95 confidence intervals
#' @export
predict.SIMEXResult <- function(object, newdata, originalFormula = object$formula) {
  matX <- .convertToMatrix(originalFormula, newdata)
  xBeta <- matX %*% object$coef
  var <- sapply(1:length(matX[,1]), function(i) {
    matX[i,] %*% object$vcov %*% matX[i,]
  })
  lower95 <- xBeta - qnorm(0.975) * var^.5
  upper95 <- xBeta + qnorm(0.975) * var^.5
  if (object$linkFunction == "CLogLog") {
    meanPred <- 1-exp(-exp(xBeta))
    lower95Pred <- 1-exp(-exp(lower95))
    upper95Pred <- 1-exp(-exp(upper95))
  } else if (object$linkFunction == "logit") {
    meanPred <- exp(xBeta) / (1 + exp(xBeta))
    lower95Pred <- exp(lower95) / (1 + exp(lower95))
    upper95Pred <- exp(upper95) / (1 + exp(upper95))
  } else {
    stop(paste("This link function", object$linkFunction, "is not supported!"))
  }
  return(data.frame(meanPred, lower95Pred, upper95Pred))
}


#'
#' Plot the Observed and Predicted Parameter Estimates
#'
#' @param obj an instance of the S3 class SIMEXResult
#'
#' @export
plot.SIMEXResult <- function(object) {
  for (name in object$getEffectNames()) {
    print(object$getPlotForThisParameterEstimate(name, decorated = T))
  }
}

#'
#' Provide the Coefficient (Parameter Estimates) of the Model
#'
#' @param obj an instance of the S3 class SIMEXResult
#'
#' @return a named vector
#'
#' @export
coef.SIMEXResult <- function(object) {
  return(object$coef)
}

#'
#' Provide the Predictions Based on the SIMEX Parameter Estimates
#'
#' @return a vector
#'
#' @export
fitted.SIMEXResult <- function(object) {
  return(object$fitted)
}


#'
#' Provide the Estimated Variance-Covaraince of the Model Coefficients
#'
#' @param obj an instance of the S3 class SIMEXResult
#'
#' @return a matrix
#'
#' @export
vcov.SIMEXResult <- function(object) {
  return(object$vcov)
}

new_SIMEXResult <- function(glmJavaObject,
                            simexJavaObject,
                            formula,
                            linkFunction,
                            fieldWithMeasError,
                            varianceFieldName,
                            nbBootstrapRealizations) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("SIMEXResult")
#  me$.glm <- glmJavaObject

  me$formula <- formula
  me$linkFunction <- linkFunction
  me$fieldWithMeasError <- fieldWithMeasError
  me$varianceFieldName <- varianceFieldName
  me$nbBootstrapRealizations <- nbBootstrapRealizations

  me$obsParmEst <-  .convertJavaDataSetIntoDataFrame(simexJavaObject$getObservedParameterEstimates())
  me$predParmEst <- .convertJavaDataSetIntoDataFrame(simexJavaObject$getPredictedParameterEstimates())

  me$summary <- paste0(glmJavaObject$getSummary(), simexJavaObject$getSummary())
  me$coef <- .convertJavaMatrixToR(simexJavaObject$getParameters())
  names(me$coef) <- unique(me$obsParmEst$parmID)
  me$vcov <- .convertJavaMatrixToR(simexJavaObject$getEstimator()$getParameterEstimates()$getVariance())
  row.names(me$vcov) <- unique(me$obsParmEst$parmID)
  colnames(me$vcov) <- unique(me$obsParmEst$parmID)

  predicted <- simexJavaObject$getPredicted()
  range <- 0:(predicted$m_iRows-1)
  me$fitted <- predicted$getValueAt(range, as.integer(0))

  delayedAssign("getEffectNames",
                #### getModelDataFields ####
                function() {
                  return(names(me$coef))
                },
                assign.env = me)

  delayedAssign("getPlotForThisParameterEstimate",
                #### getModelDataFields ####
                function(parmName, decorated = F) {
                  require(ggplot2)
                  obs <- me$obsParmEst
                  obs_i <- obs[which(obs$parmID == parmName),]
                  pred <- me$predParmEst
                  pred_i <- pred[which(pred$parmID == parmName),]
                  plot <- ggplot2::ggplot() +
                    ggplot2::geom_point(ggplot2::aes(x=zeta, y=obs), obs_i, size = 2) +
                    ggplot2::geom_line(ggplot2::aes(x=zeta, y=pred), pred_i, size = 1)
                  if (decorated) {
                    plot <- ggplot2::ggplot() +
                      ggplot2::geom_point(ggplot2::aes(x=zeta, y=obs), obs_i, size = 3) +
                      ggplot2::geom_point(ggplot2::aes(x=zeta, y=pred), pred_i[which(pred_i$zeta == -1),], shape = 17, fill="black", size = 3) +
                      ggplot2::geom_line(ggplot2::aes(x=zeta, y=pred), pred_i[which(pred_i$zeta >= 0),], size = 1) +
                      ggplot2::geom_line(ggplot2::aes(x=zeta, y=pred), pred_i[which(pred_i$zeta < 0),], lty = "dashed", size = 1) +
                      ggplot2::xlab("Variance inflation") +
                      ggplot2::ylab(parmName) +
                      ggplot2::theme(text = ggplot2::element_text(size=18),
                          axis.text = ggplot2::element_text(size=18),
                          axis.line = ggplot2::element_line(colour = "black"),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.ticks.length = ggplot2::unit(3,"mm"),
                          panel.border = ggplot2::element_blank())
                  } else {
                    plot <- ggplot2::ggplot() +
                      ggplot2::geom_point(ggplot2::aes(x=zeta, y=obs), obs_i) +
                      ggplot2::geom_line(ggplot2::aes(x=zeta, y=pred), pred_i)
                  }
                  return(plot)
                },
                assign.env = me)
  return(me)
}





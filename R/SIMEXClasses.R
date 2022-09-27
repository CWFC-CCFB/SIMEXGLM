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
  cat(object$.glm$getSummary())
  cat(object$.simex$getSummary())
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
#' @export
coef.SIMEXResult <- function(object) {
  return(.convertJavaMatrixToR(object$.simex$getParameters()))
}

#'
#' Provide the Estimated Variance-Covaraince of the Model Coefficients
#'
#' @param obj an instance of the S3 class SIMEXResult
#'
#' @export
vcov.SIMEXResult <- function(object) {
  return(.convertJavaMatrixToR(object$.simex$getEstimator()$getParameterEstimates()$getVariance()))
}


new_SIMEXResult <- function(glmJavaObject, simexJavaObject) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("SIMEXResult")
  me$.glm <- glmJavaObject
  me$.simex <- simexJavaObject

  me$obsParmEst <-  .convertJavaDataSetIntoDataFrame(me$.simex$getObservedParameterEstimates())
  me$predParmEst <- .convertJavaDataSetIntoDataFrame(me$.simex$getPredictedParameterEstimates())

  delayedAssign("getEffectNames",
                #### getModelDataFields ####
                function() {
                  obs <- me$obsParmEst
                  return(unique(obs$parmID))
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





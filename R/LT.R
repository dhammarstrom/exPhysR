#' Calculate and plot lactate breakpoint (lactate threshold)
#' @export
#' @description Calculates workload at the lactate breakpoint. Based on functions described by Newell et al. 2007 (see reference)
#' @param data A data frame with work intensity (e.g. watt) and lactate measurements
#' @param workload.column An integer specifying the column with work intensity data, default=1. 
#' @param lactate.column An integer specifying the column with lactate data, default=2.
#' @param plot Logical specify whether to add plot or not. The plot shows data points with the model fit
#' @return Work intensity at a specified rise from baseline lactate concentration 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @references 
#' Software for calculating blood lactate endurance markers. 
#' John Newell , David Higgins , Niall Madden , James Cruickshank , Jochen Einbeck , Kenny McMillan , Roddy McDonald. 
#' Journal of Sports Sciences, Vol. 25, Iss. 12, 2007

LT<-function(data, workload.column=1, lactate.column=2, plot=T){
      
      
      lactate.grid <- seq(from = min(data[,workload.column]), to = max(data[,workload.column]), length = 1000)
      
      lhs<-function(x, break.pt){ifelse(x < break.pt, break.pt - x, 0)}
      
      rhs<-function(x, break.pt){ifelse(x < break.pt, 0, x - break.pt)}
      
      
      LT.bs.reg2<-function(break.pt, y, x)
      {
            lt.fit <- lm(y ~ lhs(x, break.pt) + rhs(x, break.pt),
                         singular.ok = T)
            lt.mse <- (sum(lt.fit$residuals^2))/lt.fit$df.residual
            lt.mse
      }
      
      
      LT.est<-function(lactate2,speed2) {
            LT <- round(optim(speed2[2],LT.bs.reg2, method = "BFGS", y = lactate2, x = speed2)$par, 2)
            lSpeed <- log(speed2)
            lLactate <- log(lactate2)
            LT2 <-round(optim(lSpeed[2],LT.bs.reg2, method = "BFGS", y = lLactate, x = lSpeed)$par, 2)
            c(LT, exp(LT2))
      }   
      
      est.LT <- LT.est(data[,lactate.column], data[,workload.column])

      
      return(est.LT)
}

      




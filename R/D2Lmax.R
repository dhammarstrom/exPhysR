#' Calculate workload at the point of maximum acceleration of the lactate curve
#' @export
#' @description Calculates the workload at the maximum of the second derivate of the 
#' lactate curve. Based on functions described and written by Newell et al. 2007 (see reference)
#' @param data A data frame with work intensity (e.g. watt) and lactate measurements
#' @param workload.column An integer specifying the column with work intensity data, default=1. 
#' @param lactate.column An integer specifying the column with lactate data, default=2.
#' @param plot If true, the function returns a plot


#' @return Work intensity at 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import pspline
#' @references 
#' Software for calculating blood lactate endurance markers. 
#' John Newell, David Higgins, Niall Madden, James Cruickshank, 
#' Jochen Einbeck, Kenny McMillan, Roddy McDonald. 
#' Journal of Sports Sciences, Vol. 25, Iss. 12, 2007

D2Lmax<-function(data, workload.column=1, lactate.column=2, plot=T){
 
      
      
      
      data<-data.frame(data[!(is.na(data[,lactate.column])),])
      
      if(length(data[,lactate.column])<2*3+1){
            warning(paste("Number of workload/lactate measurements are to few, must be more than",2*3))
            return(NA)
      }
      
      
      peaks<-function(series,span=3)
      {
            z <- embed(series, span)
            result<- max.col(z) == 1 + span %/% 2
            result
      }

      fit <- smooth.Pspline(data[,workload.column], data[,lactate.column], norder = 3, df = length(data[,lactate.column])-3, method = 2)
      lactate.grid <- seq(from = min(data[,workload.column]), to = max(data[,workload.column]), length = 1000)
      fit.deriv.2 <- predict(fit, lactate.grid, 2)
      grid <- cbind(lactate.grid, fit.deriv.2)
      d2lmax <- max(lactate.grid[peaks(fit.deriv.2, span = 3)])
      
      
      
   return(d2lmax)   
      
      
}



 
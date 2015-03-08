#' Calculate the 
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


Dmax<-function(data, workload.column=1, lactate.column=2, plot=T){
      
      model <-lm(data[,lactate.column]~data[,workload.column]+I(data[,workload.column]^2)+I(data[,workload.column]^3))
      poly.coefs <- model$coefficients
      
      linbeta<-(data[length(data[,lactate.column]),lactate.column]-data[1,lactate.column])/(data[length(data[,workload.column]),workload.column]-data[1,workload.column])
      
      dmax<-Re(polyroot(c( poly.coefs[2]-linbeta,2*poly.coefs[3],3*poly.coefs[4])))
      dmax<-max(dmax[dmax<=max(data[,workload.column])])
      
      return(dmax)
      
      
      
}
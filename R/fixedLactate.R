#' Calculate and plot lactate threshold based on a fixed lactate concentration
#' @export
#' 
#' @param data A data frame with work intensity (e.g. watt) and lactate measurements
#' @param workload.column A integer specifying the column with work intensity data, default=1. 
#' @param lactate.column A integer specifying the column with lactate data, default=2.
#' @param plot Logical specify whether to add plot or not. The plot shows data points with the model fit
#' @param lactate A number specifying the lacate level at which you want to now the work intensity. 
fixedLactate<-function(data, workload.column=1, lactate.column=2, plot=T, lactate=4){
  require(ggplot2)
  require(dplyr)
  data<-data.frame(data)
  
  model <-lm(data[,lactate.column]~data[,workload.column]+I(data[,workload.column]^2)+I(data[,workload.column]^3))
  poly.coefs <- model$coefficients
  
  fixed.lactate<- Re(polyroot(c(poly.coefs[1]-lactate,poly.coefs[2],poly.coefs[3],poly.coefs[4])))
  
  fixed.lactate<-max(fixed.lactate[fixed.lactate<=max(data[,workload.column])])
  
  if(plot==TRUE){

    names(data)[workload.column]<-"Workload"
    names(data)[lactate.column]<-"Lactate"
    
    
    
      plot<-data%>%
        filter(!is.na(Lactate))%>%
        ggplot(aes(Workload, Lactate))+
        geom_point()+stat_smooth(se=F,
                               method="lm",
                               formula=y~poly(x,3), color="red")
    
    return(list(round(fixed.lactate,1), plot))
      
  }
  
  return(round(fixed.lactate,1))
  
  
}

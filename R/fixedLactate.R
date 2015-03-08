#' Calculate and plot lactate marker based on a fixed lactate concentration
#' @export
#' @description Calculates the workload at a specified fixed lactate concentration. Based on functions described by Newell et al. 2007 (see reference)
#' @param data A data frame with work intensity (e.g. watt) and lactate measurements
#' @param workload.column An integer specifying the column with work intensity data, default=1. 
#' @param lactate.column An integer specifying the column with lactate data, default=2.
#' @param plot Logical specify whether to add plot or not. The plot shows data points with the model fit
#' @param lactate A number specifying the lacate level at which you want to now the work intensity.
#' @return Work intensity at a specified lactate concentration 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @references 
#' Software for calculating blood lactate endurance markers. 
#' John Newell , David Higgins , Niall Madden , James Cruickshank , Jochen Einbeck , Kenny McMillan , Roddy McDonald. 
#' Journal of Sports Sciences, Vol. 25, Iss. 12, 2007
fixedLactate<-function(data, workload.column=1, lactate.column=2, plot=T, lactate=4){

  data<-data.frame(data)
  
  
  
  if(max(data[,lactate.column], na.rm=T)<lactate){
        stop(paste("Desired lactate levels is outside measured range. Must be within ",
                   min(data[,lactate.column], na.rm=T)," - ",max(data[,lactate.column], na.rm=T),
                   " mmol/L.", sep=""))
  }
  
  if(min(data[,lactate.column], na.rm=T)>lactate){
        stop(paste("Desired lactate levels is outside measured range. Must be within ",
                   min(data[,lactate.column], na.rm=T)," - ",max(data[,lactate.column], na.rm=T),
                   " mmol/L.", sep=""))
  }
  
  model <-lm(data[,lactate.column]~data[,workload.column]+I(data[,workload.column]^2)+I(data[,workload.column]^3))
  poly.coefs <- model$coefficients
  
  fixed.lactate<- Re(polyroot(c(poly.coefs[1]-lactate,poly.coefs[2],poly.coefs[3],poly.coefs[4])))
  
  fixed.lactate<-max(fixed.lactate[fixed.lactate<=max(data[,workload.column])])
  
  if(plot==TRUE){

    names(data)[workload.column]<-"Workload"
    names(data)[lactate.column]<-"Lactate"
    
    segment<-data.frame(x=min(data[,workload.column], na.rm=T),
                        xend=fixed.lactate,
                        y=min(data[,lactate.column], na.rm=T),
                        yend=lactate)
    
      plot<-data%>%
        filter(!is.na(Lactate))%>%
        ggplot(aes(Workload, Lactate))+
        geom_point(size=4)+stat_smooth(se=F,
                               method="lm",
                               formula=y~poly(x,3), color="red")+
          geom_segment(data=segment, aes(x=x,xend=xend,y=yend,yend=yend))+
          geom_segment(data=segment, aes(x=xend,xend=xend,y=y,yend=yend))
    
    return(list(round(fixed.lactate,1), plot))
      
  }
  
  return(round(fixed.lactate,1))
  
  
}

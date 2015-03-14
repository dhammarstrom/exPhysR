#' Calculate and plot lactate marker based on a fixed rise from baseline lactate
#' @export
#' @description Calculates the workload at a specified fixed lactate concentration. Based on functions described by Newell et al. 2007 (see reference)
#' @param data A data frame with work intensity (e.g. watt) and lactate measurements
#' @param workload.column An integer specifying the column with work intensity data, default=1. 
#' @param lactate.column An integer specifying the column with lactate data, default=2.
#' @param rise.from.baseline A numeric specifying the rise frome baseline, default=1
#' @param estimate A logical specifying if baseline should be estimated from the model fit or be defined by the lowest measured value, default=TRUE
#' @param plot Logical specify whether to add plot or not. The plot shows data points with the model fit
#' @return Work intensity at a specified rise from baseline lactate concentration 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @references 
#' Software for calculating blood lactate endurance markers. 
#' John Newell , David Higgins , Niall Madden , James Cruickshank , Jochen Einbeck , Kenny McMillan , Roddy McDonald. 
#' Journal of Sports Sciences, Vol. 25, Iss. 12, 2007

riseBaseline<-function(data, workload.column=1, lactate.column=2, rise.from.baseline=1, estimate=T, plot=T){
      
      data<-data.frame(data)
      
      
      model <-lm(data[,lactate.column]~data[,workload.column]+I(data[,workload.column]^2)+I(data[,workload.column]^3))
      poly.coefs <- model$coefficients
    
      if(estimate==TRUE){
            
            workload.grid<-seq(from=data[which.min(data[,workload.column]),workload.column], to=data[which.max(data[,workload.column]),workload.column], length.out = 1000)
            
            lactate.grid<-poly.coefs[1]+poly.coefs[2]*workload.grid+poly.coefs[3]*workload.grid^2+poly.coefs[4]*workload.grid^3
      
            tmp<-data.frame(workload.grid, lactate.grid)
      
            target.lactate<-tmp[which.min(tmp[,2]),2]+rise.from.baseline
      
      } 
      
      if(estimate==FALSE){
            target.lactate<-min(data[,lactate.column], na.rm=T)+rise.from.baseline
      }
      
      lactate.rise<- Re(polyroot(c(poly.coefs[1]-target.lactate,poly.coefs[2],poly.coefs[3],poly.coefs[4])))
      lactate.rise<-max(lactate.rise[lactate.rise<=max(data[,workload.column])])
      
      if(plot==TRUE){
            
            names(data)[workload.column]<-"Workload"
            names(data)[lactate.column]<-"Lactate"
            
            segment<-data.frame(x=min(data[,workload.column], na.rm=T),
                                xend=lactate.rise,
                                y=min(data[,lactate.column], na.rm=T),
                                yend=target.lactate)
            
            plot<-data%>%
                  filter(!is.na(Lactate))%>%
                  ggplot(aes(Workload, Lactate))+
                  geom_point(size=4)+stat_smooth(se=F,
                                                 method="lm",
                                                 formula=y~poly(x,3), color="red")+
                  geom_segment(data=segment, aes(x=x,xend=xend,y=yend,yend=yend))+
                  geom_segment(data=segment, aes(x=xend,xend=xend,y=y,yend=yend))
            
            return(list(round(lactate.rise,1), plot))
            
      }
      
      return(round(lactate.rise,1))
      
      
}
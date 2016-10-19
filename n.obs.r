#################################################################
# n.obs - function to count number of non-missing observation
#
# Author: Alan Haynes, Peter Waldner
#
# Usage: e.g. to count number of observations in y1,y2 for groups of x1,x2
#   dat3 = aggregate(dat[,c("y1","y2"),by=list(dat$x1,dat$x2),n.obs)
#   names(dat3)=c("x1","x2","y1","y2")
#

n.obs <- function(x, na.rm=TRUE)
{
   if(is.numeric(x)) 
   {
     if(na.rm==TRUE) x = na.omit(x)
     if(class(x)=="data.frame") n=length(x[,1])
     if(class(x)=="numeric") n=length(x)
     if(class(x)=="integer") n=length(x)
     if(!exists("n")) n=0
   }
   if(is.character(x))
   {
     x=na.omit(x)  
     if(class(x)=="data.frame") n=length(x[nchar(x)>0,1])
     if(class(x)=="numeric") n=length(x)
     if(class(x)=="integer") n=length(x)
     if(class(x)=="character") n=length(x[nchar(x)>0])
     if(!exists("n")) n=0
   }
   return(n)
}

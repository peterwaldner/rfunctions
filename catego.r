##############################################################
# catego - function to split continuous data into ranges
# 
# Description
#  splits value range using hist() and returns text with range description
# Usage 
#  xc=categorise(x,breaks=5)
#  xc=categorise(x,breaks=c(0,20,30))
catego=function(x,breaks=5,categorise=TRUE)
{
     # categorisation of numeric legend variable
     if (categorise==TRUE & is.numeric(x))
     {
        # categorise lvar with lbreaks
        breaks=hist(x,breaks=breaks,plot=FALSE)$breaks
        litems=paste(breaks[1:length(breaks)-1],breaks[2:length(breaks)],sep="-")
        litemvals=findInterval(x,breaks)
        if (max(breaks)==max(x,na.rm=TRUE)) {
          litemvals[na.omit(litemvals==max(litemvals,na.rm=TRUE))]=litemvals[na.omit(litemvals==max(litemvals,na.rm=TRUE))]-1
        }
     } else {
        litems=unique(x)
        litemvals=match(x,litems)
     }
     # 
     for (i in 1:length(x))
     {
        if (i==1) {
           y=litems[litemvals[i]]
        } else {
           y=c(y,litems[litemvals[i]])
        }
     }
     return(y)
}

# catego(chb2$ntot2007)

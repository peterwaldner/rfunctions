########################################
# histby - overlay of histogramms with Frequency (%)
#
# Description
#  plots histogramms
# Usage
#  histby(d,xvar="x",lvar="x")
# Details
#  returns brake values for xvar
# Arguments
#  d         data frame
#  xvar      name of variable for histogramm
#  lvar      name of variable with legend categories
#  plottitle main title
#  ltitle    legend title 
#  lpos      position of legend
#  cols      colors for symbols
#             NA > rainbow will be taken
#             c("red","green") > colorRampPalette will be taken
#             c("red","blue","green","olive") > colors will be taken
#  categorise  categorise legend items
#             TRUE=categorise numeric data 
#             FALSE=no categorisation is done
#  lbreaks   number of or list of values for categorisation of numeric legend data
#  breaks    number of or list of values for categorisation of numeric histogramm data
#  sorting   of legend items TRUE/FALSE
#  n         show number of yvar per legend item TRUE/FALSE
#  ylim      ylim
#  xlim      xlim
#  y         type of values on y-axis
#            "counts" percentage of counts for each dot (default)
#            "density" density for each histogramm bar (that is plotted as point in fact)
#            "scaled" count scaled density
#  xaxs      "i" do not plot margin within plot area on x-axes
#  yaxs      "i" do not plot margin within plot area on y-axes
#  type      "b" plot points and lines
#  pch       point symbol, e.g. 20=filled circle

histby=function(d,xvar,lvar,breaks=NA,xbreaks=NA,lbreaks=4,
       categorise=TRUE,sorting=TRUE,lpos="topright",n=TRUE,
       plottitle=NA,ltitle=NA,
       xlab=NA,ylab="Haeufigkeit (%)",
       type="b",pch=20,cols=NA,y="scaled",
       ylim=c(0,30),xlim=NA,xaxs="i",yaxs="i",...)
{
  
  # arguments
  if (is.na(ltitle)) ltitle=lvar; 
  if (is.na(xlab)) xlab=xvar;
  if (!is.na(xbreaks)[1]) breaks=xbreaks; 

  # categorisation of numeric legend variable
  if (categorise==TRUE & is.numeric(d[,lvar]))
  {
     # categorise lvar with lbreaks
     lbreaks=hist(d[,lvar],breaks=lbreaks,plot=FALSE)$breaks
     litems=paste(lbreaks[1:length(lbreaks)-1],lbreaks[2:length(lbreaks)],sep="-")
     litemvals=findInterval(d[,lvar],lbreaks)
     if (max(lbreaks)==max(d[,lvar],na.rm=TRUE)) {
       litemvals[na.omit(litemvals==max(litemvals,na.rm=TRUE))]=litemvals[na.omit(litemvals==max(litemvals,na.rm=TRUE))]-1
     }
  } else {
     litems=unique(d[,lvar])
     if (sorting==TRUE) {litems=sort(litems);}
     litemvals=match(d[,lvar],litems)
  }

  # number of values per legend item
  d1=cbind(d,litemvals)
  litems_n=means(d1,byvars="litemvals",nvars=xvar)
  # colors
  if (is.na(cols)[1]) {
    cols=rainbow(length(litems)+1); 
  } else {
    if (length(cols)==2) {
       cols=colorRampPalette(cols)(length(litems)+1);
    } else {
       cols=cols;
    }
  }
  # histogramm breaks
  if (is.na(breaks)[1]) {
     breaks=hist(d[,xvar],plot=FALSE)$breaks;
  } else {
     breaks=hist(d[,xvar],plot=FALSE,breaks=breaks)$breaks;
  }
  
  # lims
  if (is.na(xlim)[1]) xlim=c(min(breaks),max(breaks))
  
  # litems
  for (i in 1:length(litems)) {
     litem=litems[i]
     a=hist(d[litemvals==i,xvar],plot=FALSE,breaks=breaks)  
     if(y=="counts") {
       # plot percentage of counts on y-axes
       if(i==1) {
          plot(a$mids,a$counts/sum(a$counts)*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,
             xlim=xlim,ylim=ylim,
             main=plottitle,xlab=xlab,ylab=ylab,pch=pch,...);
       } else {
          points(a$mids,a$counts/sum(a$counts)*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,pch=pch,...); 
       }
     } else {
      if (y=="scaled") {
       # plot scaled density on y-axis
       if(i==1) {
          plot(a$mids,a$density/sum(a$density)*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,
             xlim=xlim,ylim=ylim,
             main=plottitle,xlab=xlab,ylab=ylab,pch=pch,...);
       } else {
          points(a$mids,a$density/sum(a$density)*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,pch=pch,...); 
       }
      } else {
       # plot density on y-axis
       if(i==1) {
          plot(a$mids,a$density*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,
             xlim=xlim,ylim=ylim,
             main=plottitle,xlab=xlab,ylab=ylab,pch=pch,...);
       } else {
          points(a$mids,a$density*100,type=type,col=cols[i],xaxs=xaxs,yaxs=yaxs,pch=pch,...); 
       }
      }
     }
  }
  # legend with (n=...) or without
  if (n==TRUE) {
    legend(lpos,legend=paste(litems[litems_n[,1]]," (n=",litems_n[,2],")",sep=""),
      col=cols[litems_n[,1]],pch=pch,bty="n",title=ltitle,...)
  } else {
    legend(lpos,legend=litems[litems_n[,1]],col=cols[litems_n[,1]],pch=pch,bty="n",title=ltitle,...)
  }
  # returns brake values for xvar
  return(xbreaks=breaks)
}

# source(paste(rpath,"histby.r",sep="")); histby(chb2,"cnfh","hufo_chb"); 

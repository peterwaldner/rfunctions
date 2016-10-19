########################################
# p - plot Swiss map with colored data points 
#
# Description
#  plots continuous (a) or categorised (b) data on a map of Switzerland
# Usage
#  plotch(d=d,)
# Arguments
#  d         data frame
#  lvar      name of variable 
#  varxy     name of variable with x and y coordinates (CH1903+)
#  plottitle main title
#  ltitle    legend title 
#  cex       size of symbols
#  cols      colors for symbols
#             NA > rainbow will be taken
#             c("red","green") > colorRampPalette will be taken
#             c("red","blue","green","olive") > colors will be taken
#  pch       symbol
#  lpos      position of legend
#  rpath     path for chgrenze.shp and gew_poly_polygon.shp
#  cat       categorise legend items
#             TRUE=categorise numeric data 
#             FALSE=no categorisation is done
#  nbreaks   number of or list of breaks for categorisation of numeric data
#  sorting   sorting of categorised data
#  rpath     directory containing a subdirectory 'gis' 
#             with Shape-Files chgrenze.shp and gew_poly_polygon.shp

plotch=function(d,lvar,varxy=c("x","y"),plottitle=NA,
       ltitle=NA,categorise=TRUE,lbreaks=4,cex=0.9,cols=NA,pch=19,lpos="topleft",
       gispath=NA,
       sorting=TRUE,n=TRUE,...)
{
  # packages
  require(proj4)
  require(sp)
  require(rgdal)
  require(mapproj)
  require(maps)
  require(mapdata)
  require(maptools)
  
  # arguments
  # plottitle
  if (is.na(ltitle)) ltitle=lvar; 
  # default directory for Shape-Files
  if (is.na(gispath)) { gispath=paste(rpath,"gis/",sep=""); } else {gispath=gispath; }

  # background shape files
  if (!exists("chgrenze")) {
  chgrenze=readShapeLines(paste(gispath,"chgrenze.shp",sep=""));
  gew_poly_polygon=readShapePoly(paste(gispath,"gew_poly_polygon.shp",sep=""));
  }

  # plot map 
  par(mar=c(0,0,1,0));
  plot(chgrenze,xaxs="i",yaxs="i",main=plottitle)
  plot(gew_poly_polygon,col="grey",add=TRUE)

  # categorisation of numeric data 
  if (categorise==TRUE & is.numeric(d[,lvar]))
  {
     # categorise data
     breaks=hist(d[,lvar],breaks=lbreaks,plot=FALSE)$breaks
     litems=paste(breaks[1:length(breaks)-1],breaks[2:length(breaks)],sep="-")
     litemvals=findInterval(d[,lvar],breaks)
  } else {
     litems=unique(d[,lvar])
     if (sorting==TRUE) {litems=sort(litems);}
     litemvals=match(d[,lvar],litems)
  }

  # number of values per legend item
  d1=cbind(d,litemvals,stringsAsFactors=FALSE)
  # function aggregate failed, so use work around. 
  if (is.character(d1[,lvar])) {
      litems_n=data.frame(litemval=0,n=0,stringsAsFactors=FALSE)
      for (i in 1:length(litems)) {
          litems_n[i,1]=i
          litems_n[i,2]=length(litemvals[litemvals==i])
      }
  } else {
      litems_n=means(d1,byvars="litemvals",nvars=lvar)
  }
  
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
  
  # pch
  if (length(pch)==1) {
    pch1=pch
    pch2=pch
  } else {
    pch1=pch[litemvals]
    pch2=pch[litems_n[,1]]
  }
  
  # plot data and legend
  dxy=d
  coordinates(dxy)=varxy
  points(dxy,pch=pch1,cex=cex,col=cols[litemvals],...)
  # legend with (n=...) or without
  if (n==TRUE) {
    legend(lpos,legend=paste(litems[litems_n[,1]]," (n=",litems_n[,2],")",sep=""),
      col=cols[litems_n[,1]],pch=pch2,cex=cex,bty="n",title=ltitle,...)
  } else {
    legend(lpos,legend=litems[litems_n[,1]],col=cols[litems_n[,1]],
       pch=pch2,cex=cex,bty="n",title=ltitle,...)
  }
  par(mar=c(5, 4, 4, 2) + 0.1);
  
  # returns brake values for xvar
  if (exists("breaks")) {
      return(xbreaks=breaks)
  }
}

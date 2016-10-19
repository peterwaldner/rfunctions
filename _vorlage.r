#######################################################################
#                                      R-Programme, {Datum}, {Autor}
# {TITEL}
# --------------------------------------
#   Files: {input} > {output}
#######################################################################

# -----------------------------------------------------------
# TODO

# -----------------------------------------------------------
# SETTINGS
  # Memory
  rm(list=ls(all=TRUE));
  memory.size(4000);
  
  # Directories (replace \ with /)
  path="O:/LWF"
  rpath="O:/lwf/Auswertungen/rfunctions"
  
  # Functions
  source(paste(rpath,"/rfunctions.r",sep=""))
  # install.packages("RODBC")
  # source(paste(rpath,"/database_connection.r",sep=""))
  
  # Packages
  # install.packages("")
  # library()
 
# -----------------------------------------------------------
# IMPORT

  # DAT Import tabulator delimited
  da = read.table(paste(path,"/","dat.dat",sep=""),
     sep="\t",header=TRUE,stringsAsFactors=FALSE); 
  names(dat)=tolower(dat);
  for (var in c("DATE")) {dat[,var]=as.Date(dat[,var],format="%Y-%m-%d");}

  # CSV Import semicolon delimited (for Regional Settings German)
  db = read.table(paste(path,"/","dat.csv",sep=""),
     sep=";",header=TRUE,stringsAsFactors=FALSE); 
  names(dat)=tolower(dat);
  for (var in c("DATE")) {dat[,var]=as.Date(dat[,var],format="%Y-%m-%d");}

  # Database Import
  # remove '#' before 'source .. database_connection ..' above
  dc = sql("select * from dual"); 
  
# -----------------------------------------------------------
# LOGISTIK
  # using lists of variables
  byvars=c("a","b");
  dvars=c("c","d")

  # Merge datasets using byvars as key
  d2=merge(da,db,by=c("a","b"),stringsAsFactor=FALSE)
  d2=merge(da,db,by=byvars,stringsAsFactor=FALSE)

  # Aggregate
  # calculate values
  dm=means(d2,byvars="a",meanvars=c("c","d"),sdvars=c("c","d"),nvars=c("c","d"))
  dm=means(d2,byvars=byvars,meanvars=dvars,sdvars=dvars,nvars=dvars)
  # completness criteria
  for (dvar in 1:length(dvars)) {
    criteria=dm[,sufix(dvar,"_n")]<5
    dm[criteria,dvar]=NA
    dm[criteria,sufix(dvar,"_sd")]=NA
  }
  

# -----------------------------------------------------------
# PLOT
  plotby(d2,"c","d",lvar="b",byvar="d",type="p",dev="scr")
  

# -----------------------------------------------------------
# STATISTICS

# Linear Regression
  mod=lm(d~c,data=d2)
  summary(mod)
  plot(mod)
  plotmod(mod,nplots=1)
  
# -----------------------------------------------------------
# MAP  
  install.packages("proj4")
  install.packages("sp")
  install.packages("rgdal")
  install.packages("mapproj")
  install.packages("maps")
  install.packages("mapdata")
  install.packages("maptools") # requires foreign, lattice

  library(proj4)
  library(sp)
  library(rgdal)
  library(mapproj)
  library(maps)
  library(mapdata)
  library(maptools)

  # background shape files
  chgrenze=readShapeLines("O:/LWF/Projekte/NitLeach/gis/chgrenze.shp")
  gew_poly_polygon=readShapePoly("O:/LWF/Projekte/NitLeach/gis/gew_poly_polygon.shp")

  # plot Swiss map with data
  plotch(d,"c",plottitle="sites",ltitle="c")
  
  # arguments
  maintitle="CHB"
  legendtitle="C/N 0-10 cm"
  lvar="cn10o"
  nbreaks=4
  nbreaks=c(0,15,20,25,30,80)
  d=chb2
  varxy=c("x","y")
  # draw
  par(mar=c(0,0,1,0));
  plot(chgrenze,xaxs="i",yaxs="i",main=plottitle)
  plot(gew_poly_polygon,col="grey",add=TRUE)
  breaks=hist(d[,lvar],breaks=nbreaks,plot=FALSE)$breaks
  cols=rainbow(length(breaks))
  cols=colorRampPalette(c("red","blue","green"))(length(breaks))
  cols=c("red","orange","yellow","green","darkgreen")
  dxy=d
  coordinates(dxy)=varxy
  points(dxy,pch=19,cex=0.9,col=cols[findInterval(d[,lvar],breaks)])
  litems=paste(breaks[1:length(breaks)-1],breaks[2:length(breaks)],sep="-")
  legend("topleft",legend=litems,col=cols,pch=19,cex=1,bty="n",title=ltitle)
  par(mar=c(5, 4, 4, 2) + 0.1);
  
  # plot map with category data
  # arguments
  plottitle="CHB"
  ltitle="Bodentyp"
  lvar="btyp_chb"
  d=chb2
  varxy=c("x","y")
  # draw
  par(mar=c(0,0,1,0));
  plot(chgrenze,xaxs="i",yaxs="i",main=plottitle)
  plot(gew_poly_polygon,col="grey",add=TRUE)
  litems=unique(d[,lvar])
  cols=c("red","orange","yellow","green","darkgreen")
  cols=colorRampPalette(c("red","green"))(length(litems))
  points(dxy,pch=19,cex=0.9,col=cols[match(d[,lvar],litems)])
  legend("topleft",legend=litems,col=cols,pch=19,cex=1,bty="n",title=ltitle)
  par(mar=c(5, 4, 4, 2) + 0.1);
  
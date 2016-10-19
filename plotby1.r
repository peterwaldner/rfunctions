###############################################################################
# plotby - Plotting xy diagrams from a dataframe
#
# Features
#   one to multi Y-Variables
#   a legend entry per l-Variable (and Y-Variable)
#   a panel per p-Variable
#   one page per by-Variable
#   type (line/point) depending on s-Variable
#   creating png-Files
# 
# Syntax
#   plotby(dataframe,xvar,yvars,lvar,byvar,pvar,svar,type,lty,pch=pch,colorlist,
#          plottitle,xlab,ylab,ltitle,lpos,
#          dev,path,file,width,height,units,dpi)
# Arguments
#   dataframe	dataframe with the data to plot
#   xvar       X-Variable 
#               *1: only one X-Variable allowed
#               see plot(), points(), lines()
#
#   yvars      Y-Variable(s) 
#               *1: one Y-Variable, lvar will be used to define XY-lines and legend entry
#               *2: multiple Y-Variables, they appear as XY-lines (or ploints) and legend entry
#               see plot(), points(), lines()
#
#   lvar       Legend-Variables *1, one XY-line (or points) for each value of ...
#                see legend()
#
#   byvar      By-Variable *1, one plot (or file) for each value of ...
#
#   pvar       Panel-Variable *1, one plot for each value of ... 
#                see op = par(mfrow=...)
#
#   svar       Symbol-Variable *1, defines symbol of XY-points (cf. plot)
#
#   type       "l"=line, "p"=points 
#   lty        linetype
#   pch=pch        symbol
#   colorlist  list of colors used for the XY-lines (or points)
#   plottitle  Title of the plot, value of By-Variable will be added if used 
#   xlab       Title of the X-Axis
#   ylab       Title of the Y-Axis, 
#                if multiple Y-Variable are plotted, name of Y-Variable will be added
#              see plot(), points(), lines()
#
#   ltitle     Title of the Legend
#   lpos       Position of the Legend
#              see legend()
#   
#   dev        Device to show or save plots
#              "screen"  plots will be shown in R Graphics window 
#                 make shure to have set History > Aufzeichnen before starting
#              "png"     plots will be saved into PNG files
#   path       Path-name to save the graphic-files. Has to end with "/"
#   file       First letters of the filenames, values of the By-Variable and ".png" will be added
#   width,height,units,dpi: properties of the graphic files
#              see png()
#
#   printnames TRUE=Names of all Variables in the dataframe will be printed
#              FALSE=...will not be printed.
#
# Explanation
#   *1) string with the name of the column in the dataframe to be used as ...
#   *2) string with the name of the column in the dataframe to be used as ...
#       or list of string with the names of columns to be used as .. 
#
# Version 4
#    
# Author
#   Peter Waldner, WSL, 2011

plotby <- function (
   d, xvar, yvars, 
   lvar = "", 
   byvar = "", 
   pvar = "",
   svar = "",
   type = "l", lty=1, pch=1, colorlist = c("orange","green","blue","red","brown","black","grey","cyan","magenta","brown","black","black"),
   plottitle = "", xlab = "auto", ylab = "auto", 
   ltitle = "", lpos = "topleft", 
   dev="screen",path = paste(loc,"png/",sep=""), file = "", width = 12, height = 10, units = "cm", dpi=300,
   printnames=FALSE,
   ...)
{
  # print d
  if (printnames==TRUE){
  print(names(d))
  }
  # subset d (safe memory)
  vars = c(xvar, yvars)
  if (!(lvar == "")) vars = c(vars,lvar)
  if (!(byvar == "")) vars = c(vars,byvar)
  if (!(pvar == "")) vars = c(vars,pvar)
  if (!(svar == "")) vars = c(vars,svar)
  d = d[,vars]
  
  # init adjustments
  typeori=type
  marori =par("mar")
  
  # path
  if (dev!="screen")
  {
   if (substr(path,nchar(path),nchar(path))=="/") 
   {path;} else
   {path=paste(path,"/",sep="");}
   # substr(path,1,nchar(path)-1)
   if (dev=="png") {print(path);}
  }
  
  # if byvar
  if (byvar == "") 
  {
    
    # init device
    if (dev == "png") png(paste(path,file,"_",yvars[1],".png",sep=""),width=width,height=height,units=units,res=dpi)
    
    # no panels
    # ---
    if ((pvar == ""))
    {

    #->
    #
    if (lvar == "")
    {
      # no sort data
      d = d[order(d[,xvar]),]
    }
    else
    {
      # sort data
      d = d[order(d[,lvar],d[,xvar]),]
    }

    # init litem
    ilitem = 0
    
    # axis range
    xlims = range(d[,xvar], na.rm=TRUE)
    ylims = range(d[,yvars], na.rm=TRUE)
    
    # xlab
	 if (xlab=="auto") {
	    if (xvar == "t" | xvar == "year") xlabtext = "" else xlabtext = xvar
	 }
	 else {xlabtext=xlab}
	 # ylab
    if (ylab=="auto") {
       ylabtext=paste(yvars,collapse=",")
    }
    else {ylabtext=ylab}
    # plot frame
    plot(xlims, ylims, type="n", 
       main=paste(plottitle), 
       ylab=ylabtext,xlab=xlabtext,
       ...)
    
    # loop over yvars
    for (yvar in yvars)
    {
      # if lvar
      if (lvar=="")
      {
        # legend item counter
        ilitem = ilitem + 1
        

        # svar
        if (!(svar == ""))
        {
          sval = unique( d[,svar] )[1]
          if (!(is.na(sval))) type = as.character(sval)
        }

        # plot line
        lines(d[,xvar],
              d[,yvar],
              type, lty, col=colorlist[ilitem], pch=pch)
        # if yvars
        if (length(yvars) == 1)
        {
          # no legend item
        }
        else
        {
          # yvar legend
          litem = as.character(yvar)
          # add to litems
          if (ilitem == 1) { litems = litem} 
          if (ilitem >1 )  { litems = c(litems,litem) }
        }
      } # endif no lvar
      else
      {
        # loop over uniqe(lvar)
        lvals = unique(d[,lvar])
        for (ilval in 1:length(lvals))
        {
          lval = lvals[ilval]

          # legend item counter
          ilitem = ilitem +1
          
          # svar
          if (!(svar == ""))
          {
            sval = unique( d[d[,lvar] == lval,svar] )[1]
            if (!(is.na(sval))) type = as.character(sval)
          }
          
          # plot line
          lines(d[d[,lvar] == lval,xvar],
                d[d[,lvar] == lval,yvar],
                type, lty, col=colorlist[ilitem],pch=pch)
          
          # if yvars
          if (length(yvars) == 1)
          {
            # lvar legend item
            litem = as.character(lval)
            # add to litems
            if (ilitem == 1) {litems = litem} else { litems = c(litems,litem) }
          } # endif yvars
          else
          {
            # yvars lvar legend item
            litem = paste(as.character(yvar), as.character(lval))
            # add to litems
          if (ilitem == 1) {litems = litem} 
          if (ilitem >1 )  { litems = c(litems,litem) }
          }
        } # end loop lvar
      } # endif lvar
    } # end loop yvars
    # create legend
    if (!lvar == " " | !length(yvars) == 1 )
    {
     if (!lpos == "none")
     {
      if (paste(ltitle,lvar) == " ")
      {
        legend(lpos, litems, col=colorlist[1:length(litems)], lty=1)
      }
      if (!(paste(ltitle,lvar) == " "))
      {
        legend(lpos, litems, title=paste(ltitle, lvar), col=colorlist[1:length(litems)], lty=1)
      }
     }
    }
    
    #
    #->

    } # end no panels

    # panels
    # ---
    if (!(pvar == ""))
    {
      pvals = unique(d[,pvar])
      op = par(mfrow=c(length(pvals),1))
      # loop pval
      for (ipval in 1:length(pvals))
      {
        pval = pvals[ipval]
        # Margins
		  mar = marori
		  if (!(ipval == 1)) mar[3]=1
		  if (!(ipval == length(pvals))) mar[1]=1
		  op = par(mar=mar)

    #-->
    #

    if (lvar == "")
    {
      # sort data
      d = d[order(d[,pvar],d[,xvar]),]
    }
    else
    {
      # sort data
      d = d[order(d[,pvar],-d[,lvar],d[,xvar]),]
    }

    # init litem
    ilitem = 0
    
    # axis range
    xlims = range(d[,xvar], na.rm=TRUE)
    ylims = range(d[,yvars], na.rm=TRUE)
    
        # xlab
	     if (xlab=="auto") {
	        if (xvar == "t" | xvar == "year") xlabtext = "" else xlabtext = xvar
	     }
	     else {xlabtext=xlab}
	     # ylab
        if (ylab=="auto") {
           ylabtext=paste(yvars,collapse=",")
        }
        else {ylabtext=ylab}
	     
	     # plot frame
	     plot(xlims, ylims, type="n", 
	        main=paste(plottitle), 
	        ylab=ylabtext,xlab=xlabtext,
	        ...)
	     

    
    # loop over yvars
    for (yvar in yvars)
    {
      # if lvar
      if (lvar=="")
      {
        # legend item counter
        ilitem = ilitem + 1
        

        # svar
        type=typeori
        if (!(svar == ""))
        {
          sval = unique( d[d[,pvar] == pval,svar] )[1]
          if (!(is.na(sval))) type = as.character(sval)
        }

        # plot line
        lines(d[d[,pvar] == pval,xvar],
              d[d[,pvar] == pval,yvar],
              type, lty, col=colorlist[ilitem], pch=pch)
        # if yvars
        if (length(yvars) == 1)
        {
          # no legend item
        }
        else
        {
          # yvar legend
          litem = as.character(yvar)
          # add to litems
          if (ilitem == 1) { litems = litem} 
          if (ilitem >1 )  { litems = c(litems,litem) }
        }
      } # endif no lvar
      else
      {
        # loop over uniqe(lvar)
        lvals = unique(d[,lvar])
        for (ilval in 1:length(lvals))
        {
          lval = lvals[ilval]

          # legend item counter
          ilitem = ilitem +1
          
          # svar
          type=typeori
          if (!(svar == ""))
          {
            sval = unique( d[d[,pvar] == pval & d[,lvar] == lval,svar] )[1]
            if (!(is.na(sval))) type = as.character(sval)
          }
          
          # plot line
          lines(d[d[,pvar] == pval & d[,lvar] == lval,xvar],
                d[d[,pvar] == pval & d[,lvar] == lval,yvar],
                type, lty, col=colorlist[ilitem],pch=pch)
          
          # if yvars
          if (length(yvars) == 1)
          {
            # lvar legend item
            litem = as.character(lval)
            # add to litems
            if (ilitem == 1) {litems = litem} else { litems = c(litems,litem) }
          } # endif yvars
          else
          {
            # yvars lvar legend item
            litem = paste(as.character(yvar), as.character(lval))
            # add to litems
          if (ilitem == 1) {litems = litem} 
          if (ilitem >1 )  { litems = c(litems,litem) }
          }
        } # end loop lvar
      } # endif lvar
    } # end loop yvars
    # create legend
    if (!lvar == " " | !length(yvars) == 1 )
    {
     if (!lpos == "none")
     {
      if (paste(ltitle,lvar) == " ")
      {
        legend(lpos, litems, col=colorlist[1:length(litems)], lty=1)
      }
      if (!(paste(ltitle,lvar) == " "))
      {
        legend(lpos, litems, title=paste(ltitle, lvar), col=colorlist[1:length(litems)], lty=1)
      }
     }
    }
    
    #
    #-->
      } # end loop pvals
      # ---
    } # end if panels

    # end device
    if (dev == "png") dev.off()


  } # end if no byvars
  
  # if byvars
  if (!(byvar == ""))
  {

    # no panels
    # ---
    if ((pvar == ""))
    {

    #->
    #

    # sort
    if (lvar == "")
    {
      # sort data byvar
      d = d[order(d[,byvar],d[,xvar]),]
    }
    else
    {
      # sort data byvar, lvar
      d = d[order(d[,byvar],d[,lvar],d[,xvar]),]
    }
 
    # loop over byvar
    byvals = unique(d[,byvar])
    for (ibyval in 1:length(byvals))
    {
      byval = byvals[ibyval]
      if (!(is.numeric(byval)))
      {byval = as.character(byval)}
      if (printnames==TRUE){
      print(byval)
      }
     
      # init device
      if (dev == "png") png(paste(path,file,"_",yvars[1],"_",byval,".png",sep=""),width=width,height=height,units=units,res=dpi)

      # init litem
      ilitem = 0
      
      # axis range
      xlims = range(d[,xvar], na.rm=TRUE)
      ylims = range(d[,yvars], na.rm=TRUE)
      
        # xlab
	     if (xlab=="auto") {
	        if (xvar == "t" | xvar == "year") xlabtext = "" else xlabtext = xvar
	     }
	     else {xlabtext=xlab}
	     
	     # ylab
	     if (ylab=="auto") {
	        ylabtext=paste(yvars,collapse=",")
	     }
	     else {ylabtext=ylab}
	     
	     # plot frame
	     plot(xlims, ylims, type="n", 
	        main=paste(plottitle,byval), 
	        ylab=ylabtext, xlab=xlabtext, 
	        ...)
      
      # loop over yvars
      for (yvar in yvars)
      {
        # if lvar
        if (lvar == "")
        {
          # legend item counter
          ilitem = ilitem + 1
          
          # svar
          type=typeori
          if (!(svar == ""))
          {
            sval = unique( d[d[,byvar] == byval,svar] )[1]
            if (!(is.na(sval))) type = as.character(sval)
          }
          
          # plot line
          lines(d[d[,byvar] == byval,xvar],
                d[d[,byvar] == byval,yvar],
                type, lty, col=colorlist[ilitem], pch=pch)
          
          # if yvars
          if (length(yvars) == 1)
          {
            # no legend item
          }
          else
          {
            # yvar legend
            litem = as.character(yvar)
            # add to litems
            if (ilitem == 1) {litems = litem} 
            if (ilitem >1 )  { litems = c(litems,litem) }
          }
        } # endif no lvar
        if (!(lvar == ""))
        {
          # loop over uniqe(lvar)
          lvals = unique(d[,lvar])
          for (ilval in 1:length(lvals))
          {
            lval = lvals[ilval]
            
            # legend item counter
            ilitem = ilitem +1
            
            # svar
            type=typeori
            if (!(svar == ""))
            {
              sval = unique( d[d[,byvar] == byval & d[,lvar] == lval,svar] )[1]
              if (!(is.na(sval))) type = as.character(sval)
            }

            # plot line
            lines(d[d[,byvar] == byval & d[,lvar] == lval,xvar],
                  d[d[,byvar] == byval & d[,lvar] == lval,yvar],
                  type, lty, col=colorlist[ilitem],pch=pch)
            
            # if yvars
            if (length(yvars) == 1)
            {
              # lval legend item
              litem = as.character(lval)
              # add to litems
              if (ilitem == 1) {litems = litem} 
              if (ilitem >1 )  { litems = c(litems,litem) }
            } # endif yvars
            else
            {
              # yvars lval legend item
              litem = paste(as.character(yvar), as.character(lval))
              # add to litems
              if (ilitem == 1) {litems = litem} 
              if (ilitem >1 )  { litems = c(litems,litem) }
            }
          } # end loop lvar
        } # endif lvar
      } # end loop yvars
      # create legend
      if (!lpos == "none")
      {
       if (paste(ltitle,lvar) == " ")
       {
         legend(lpos, litems, col=colorlist[1:length(litems)], lty=1)
       }
       if (!(paste(ltitle,lvar) == " "))
       {
         legend(lpos, litems, title=paste(ltitle, lvar), col=colorlist[1:length(litems)], lty=1)
       }
      }

      # init device
      if (dev == "png") dev.off()

    } # end loop byvar

    #->
    } # end no panels


    # panels
    # ---
    if (!(pvar == ""))
    {
      # equal pvals for all byvals
      # pvals = unique(d[,pvar])
      # print(length(pvals))
      # op = par(mfrow=c(length(pvals),1))
      
    #->
    #

    # sort
    if (lvar == "")
    {
      # sort data byvar
      d = d[order(d[,byvar],d[,xvar]),]
    }
    else
    {
      # sort data byvar, lvar
      d = d[order(d[,byvar],-d[,lvar],d[,xvar]),]
    }
 
    # loop over byvar
    byvals = unique(d[,byvar])
    for (ibyval in 1:length(byvals))
    {
      byval = byvals[ibyval]
      if (!(is.numeric(byval)))
      {byval = as.character(byval)}
      if (printnames==TRUE){
      print(byval)
      }
     
      # init device
      if (dev == "png") png(paste(path,file,"_",yvars[1],"_",byval,".png",sep=""),width=width,height=height,units=units,res=dpi)

      # individual pvals for each byval
      pvals = unique(d[d[,byvar] == byval,pvar])
      #print(length(pvals))
      op = par(mfrow=c(length(pvals),1))

      # loop pval
      for (ipval in 1:length(pvals))
      {
        pval = pvals[ipval]
        # Margins
        mar = marori
        if (!(ipval == 1)) mar[3]=1
        if (!(ipval == length(pvals))) mar[1]=1
        op = par(mar=mar)

      #->
      #

      # init litem
      ilitem = 0
      
      # axis range
      xlims = range(d[,xvar], na.rm=TRUE)
      ylims = range(d[,yvars], na.rm=TRUE)
      
      # xlab
      if (ylab == "auto")
      {
         if (xvar == "t" | xvar == "year") xlabtext = "" else xlabtext = xvar
      }
      else {xlabtext=xlab}
      # ylab
      if (ylab == "auto")
      {
         ylab=paste(yvars, collapse = ", ")
      }
      else {ylabtext=ylab;}
      # plot frame
      plot(xlims, ylims, type="n", 
         main=paste(plottitle, byval, paste("(",pvar," ", pval,")",sep="")), 
         ylab=ylabtext,xlab=xlabtext,
         ...)
      
      # loop over yvars
      for (yvar in yvars)
      {
        # if lvar
        if (lvar == "")
        {
          # legend item counter
          ilitem = ilitem + 1
          
          # svar
          type = typeori
          if (!(svar == ""))
          {
            sval = unique( d[d[,byvar] == byval & d[,pvar] == pval,svar] )[1]
            if (!(is.na(sval))) type = as.character(sval)
          }
          
          # plot line
          lines(d[d[,byvar] == byval & d[,pvar] == pval,xvar],
                d[d[,byvar] == byval & d[,pvar] == pval,yvar],
                type, lty, col=colorlist[ilitem], pch=pch)
          
          # if yvars
          if (length(yvars) == 1)
          {
            # no legend item
          }
          else
          {
            # yvar legend
            litem = as.character(yvar)
            # add to litems
            if (ilitem == 1) {litems = litem} 
            if (ilitem >1 )  { litems = c(litems,litem) }
          }
        } # endif no lvar
        if (!(lvar == ""))
        {
          # loop over uniqe(lvar)
          lvals = unique(d[,lvar])
          for (ilval in 1:length(lvals))
          {
            lval = lvals[ilval]
            
            # legend item counter
            ilitem = ilitem +1
            
            # svar
            if (!(svar == ""))
            {
              sval = unique( d[d[,byvar] == byval & d[,pvar] == pval & d[,lvar] == lval,svar] )[1]
              if (!(is.na(sval))) type = as.character(sval)
            }

            # plot line
            lines(d[d[,byvar] == byval & d[,pvar] == pval & d[,lvar] == lval,xvar],
                  d[d[,byvar] == byval & d[,pvar] == pval & d[,lvar] == lval,yvar],
                  type, lty, col=colorlist[ilitem],pch=pch)
            
            # if yvars
            if (length(yvars) == 1)
            {
              # lval legend item
              litem = as.character(lval)
              # add to litems
              if (ilitem == 1) {litems = litem} 
              if (ilitem >1 )  { litems = c(litems,litem) }
            } # endif yvars
            else
            {
              # yvars lval legend item
              litem = paste(as.character(yvar), as.character(lval))
              # add to litems
              if (ilitem == 1) {litems = litem} 
              if (ilitem >1 )  { litems = c(litems,litem) }
            }
          } # end loop lvar
        } # endif lvar
      } # end loop yvars
      # create legend
      if (!lpos == "none")
      {
       if (paste(ltitle,lvar) == " ")
       {
         legend(lpos, litems, col=colorlist[1:length(litems)], lty=1)
       }
       if (!(paste(ltitle,lvar) == " "))
       {
         legend(lpos, litems, title=paste(ltitle, lvar), col=colorlist[1:length(litems)], lty=1)
       }
      }

      } # end loop pvals
      #
      #->

      # init device
      if (dev == "png") dev.off()

    } # end loop byvar
    #
    #->

    } # end if panels

  } # end if byvar 
  op = par(mar = marori)
  op = par(mfrow = c(1,1))
} # end function      

# plotby(clim,"t",c("tt","ttmin"), xlim=c(2000,2005))
# plotby(clim,"t",c("tt","ttmin"), "mon",xlim=c(2000,2005), colorlist=1:30)
# plotby(clim_dep,"t",c("rrBD","rrTF"), "","site", xlim=c(2000,2005), type="l", colorlist=1:30)
# plotby(clim_dep,"t",c("rrBD","rrTF"), "mon","site", xlim=c(2000,2005), type="p", colorlist=1:30)
# plotby(clim_dep,"t",c("rrBD"), "site", xlim=c(2000,2005), type="p", colorlist=1:30)
# resf=cbind(resf,s="l")

# plotby(resf,"t","no3","scenario","site",pvar="layer",svar="s",file="test")

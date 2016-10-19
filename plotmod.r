########################################
# p - plot results of statistical model 
#
# Description
#  plots modelled versus measured values in an 1:1 plot
# Usage
#  mod=lm(y~x,data=d)
#  plotmod(mod)
# Arguments
#  mod  statistical model result, preferably from lm()
#  digits   number of significant digits to be displayed in model equation
#  lpos     legend position (c.f. legend)
#  lty      line type
#  pch      symbol for measured and modelled values. 
#  nplots   plots versus explanatory variables
#             NA: all plots
#             0:  no 
#             1:  only 1:1 plot 
#             >1: 1:1 plot + versus explanatory variables
#  cex.main font size of title 
plotmod = function(mod, digits=4, lpos="topleft",lty=1,pch=c(19,21), nplots=NA, main=NA, cex.main=0.8, ...) 
{
   # measured
   y=mod$model[,1]
   # modelled
   ymod=predict(mod)
   # 1:1 xlim, ylim
   ylim=c(min(y,ymod),max(y,ymod))
   # equation
   # response variable, e.g.  y = 
   equation=names(mod$model)[1]
   # intercept, e.g. y = 0
   equation=paste(equation," = ",format(mod$coefficients[1],digits=digits),sep="")  
   # explanatory variables
   for (i in 2:length(mod$coefficients)) {
     p.value=summary(mod)$coefficients[i,"Pr(>|t|)"]
     equation=paste(equation,
        # sign, e.g. y = 0 + 
        ifelse(sign(mod$coefficients[i])>-1," + "," - "),
        # value, e.g. y = 0 + 2.15
        format(abs(mod$coefficients[i]),digits=digits),
        # sign between value and name, e.g. y = 0 + 2.15
        " ",
        # name, e.g. y = 0 + 2.15 * x
        names(mod$coefficients[i]),
        # significance of term
        ifelse(p.value<0.05,"*",""),
        ifelse(p.value<0.01,"*",""),
        ifelse(p.value<0.001,"*",""),
        sep="")
   }   
   # model performance indicators
     # gatter values
   r.squared=summary(mod)$r.squared
   adj.r.squared=summary(mod)$adj.r.squared
   AIC=AIC(mod)
   BIC=BIC(mod)
   df.residual=mod$df.residual
   RSD=sd(mod$residuals)/mean(mod$fitted)
     # concatenate text
   performance=paste("(",sep="")
   performance=paste(performance,"R2=",format(r.squared,digits=2),sep="")
   performance=paste(performance," adjR2=",format(adj.r.squared,digits=2),sep="")
   performance=paste(performance," AIC=",format(AIC),sep="")
   performance=paste(performance," BIC=",format(BIC),sep="")
   performance=paste(performance," df=",format(df.residual),sep="")
   performance=paste(performance," RSD=",format(RSD),sep="")
   performance=paste(performance,")",sep="")

   
   # plot response variable versus explanatory   
   if (is.na(nplots)) {
      nxplots=length(mod$coefficients); 
   } else {
      nxplots=min(nplots,mod$coefficientc);
   }
   if (nxplots>1) {
     # loop over explanatory variables until nplots-1
     for (i in 2:nxplots) {
       # plot y~x
       x=mod$model[,i]
       plot(y~x,ylim=ylim,
         ylab=names(mod$model)[1],
         xlab=names(mod$model)[i],
         pch=20, cex.main=cex.main, ...)
       points(ymod~x,pch=21)
       legend(lpos,legend=c("measured","modelled"),lty=lty,pch=pch)
       # equation 
       mtext(equation,side=3,line=1,cex=cex.main, ...)
       # performance
       mtext(performance,side=3,line=0,cex=cex.main, ...)
       # plot title
       if(!is.na(main)) {mtext(main,side=3,line=2,cex=cex.main, ...);}
     }
   } # plot y~x plots
   
   # plot 1:1 y~y
   plot(ymod~y,xlim=ylim,ylim=ylim,
     ylab="modelled",xlab="measured",...)
    # equation
    mtext(equation,side=3,line=1,cex=cex.main, ...)
    # performance
    mtext(performance,side=3,line=0,cex=cex.main, ...)
    # plot title
    if(!is.na(main)) {mtext(main,side=3,line=2,cex=cex.main, ...);}
    
    # return model result text
    if (is.na(main)) main="" else main=paste(main," ",sep="")
    return(paste(main,equation," ",performance,sep=""))
}

# Read Results of Statistical Analyses
#  adj R2 = summary(mod)$adj.r.squared)
#  R2 = summary(mod)$r.squared)
#  summary(mod)$coefficients[,c("Estimate","Std. Error","t value","Pr(>|t|)")]
#   p.value =    summary(mod)$coefficients[i,"Pr(>|t|)"]

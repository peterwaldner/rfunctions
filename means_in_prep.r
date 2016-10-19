################################################################################
# means - Aggregate X-variables by groups of By-Variables
#
# Features
#   one to multi Variables
#   mean, min, max, n, 
# 
# Syntax
#   means(dataframe,byvars,meanvars,minvars,maxvars,sumvars,nvars)
# Arguments
#   dataframe	dataframe with the data to plot
#   byvars     names of variables defining the By-Groups
#   meanvars       unweighted means
#
#   sufix      auto: output variables will get sufix with function name, e.g. treeheight_min
#                    except functions mean and sum
#              fun:  output variables will get sufix name anyway
#   sep        characters between variable name and sufix 
#
# Example
#   d1 = means(d,byvars=c("plac","year"),meanvars=c("treeheight","dbh"),nvars="dbh",sufix=TRUE))
# 
# Known Issues
#   - date/time in byvars is converted to string and in vars to numeric


# Auxiliary FUN functions

  # number of non missing observations
  n.obs <- function(x, na.rm=TRUE)
  {
     if(na.rm==TRUE) x <- na.omit(x)
     if(class(x)=="data.frame") n=length(x[,1])
     if(class(x)=="numeric") n=length(x)
     if(!exists("n")) n=0
     return(n)
  }

  # coefficient of variance
  cv <- function(x, na.rm=TRUE) {
     return(sd(x,na.rm=na.rm)/mean(x,na.rm=TRUE))
  }
  # quantiles
  q25 <- function(x, na.rm=TRUE) { return(quantile(x,0.25,na.rm=na.rm)) }
  q75 <- function(x, na.rm=TRUE) { return(quantile(x,0.75,na.rm=na.rm)) }
  q10 <- function(x, na.rm=TRUE) { return(quantile(x,0.10,na.rm=na.rm)) }
  q90 <- function(x, na.rm=TRUE) { return(quantile(x,0.90,na.rm=na.rm)) }


# Main function
means <- function (d,byvars,
                   meanvars="",  meansufix="mean",
						 minvars="",   minsufix="min",  
						 maxvars="",   maxsufix="max",  
						 sumvars="",   sumsufix="sum",
						 nvars="",     nsufix="n",
						 cvvars="",    cvsufix="cv",
						 sdvars="",    sdsufix="sd",
						 medianvars="",mediansufix="median",
						 q25vars="",   q25sufix="q25",
						 q75vars="",   q75sufix="q75",
						 q10vars="",   q10sufix="q10",
						 q90vars="",   q90sufix="q90",
						 sufix="auto",sep="_",
                   na.rm=TRUE,...) 
{



   # Function aggreg
   aggreg <- function (d,d2,byvars,vars,fsufix="",FUN="mean",sufix="",sep="_",na.rm=TRUE,...)
   {
      
      if (max(nchar(vars))>0) 
      {
         if (length(byvars)==1) {
            d1 = aggregate(d[,vars],by=list(d[,byvars]),na.rm=na.rm,FUN=FUN, ...)
         }
         if (length(byvars)>1) {
            d1 = aggregate(d[,vars],by=as.list(d[,byvars]),na.rm=na.rm,FUN=FUN, ...)
         }
         if (sufix=="fun") {
            if (nchar(fsufix)==0) {
               names(d1) = c(byvars,vars)
            } else {
               names(d1) = c(byvars,paste(vars,fsufix,sep=sep))
            }
         }
         else {
            if (sufix=="auto") {
               if (fsufix=="mean" | fsufix=="sum") {
                  names(d1) = c(byvars,vars)
               } else {
                    if (nchar(fsufix)==0) {
                       names(d1) = c(byvars,vars)
                    } else {
                       names(d1) = c(byvars,paste(vars,fsufix,sep=sep))
                    }
               }               
            } else {
              if (sufix=="AUTO") {
                 if (fsufix=="mean" | fsufix=="sum") {
                    names(d1) = c(byvars,vars)
                 } else {
                    if (nchar(fsufix)==0) {
                       names(d1) = c(byvars,vars)
                    } else {
                       names(d1) = c(byvars,paste(vars,toupper(fsufix),sep=sep))
                    }
                 }               
              } else {
                 if (nchar(sufix)==0) {
                    names(d1) = c(byvars,vars)
                 } else {
                    names(d1) = c(byvars,paste(vars,sufix,sep=sep))
                 }               
              }
            }
         }
         d2 = merge(d2,d1,by=byvars,all=TRUE)
         rm(d1)
      }
      return(d2)
   }
   
   
   # check that byvars are not all NA
   for (i in 1:length(byvars))
   {
      byvar=byvars[i]
      byval = unique(d[,byvar])
      if (length(byval)>1 | !min(is.na(byval)))
      {
        if (i==1) {byvars2=byvar;} else {byvars2=c(byvars2,byvar);}
      }
   }
   byvars=byvars2;
   
   # Start with byvars
   d2 = unique(d[,byvars])
   
   # Aggregate aggretation types
   d2 = aggreg(d,d2,byvars=byvars,vars=meanvars,   fsufix=meansufix,   FUN="mean",   sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=minvars,    fsufix=minsufix,    FUN="min",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=maxvars,    fsufix=maxsufix,    FUN="max",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=sumvars,    fsufix=sumsufix,    FUN="sum",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=nvars,      fsufix=nsufix,      FUN="n.obs",  sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=cvvars,     fsufix=cvsufix,     FUN="cv",     sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=sdvars,     fsufix=sdsufix,     FUN="sd",     sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=medianvars, fsufix=mediansufix, FUN="median", sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=q25vars,    fsufix=q25sufix,    FUN="q25",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=q75vars,    fsufix=q75sufix,    FUN="q75",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=q10vars,    fsufix=q10sufix,    FUN="q10",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   d2 = aggreg(d,d2,byvars=byvars,vars=q90vars,    fsufix=q90sufix,    FUN="q90",    sufix=sufix,sep=sep,na.rm=na.rm, ...)
   
   return(d2)
}

# Would be nice
# - weighed means
# - shorter names e.g. by instead of byvars (however, consistent with plotby.r)
# - sufix default: sufix only if more than one function is used for a variable. 
# - do calculations only if necessary (re-structure if/then)
# - open list of arguments, use argument name as function if appropriate
#   c.f. R-lang.html#Argument-evalutation: match.call(expand.dots=FALSE) ...

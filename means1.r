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
#              functions: names of variables for wich:
#   meanvars   unweighted means
#   minvars    minimum
#   maxvars    maxium
#   sumvars    sum
#   sdvars     standard deviation
#   cvvars     coefficient of variance = sd/mean
#   medianvars median
#   q25vars    25%-quantile
#   q75vars    ..
#   q10vars    ..
#   q90vars    ..
#   nvars      number of non NA values
#   sufix      auto: output variables will get sufix with function name, e.g. treeheight_min
#                    except functions mean and sum
#              fun:  output variables will get sufix name anyway
#   sep        characters between variable name and sufix 
#
# Example
#   d1 = means(d,byvars=c("plac","year"),meanvars=c("treeheight","dbh"),nvars="dbh",sufix=TRUE))
#
# Example using v()
# 
#   d1 = means(d,byvars=c("plac year"),meanvars=c("treeheight dbh"),nvars="dbh",sufix=TRUE))


# Auxiliary FUN functions

  # number of non missing observations
  nobs2 <- function(x, na.rm=TRUE)
  {
     if (is.data.frame(x)) {
       if (na.rm==TRUE) {
         n=colSums(!is.na(x)); 
       }
       if (na.rm==FALSE) {
         n=rep(length(x[,1]),length(x[1,]));
       }
     }
     else {
       if (na.rm==TRUE) {
         n=sum(!is.na(x));
       }
       if (na.rm==FALSE) {
         n=length(x);
       }
     }
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
means <- function (d,byvars,meanvars="",minvars="",maxvars="",sumvars="",nvars="",cvvars="", sdvars="",
                   medianvars="", q25vars="", q75vars="", q10vars="", q90vars="",
                   sufix="auto",sep="_",
                   na.rm=TRUE,stringsAsFactors=FALSE,...) 
{

   # number of non missing observations
   nobs2 <- function(x, na.rm=TRUE)
   {
     if (is.data.frame(x)) {
       if (na.rm==TRUE) {
         n=colSums(!is.na(x)); 
       }
       if (na.rm==FALSE) {
         n=rep(length(x[,1]),length(x[1,]));
       }
     }
     else {
       if (na.rm==TRUE) {
         n=sum(!is.na(x));
       }
       if (na.rm==FALSE) {
         n=length(x);
       }
     }
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

   # defactor
   defactor = function(d,f="as.character",stringsAsFactors=FALSE)
   {
     vars=names(d)
     for (var in vars)
     {
        if (class(d[,var])=="factor")
        {
           if (f=="as.character")
           {
              d[,var]=as.character(d[,var])
           }
        }
     }
     return(data.frame(d,stringsAsFactors=stringsAsFactors));
   }



   # Function aggreg
   aggreg <- function (d,byvars,vars,sufix="auto",sep="_",na.rm=TRUE,fsufix="", FUN="mean",...)
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
            names(d1) = c(byvars,paste(vars,fsufix,sep=sep))
         }
         else {
            if (sufix=="auto") {
               if (fsufix=="mean" | fsufix=="sum") {
                  names(d1) = c(byvars,vars)
               } else {
                  names(d1) = c(byvars,paste(vars,fsufix,sep=sep))
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
      else 
      {
         d1 = data.frame(unique(d[,byvars]),stringsAsFactors=stringsAsFactors)
         names(d1) = c(byvars)
      }
      d1=defactor(d1,stringsAsFactors=stringsAsFactors)
      return(d1)
   }
   
   # Aggregate
   d_mean = aggreg(d,byvars,meanvars,sufix=sufix, sep=sep,na.rm=na.rm, fsufix="mean",FUN="mean", ...)
   d_min  = aggreg(d,byvars,minvars, sufix=sufix, sep=sep,na.rm=na.rm, fsufix="min", FUN="min",   ...)
   d_max  = aggreg(d,byvars,maxvars, sufix=sufix, sep=sep,na.rm=na.rm, fsufix="max", FUN="max",   ...)
   d_sum  = aggreg(d,byvars,sumvars, sufix=sufix, sep=sep,na.rm=na.rm, fsufix="sum", FUN="sum",   ...)
   d_n    = aggreg(d,byvars,nvars, sufix=sufix, sep=sep,na.rm=na.rm, fsufix="n",   FUN="nobs2",   ...)
   d_cv   = aggreg(d,byvars,cvvars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="cv",  FUN="cv",   ...)
   d_sd   = aggreg(d,byvars,sdvars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="sd",  FUN="sd",   ...)
   d_median= aggreg(d,byvars,medianvars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="median",  FUN="median",   ...)
   d_q25   = aggreg(d,byvars,q25vars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="q25",  FUN="q25",   ...)
   d_q75   = aggreg(d,byvars,q75vars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="q75",  FUN="q75",   ...)
   d_q10   = aggreg(d,byvars,q10vars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="q10",  FUN="q10",   ...)
   d_q90   = aggreg(d,byvars,q90vars,  sufix=sufix, sep=sep,na.rm=na.rm, fsufix="q90",  FUN="q90",   ...)
 
   # Merge results
   d1 = merge(d_mean,d_min,by=byvars,all=TRUE)
   d1 = merge(d1,d_max,by=byvars,all=TRUE)
   d1 = merge(d1,d_sum,by=byvars,all=TRUE)
   d1 = merge(d1,d_n,  by=byvars,all=TRUE)
   d1 = merge(d1,d_cv, by=byvars,all=TRUE)
   d1 = merge(d1,d_sd, by=byvars,all=TRUE)
   d1 = merge(d1,d_median, by=byvars,all=TRUE)
   d1 = merge(d1,d_q25, by=byvars,all=TRUE)
   d1 = merge(d1,d_q75, by=byvars,all=TRUE)
   d1 = merge(d1,d_q10, by=byvars,all=TRUE)
   d1 = merge(d1,d_q90, by=byvars,all=TRUE)
    
   return(d1)
}

# Would be nice
# - weighed means
# - shorter names e.g. by instead of byvars (however, consistent with plotby.r)
# - sufix default: sufix only if more than one function is used for a variable. 
# - do calculations only if necessary (re-structure if/then)
# - open list of arguments, use argument name as function if appropriate
#   c.f. R-lang.html#Argument-evalutation: match.call(expand.dots=FALSE) ...

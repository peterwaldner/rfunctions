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
#   sufix      TRUE: output variables will get sufix with function name, e.g. treeheight_mean
#   sep        characters between variable name and sufix 
#
# Example
#   d1 = means(d,byvars=c("plac","year"),meanvars="treeheight",sufix=FALSE))
# 

# NOT YET WORKING: problem is the unpacking of the variable lists from the arguments ...

means <- function (d,byvars,sufix=TRUE,sep="_",
                   na.rm=TRUE,...) 
{
    # arguments in ... (variable name list with name of a function)
    args = match.call(expand.dots=FALSE)
      return(args)

    # additional functions
    # number of non missing observations
    n <- function(x, na.rm=TRUE)
    {
 	   if(na.rm==TRUE) x <- na.omit(x)
 	   if(class(x)=="data.frame") length(x[,1])
       else if(class(x)=="numeric") length(x)
    }
 
    # loop over arguments (assuming them being existing functions
    for (i in 1:length(args$...)) {
      fun=names(args$...[i]);
      vars=as.character(args$...[[i]]); 
    
      # function
      if (max(nchar(vars))>0) 
      {
        print(vars)
        # aggregate
        d1 = aggregate(d[,vars],by=as.list(d[,byvars]),na.rm=na.rm,FUN=fun,...)
        if (sufix) {
           names(d1) = c(byvars,paste(vars,fun,sep=sep))
        }
        else {
           names(d1) = c(byvars,vars)
        }
        
        # merge
        if (calculations==0) {
           d2=d1;
           calculations=1;
        } else {
           d2 = merge(d2,d1,by=byvars,all=TRUE)
        }
      } # end function
    }
    # return(d2)
}


# Would be nice
# - weighed means
# - sufix default: sufix only if more than one function is used for a variable. 
# - do calculations only if necessary (re-structure if/then)
# - open list of arguments, use argument name as function if appropriate
#   c.f. R-lang.html#Argument-evalutation: match.call(expand.dots=FALSE) ...

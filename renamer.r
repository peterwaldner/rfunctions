############################################################
# sufixer: values below each other are arranged beside each other
# Details
# - grouping:  rows with same values in the byvars-variable are merged into one row
# - columns:   columns that are rearranged are the sfelder-variables (the other columns are skipped)
# - new column names: names of the sfelder-columns are replaced with the value of the sid-variable
# Arguments
#	d 		      input dataset
#	byvars      variables used to identify an output dataset 
#	sid    		variable text with the sufix (text variable)
#	sfelder		variables that should receive the sufix
#  id          variables assumed to be the same for whole group (only one value is taken)
#  sep         Text between variable name and sufix

renamer <- function(d,byvars,sid,sfelder,idvars=c(),ssort=c(),sep="_") 
{
   # list of sufixes
   if (length(ssort) == 0) {
       # normal sorting of sufixes
      sidvals = unique(d[,sid])
      sidvals = sidvals[order(sidvals)]
   } else {
      # sorting sufix according to value in ssort variable   
      sidvals = unique(d[,c(sid,ssort)])
      sidvals = sidvals[order(sidvals[,ssort]),sid]
   }
   
   
   # calculations
   calculations=0

   # loop over sufixes
   for (sidval in sidvals) 
   {  
      if (length(idvars)>0 & calculations == 0) {
         d1 = d[d[,sid]==sidval,c(byvars,idvars,sfelder)];
         names(d1) = c(byvars,idvars,paste(sidval,sep=sep));
      } else {
         d1 = d[d[,sid]==sidval,c(byvars,sfelder)];
         names(d1) = c(byvars,paste(sidval,sep=sep));
      }
      if (calculations==0) {
         d2=d1
         calculations=1
      } else {
         d2=merge(d2,d1,by=byvars,all=TRUE)
      }
   }
   
   # data/no data
   if (calculations > 0) {
      return(d2)
   } else {
      return(d)
   }
}

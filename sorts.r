# sorts - function to sort rows in a data-frame by variables with names byvars
# syntax
#  d=sorts(d,byvars)
# arguments
#  d        data-frame
#  byvars   names of variables in data-frame by which it should be sorted
# example
#  dem=sorts(dem,c("CODE_COUNTRY","PARTNER_CODE","CODE_PLOT","CODE_SAMPLER","SURVEY_YEAR","PERIOD_NUMBER"))

sorts <- function(d,byvars) 
{
   lby = length(byvars); 
   if (lby > 6) { cat("Warning only first 6 byvars were used for sorting");}
   if (lby==1) o=order(d[,byvars[1]]);
   if (lby==2) o=order(d[,byvars[1]],d[,byvars[2]]);
   if (lby==3) o=order(d[,byvars[1]],d[,byvars[2]],d[,byvars[3]]);
   if (lby==4) o=order(d[,byvars[1]],d[,byvars[2]],d[,byvars[3]],d[,byvars[4]]);
   if (lby==5) o=order(d[,byvars[1]],d[,byvars[2]],d[,byvars[3]],d[,byvars[4]],d[,byvars[5]]);
   if (lby>5)  o=order(d[,byvars[1]],d[,byvars[2]],d[,byvars[3]],d[,byvars[4]],d[,byvars[5]],d[,byvars[6]]);
   d1 = d[o,];
   return(d1)
}

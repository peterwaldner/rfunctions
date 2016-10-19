#######################################################
# keep - keep only certain variables
# Arguments
#  d       input dataframe
#  vars    array with name of variables
#  stringsAsFactors=FALSE

keep = function(d,vars,stringsAsFactors=FALSE)
{
     # warning
     vars0=names(d)
     i1=0
     for (i in 1:length(vars))
     {
        ifound=0
        for (j in 1:length(vars0)) {
           if (vars[i]==vars0[j]) {
              if (i1==0) {
                 vars1=vars[i];
              } else {
                 vars1=c(vars1,vars[i]); 
              }
              ifound=1
              i1=i1+1;
           }
        }
        if (ifound==0) {
           print(paste("Warning (keep.r): Variable '",vars[i],"' not found and not kept.",sep=""))
        }
     }
     
     # keep
     d1=subset(d,,vars1)
     return(d1)
}




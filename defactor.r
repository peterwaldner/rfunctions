#######################################################
# defactor - converts Factors into characters
# Arguments
#  d       input dataframe
#  f       function
#  stringsAsFactors=FALSE

defactor = function(d,f="as.character",stringsAsFactors=FALSE)
{
     # vars
     vars=names(d)
     
     # loop over vars
     for (var in vars)
     {
        if (class(d[,var])[1]=="factor")
        {
           if (f=="as.character")
           {
              d[,var]=as.character(d[,var])
           }
        }
     }
     return(data.frame(d,stringsAsFactors=stringsAsFactors));
}



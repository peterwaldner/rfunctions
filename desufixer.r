#######################################################
# desufixer: values in columns are arranged into lines
# Details
# - grouping:  rows with same values in the byvars-variable are merged into one row
# - columns:   columns that are rearranged have combination <sufix><sep><vars> as names 
# - new lines: the column <sid> contains the <sufix>, 
# - new column: the columns <vars> the values
# Arguments
#	d 		      input dataset
#	byvars      variables used to identify an output dataset 
#	sid    		the name of the column with the values of the sufixes (text variable)
#  sufixes     a list of sufix, e.g. c("a","b")
#	vars   		a list of variables names after removel of sufixes 
#  idvars      variables assumed to be the same for whole group (only one value is taken)
#  sep         Text between sufix and variable name 

desufixer = function(d,byvars,sufixes,vars,idvars=c(""),sid="VARNAME",sep="_",stringsAsFactors=FALSE)
{
     # keys and others
     isufixes=0
     d0=subset(d[,c(byvars,idvars)]);
     for (sufix1 in sufixes)
     {
        isufixes=isufixes+1;
        d1=cbind(d0,
            SID=sufix1,
            stringsAsFactors=stringsAsFactors); 
        names(d1)[length(names(d1))]=sid;
        
        for (var in vars)
        {
          d2=d[,paste(var,sufix1,sep=sep)]; 
          d1=cbind(d1,d2,
            stringsAsFactors=stringsAsFactors); 
          names(d1)[length(names(d1))]=var;
        }
        
        # collect
        if (isufixes==1) {d3=d1;} else {d3=rbind(d3,d1,
            stringsAsFactors=stringsAsFactors);}
     }
     return(data.frame(d3,stringsAsFactors=stringsAsFactors));
}



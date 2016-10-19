############################################################
# sufix: add a sufix to a series of strings
# Arguments
#	vars	series of strings 
#	sufix	string       variables used to identify an output dataset 
#	sep	Text between variable name and sufix

sufix <- function(vars,sufix,sep="") 
{
  return(paste(vars,sufix,sep=sep))
}

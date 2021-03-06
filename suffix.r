############################################################
# suffix: add a suffix to a series of strings
# Arguments
#	vars  	series of strings 
#	suffix	string       variables used to identify an output dataset 
#	sep   	Text between variable name and suffix

suffix <- function(vars,suffix,sep="") 
{
  return(paste(vars,suffix,sep=sep))
}

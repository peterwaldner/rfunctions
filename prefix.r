############################################################
# prefix: add a prefix to a series of strings
# Arguments
#	vars  	series of strings 
#	prefix	string       variables used to identify an output dataset 
#	sep   	Text between variable name and prefix

prefix <- function(prefix,vars,sep="") 
{
  return(paste(prefix,vars,sep=sep))
}

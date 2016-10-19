########################################
# merges - Merge two dataframes by columns with the same name
#
# Description
#  Merges is usefull to add a second dataset below a first one, 
#  if several, but not all columns are common. 
# 
# Usage
#  d = merges(d1,d2)
#
# Example
#  d1=data.frame(a=..., b=..., c=...)
#  d2=data.frame(a=..., b=..., d=...)
#  d = merge(d1,d2,by=c("a","b"),all=TRUE)
# 
merges=function(x,y,all=TRUE,...) 
{
     xvars=data.frame(var=names(x),i=1:length(names(x)),stringsAsFactors=FALSE)
     yvars=data.frame(var=names(y),stringsAsFactors=FALSE)
     vars=sorts(merge(xvars,yvars,all=FALSE,stringsAsFactors=FALSE),by="i")$var
	  return(merge(x,y,by=vars,all=all, ...))
}

########################################
# rename - SAS like renaming of variables in a dataframe
#
# Usage
#  d2 = rename(d,"a=b c=d")
#
# To Do
#  - warning if old name is not existing
#  - warning if new name already exists, overwrite options
rename <- function(d,renametext) 
{
   vars=names(d)
   renamepairs=strsplit(renametext," ")[[1]]
   for (renamepair in renamepairs) 
   {
      renamevars=strsplit(renamepair,"=")[[1]]
      # loop over variables
      for (i in 1:length(vars)) 
      {
          if (vars[i]==renamevars[1]) { vars[i]=renamevars[2]; }
      }
   }
   names(d)=vars
   return(d)
}

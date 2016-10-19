########################################
# v - variable name list without "," 
#
# Description
#  list is splitted at blanks from v("a b c") to c("a","b","c")
# Usage
#  vars = v("a b c")
# Todo
#  abfangen v("a","b","c")
#
v = function(text, ...) 
{
   
   v=strsplit(text," ")[[1]]
}

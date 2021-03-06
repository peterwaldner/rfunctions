################################################################
#                            R-Function, 15.1.2015 Peter Waldner
# subsets - subset of dataframe d with same values as dataframe a in columns with same names
# ---------------------------------
# Syntax d1=subsets(d,a)
# Arguments
#    d   dataframe having at least all columns of dataframe a
#    a   dataframe with one row and selected columns of d

subsets=function(d,a) {
  # columns
  avars=names(a)
  # loops rows
  for (j in 1:length(a[,1])) {
    # loops columns
    for (i in 1:length(avars)) {
      avar=avars[i]
      if (i==1) {
        seli = d[,avar]==a[j,avar]
      } else {
        seli = seli & d[,avar]==a[j,avar]
      } 
    }   
    if (j==1) {
      selj = seli
    } else {
      selj = selj | seli
    }
  }     
  return(d[selj,])
}


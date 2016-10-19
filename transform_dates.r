############################################################
# transform_dates: transforms columns with "DATE" in name to class date
# Arguments
#	d        dataframe
#	format   format used by as.Date
#	pattern  pattern in the column names of columns to be transformed

transform_dates <- function(d,format="%d.%m.%Y",pattern="DATE") 
{
  datevars = names(d)[grep(pattern,names(d))]
  for (var in datevars)
  {
    d[,var]=as.Date(d[,var],format)
  }
  return (d)
}

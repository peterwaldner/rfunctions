#                                          R-Function, 27.7.2012, Peter Waldner
# read.crd - Function to read CR800 and CR1000 Datalogger files 
#            with TOA5 Format conform with CRD Format
#            
# Explanation
# - TOA5 Format:   Is an option in LoggerNet > Setup > {Logger} > Schedule
# - CRD Format:    Is a convention of the LWF Database
#                  http://wiki.lwf.wsl.ch/tiki-index.php?page_ref_id=122

read.crd <- function(file,...) 
{
   # read file header 
     # 1st line: header (skipped)
     # 2nd line: column names (names of head)
     # 3rd line: units
     # 4th line: type
   head = read.table(file,sep=",",skip=1,header=TRUE,nrow=2,as.is=TRUE)
   # read data
   data = read.table(file,sep=",",skip=4,header=FALSE,as.is=TRUE,...)
   # column names of data
   names(data)=names(head)
   
   # CRD Elements
   statnr = data[,3]
   mprojnr= data[,4]
   pdauer = data[,5]

   # return data only
   return(data)
}



write.crd <- function(data,file,units="",types="",...)
{
   # 1st line
   write.table("TOA5",file,append=FALSE,row.names=FALSE,col.names=FALSE,sep=",")   
   # 2nd line
   write.table(t(names(data)),file,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
   # 3rd line
	write.table(units,file,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")   
   # 4th line
	write.table(types,file,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")   
   # data
	write.table(data,file,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")   
}


# Example
# smex=read.crd("O:/Meteo/MA/VOR_SMEX_Table10.crd")
# smex2=cbind(smex[,1:4],PDAUER=10,smex[,5:length(smex[1,])])
# write.crd(smex2,"O:/Meteo/MA/VOR_SMEX_Table10_TEST.crd")

####################################################
# Include all R-Functions in LWF Auswertungen
# Usage
#  source(paste(rpath,"rfunctions.r",sep=""))
# Directory with R-Functions
# rpath="O:/LWF/Auswertungen/RFunctions/";

rfunctions=c("means","sorts","n.obs","plotby",
   "prefix","sufix","suffix","rename",
   "sufixer","prefixer",
   "renamer","sign1","rkt",
   "transform_dates","subsets","desufixer","defactor",
   "merges","v","p",
   "plotmod","keep","plotch","histby");

# Auxiliary Functions   
for (rfunction in rfunctions)
{
 source(paste(rpath,rfunction,".r",sep=""))
}

# Database Connection
# source(paste(rpath,"database_connection,".r",sep=""))



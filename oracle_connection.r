#####################################################
# Peter Waldner, 4.3.2015, 29.3.2016
#
# Database Connection To ORACLE Database from R
# using /home/r/oracle.r
#
#####################################################

# Oracle Database Administrator
  # ask DB Administrator for an Account to get an Oracle-Username and Oracle-Passwort
  # ask DB Administrator for server name, UDP/TCP port number and DB name 

# Windows
  # install Oracle ODBC driver on computer (only once)
      # a) ask IT to install the ORACLE ODBC drivers (Padruot Nogler)
      # b) http://webintra.wsl.ch/lfi/doku/htdocs/dab/oraclientrodbc.php (R. Meile)
  # define an ODBC data source 'klaros' using DB server name and port number (only once)
      # Start > Settings > Administrative Tools > ODBC > SYSTEM DSN
  # store Oracle-Username and Oracle-Password in protected file if using variante c
      # copy O:\LWF\_Infomanagement\Tipps\R\oracle_vorlage.r into H:\r\oracle.r 
      #      and change username and password
  # connect
if (.Platform$OS.type=="windows")
{  

  # install.packages("RODBC")
    require("RODBC")
    
  # load package
    library(RODBC) 
    
  # connect to oracle DB using Oracle-Username and Oracle-Password
    # c) R Studio, R Console: stored username and password in protected file
    source("H:/r/oracle.r")
    conn = odbcConnect("klaros",uid=orauser, pwd=orapwd)    
    
  # compatibility with RJDBC used for Macintosh 
    dbGetQuery = function (...) {sqlQuery(...)}
    dbDisconnect = function(...) {odbcClose(...)}
    sql = function (...) {d=sqlQuery(conn,...);names(d)=tolower(names(d)); return(d);}
}
  
# Macintosh
  # install driver
      # download jdbc-driver "ojdbc14.jar" and copy it into /Applications (only once)
  # store Oracle-Username and Oracle-Password in protected file if using variante c
      # copy O:\LWF\_Infomanagement\Tipps\R\oracle_vorlage.r into /home/oracle.r 
      #      and change username and password
      
  # connect  
if (.Platform$OS.type!="windows")
{
    # install.packages("RJDBC")
    require("RJDBC")
    
    # load package
    load(RJDBC)

    # connection settings
    drv <- JDBC("oracle.jdbc.OracleDriver","/Applications/ojdbc14.jar",identifier.quote="`")

    # conncet to oracle DB using connection string (server name and port number and DB name), Oracle-username, Oracle-Passwort
    # keep only prefered variante 
    # a) store username and password in this file
    conn <- dbConnect(drv, "jdbc:oracle:thin:@klaros:1521:kla01", "username", password="password")
    # b) store username and password in protected file 
    source("/home/r/oracle.r")
    conn <- dbConnect(drv, "jdbc:oracle:thin:@klaros:1521:kla01", orauser, password=orapwd)

    # compatitibilty with RODBC for Windows
    sqlQuery = function (...) {dbGetQuery(...)}
    odbcClose = function(...) {dbDisconnect(...)}
    sql = function (...) {d=dbGetQuery(conn,...);names(d)=tolower(names(d)); return(d);}
}

# Example to test and use database connection

   # query data from database
   data=dbGetQuery(conn,"select * from dual")

# Disconnect from DB

  # disconnect connection
  # dbDisconnect(conn)

  
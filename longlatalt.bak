#                                        R-Script, Peter Waldner, 17.4.2015
# longlatalt - add WGS 1984 coordinates and altitude (m a.s.l.) 
#    Variables: LONGITUDE, LATITUDE, CODE_ALTITUDE -> X, Y, Z
# syntax
#   d = longlatalt(d)
longlatalt=function(d)
{
   # X: Longitude in degrees;
   X=sign(longitude)*
        (
          (floor(abs(longitude)/10000))
         +(floor(abs(longitude)/100)
           -(floor(abs(longitude)/10000))*100)/60
         +(floor(abs(longitude))
           -(floor(abs(longitude)/10000))*10000
             -(floor(abs(longitude)/100)
           -(floor(abs(longitude)/10000))*100)*100)/60/60
        );
   # Y: Latitude in degrees;
   Y=sign(latitude)*
        (
          (floor(abs(latitude)/10000))
         +(floor(abs(latitude)/100)
           -(floor(abs(latitude)/10000))*100)/60
         +(floor(abs(latitude))
           -(floor(abs(latitude)/10000))*10000
             -(floor(abs(latitude)/100)
           -(floor(abs(latitude)/10000))*100)*100)/60/60
        );
   # Z: Altitude (estimate) in meters;
   Z=(code_altitude-1)*50;
}
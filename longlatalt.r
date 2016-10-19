#                                        R-Script, Peter Waldner, 17.4.2015
# longlatalt - add WGS 1984 coordinates and altitude (m a.s.l.) 
#    Variables: LONGITUDE, LATITUDE, CODE_ALTITUDE -> X, Y, Z
# syntax
#   d = longlatalt(d)
longlatalt=function(d)
{
   d1=transform(d,
    # X: LONGITUDE in degrees;
    X=sign(LONGITUDE)*
        (
          (floor(abs(LONGITUDE)/10000))
         +(floor(abs(LONGITUDE)/100)
           -(floor(abs(LONGITUDE)/10000))*100)/60
         +(floor(abs(LONGITUDE))
           -(floor(abs(LONGITUDE)/10000))*10000
             -(floor(abs(LONGITUDE)/100)
           -(floor(abs(LONGITUDE)/10000))*100)*100)/60/60
        ),
    # Y: LATITUDE in degrees;
    Y=sign(LATITUDE)*
        (
          (floor(abs(LATITUDE)/10000))
         +(floor(abs(LATITUDE)/100)
           -(floor(abs(LATITUDE)/10000))*100)/60
         +(floor(abs(LATITUDE))
           -(floor(abs(LATITUDE)/10000))*10000
             -(floor(abs(LATITUDE)/100)
           -(floor(abs(LATITUDE)/10000))*100)*100)/60/60
        ),
    # Z: Altitude (estimate) in meters;
    Z=(CODE_ALTITUDE-1)*50
   )
}

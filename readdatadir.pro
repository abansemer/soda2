function readdatadir,lun, sun=sun
   ;Copyright © 2016 University Corporation for Atmospheric Research (UCAR). All rights reserved.
   
   datadir={tagNumber:0us, dataOffset:0us, numberBytes:0us, samples:0us, $ 
   bytesPerSample:0us,type:0b,parameter1:0b,parameter2:0b,parameter3:0b,address:0us}
   
   readu,lun,datadir
   ;IF sun THEN datadir=swap_endian(datadir)
   return,datadir
end

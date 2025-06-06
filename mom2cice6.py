#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import numpy.ma as ma
import array
import glob
import struct
import hashlib
import datetime
import time
import sys
import os



def checksum(filename: str, blocksize: int = 4096) -> str:
    hsh = hashlib.md5()
    with open(filename, "rb") as f:
        while True:
            buf = f.read(blocksize)
            if not buf:
                break
            hsh.update(buf)
    return hsh.hexdigest()



mapl_tripole = sys.argv[1]
topo_file = sys.argv[2]

grid_out = 'cice6_grid.nc'
kmt_out = 'cice6_kmt.nc'
bathy_out = 'cice6_global.bathy.nc'



cm1 = checksum(mapl_tripole)
cm2 = checksum(topo_file)


ncfilein = Dataset(topo_file, 'r', format='NETCDF4')
depth  = ncfilein.variables['depth'][:]
#ncfile.close()
for dimo in ncfilein.dimensions.values():
   print(type(dimo))
ncfilein.close()

ncfile = Dataset(mapl_tripole, 'r', format='NETCDF4')
tlat  = ncfile.variables['lat_centers'][:]
tlon  = ncfile.variables['lon_centers'][:]
ulat  = ncfile.variables['lat_corners'][:]
ulon  = ncfile.variables['lon_corners'][:]
htn   = ncfile.variables['htn'][:]
hte   = ncfile.variables['hte'][:]
hus   = ncfile.variables['hus'][:]
huw   = ncfile.variables['huw'][:]
angle = ncfile.variables['angleu'][:]
mask  = ncfile.variables['mask'][:]
ncfile.close()

ncfileout = Dataset(bathy_out, 'w', format='NETCDF4')
ni = ncfileout.createDimension("ni", depth.shape[1])
nj = ncfileout.createDimension("nj", depth.shape[0])
bathy = ncfileout.createVariable("Bathymetry", "f4",("nj", "ni",), fill_value=1.e30)
bathy.long_name = "ocean bathymetry for grounding scheme"
bathy.units = "m"
depth = ma.masked_where(mask < 0.5, depth)
bathy[:] = depth
bathy.mssing_value = 1.e30
lon = ncfileout.createVariable("TLON", "f4",("nj", "ni",), fill_value=1.e30)
lon.long_name = "T grid center longitude"
lon.units = "degrees_east"
lon[:] = tlon
lat = ncfileout.createVariable("TLAT", "f4",("nj", "ni",), fill_value=1.e30)
lat.long_name = "T grid center latitude"
lat.units = "degrees_north"
lat[:] = tlat

if '/' in topo_file:
   topo_file = topo_file.replace('/', '$')
ncfileout.setncattr_string(topo_file, cm2)

#bathy._FillValue = 1.e30
ncfileout.close()



ncfileout = Dataset(kmt_out, 'w', format='NETCDF4')
x = ncfileout.createDimension("x", depth.shape[1])
y = ncfileout.createDimension("y", depth.shape[0])
kmt = ncfileout.createVariable("kmt", "f4",("y", "x",))
kmt.units = "m"
mask[mask > 0.5] = 1000.0
kmt[:] = mask
ncfileout.setncattr_string(topo_file, cm2)
ncfileout.close()



def create_var(ncf, name, typ, dim, unit, val, scale_fac):
   fld = ncf.createVariable(name, typ, dim)
   fld.units = unit
   fld[:] = val*scale_fac

ncfileout = Dataset(grid_out, 'w', format='NETCDF4')
x = ncfileout.createDimension("x", depth.shape[1])
y = ncfileout.createDimension("y", depth.shape[0])
create_var(ncfileout, "ulat", "f8", ("y", "x",), "rad", ulat[1:,1:], np.pi/180.0)
create_var(ncfileout, "ulon", "f8", ("y", "x",), "rad", ulon[1:,1:], np.pi/180.0)
create_var(ncfileout, "htn",  "f8", ("y", "x",), "cm", htn, 100.0)
create_var(ncfileout, "hte",  "f8", ("y", "x",), "cm", hte, 100.0)
create_var(ncfileout, "hus",  "f8", ("y", "x",), "cm", hus, 100.0)
create_var(ncfileout, "huw",  "f8", ("y", "x",), "cm", huw, 100.0)
create_var(ncfileout, "angle",  "f8", ("y", "x",), "rad", angle, np.pi/180.0)

ncfileout.setncattr_string(mapl_tripole, cm1)
ncfileout.setncattr_string(topo_file, cm2)

ncfileout.close()





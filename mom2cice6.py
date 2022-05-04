#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import array
import glob
import struct
import datetime
import time
import sys
import os
sys.path.append('/home/bzhao/python_utils')
#import read_utils
import data_utils
#import get_info



mapl_tripole = sys.argv[1]
topo_file = sys.argv[2]

grid_out = 'cice6_grid.nc'
kmt_out = 'cice6_kmt.nc'
bathy_out = 'cice6_global.bathy.nc'



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
ncfile = Dataset(topo_file, 'r', format='NETCDF4')
depth  = ncfile.variables['depth'][:]
ncfile.close()





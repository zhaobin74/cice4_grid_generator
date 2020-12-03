# cice grid generator for running GEOS-5 coupled model

- - -

## How to build

- load the following modules

   * comp/intel/19.1.2.254 
   * mpi/impi/19.1.2.254 

- make 

## Usage

There are 3 executables which are used for different MOM grid format

- gengrid

   * this version works with the old MOM4 grid spec
   * no longer used 

- gengrid_new

   * this version works with MOM5 grid spec
   * there are 2 main resolutions supported: **360x200**, **1440x1080**
   * to generate the cice grid, run the following command

       `./gengrid_new < input_nml_file`

     where `input_nml_file` has the following format:

```
&nml1 
   gridin          = 'grid_spec.nc'  !input mom grid spec file
 , gridout         = 'grid_cice-360x200.bin' ! output cice grid file 
 , kmtout          = 'kmt_cice-360x200.bin'  ! output cice kmt file 
 , gridin_format   = 'nc'            ! input file format, must be nc    
 , gridout_format  = 'bin'           ! output file format, binary
 , nx_global       = 360             ! grid x dimension
 , ny_global       = 200             ! grid y dimension
 , iob             = 100             ! output grid point i index, 1 <= i <= nx_global 
 , job             = 100             ! output grid point j index, 1 <= j <= ny_global
/
``` 

- gengrid_mom6

   * this version works with new MOM6 grid spec (Mosaic)
   * there are 2 main resolutions supported: **360x210**, **360x320**, more to come...
   * to generate the cice grid, run the following command

       `./gengrid_mom6 < input_nml_file`

     where `input_nml_file` has the following format:

```
&nml1 
   gridin          = 'ocean_hgrid.nc' ! input mosaic horizontal grid file 
   maskin          = 'ocean_mask.nc'  ! input mosaic mask file
 , gridout         = 'grid_cice.bin'  ! output cice grid file 
 , kmtout          = 'kmt_cice.bin'   ! output cice kmt file 
 , gridin_format   = 'nc'             ! input file format, must be nc    
 , gridout_format  = 'bin'            ! output file format, binary
 , nx_global       = 360              ! grid x dimension
 , ny_global       = 210              ! grid y dimension
 , iob             = 100              ! output grid point i index, 1 <= i <= nx_global 
 , job             = 100              ! output grid point j index, 1 <= j <= ny_global
/
``` 
 
 
 

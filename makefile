
# Suppresses display of executed commands
# .SILENT:

CC = gcc
CCFLAGS = 
#FC = ifort
FC = mpif90
FFLAGS = -O3

#INC_DIRS = -I/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/include/netcdf/
INC_DIRS = -I/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.6_build1/x86_64-unknown-linux-gnu/ifort_15.0.2.164-mpt_2.14/Linux/include/netcdf/
#INC_DIRS = -I/usr/local/other/SLES11/SIVO-PyD/1.10.0/include/netcdf

OBJS = gengrid.o 
OBJS1 = readgrid.o 
OBJS2 = gengrid_new.o 
#LIBS = 
#LIBS = -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib/ -lnetcdff -lnetcdf -lmfhdf -ldf -lsz -ljpeg -lhdf5_hl -lhdf5 -lcurl -lz -L/usr/local/other/SLES11.1/mvapich2/1.8.1/intel-13.1.2.183/lib -lmpich -lirc -ldl -lc -lpthread -lrt 
#LIBS=-L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -lnetcdff -lnetcdf -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -lcurl -lz -lrt -lm -lm -L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.2_build1/x86_64-unknown-linux-gnu/ifort_13.1.2.183-mvapich2_1.8.1/Linux/lib -lcurl -lz -lrt -lm -L/usr/local/intel/Composer/composer_xe_2013.1.117/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -L/usr/local/other/SLES11.1/mvapich2/1.8.1/intel-13.1.2.183/lib  -lmpich -lirc -ldl -lc -lpthread -lrt  -L/gpfsm/dnb32/mbhat/GCC/install/gcc-4.6.3/lib/gcc/x86_64-unknown-linux-gnu/4.6.3 -lstdc++
#LIBS = -L/usr/local/other/SLES11/SIVO-PyD/1.10.0/lib -lnetcdf
LIBS=-L/discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.6_build1/x86_64-unknown-linux-gnu/ifort_15.0.2.164-mpt_2.14/Linux/lib -lnetcdff -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -lsz -ljpeg -lgpfs -lcurl -lssl -lcrypto -ldl -lz -lrt -lm -L/usr/local/intel/Composer/composer_xe_2015.2.164/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -L/usr/local/sgi/mpi/mpt-2.14/opt/sgi/mpt/mpt-2.14/lib -lmpi -lmpi++ -lirc -ldl -lc -lpthread -lrt  -L/gpfsm/dnb32/mbhat/GCC/install/gcc-4.6.3/lib/gcc/x86_64-unknown-linux-gnu/4.6.3 -lstdc++
#LIBS = /home/bzhao/Libraries/lib/libnetcdff.a  

#---------------------------------------------------------
# rule to build executable: goyou
#

gengrid: $(OBJS)
	$(FC) $(FFLAGS) -o gengrid $(OBJS) $(LIBS)
readgrid: $(OBJS1)
	$(FC) $(FFLAGS) -o readgrid $(OBJS1) $(LIBS)
gengrid_new: $(OBJS2)
	$(FC) $(FFLAGS) -o gengrid_new $(OBJS2) $(LIBS)

#---------------------------------------------------------
# Default build rule
#

all:  gengrid_new gengrid readgrid

#---------------------------------------------------------
# remove generated files
#

# Rules to build .o files from their sources:
#---------------------------------------------------------
#
gengrid.o: gengrid.F90
	$(FC) $(FFLAGS) $(INC_DIRS)  -c $<
gengrid_new.o: gengrid_new.F90
	$(FC) $(FFLAGS) $(INC_DIRS)  -c $<
readgrid.o: readgrid.F90
	$(FC) $(FFLAGS) $(INC_DIRS)  -c $<


clean:
	rm *.o gengrid readgrid gengrid_new 


      program gengrid


      !use netcdf
 

      implicit none
      integer, parameter :: char_len  = 80, &
                            char_len_long  = 256, &
                            log_kind  = kind(.true.), &
                            int_kind  = selected_int_kind(6), &
                            real_kind = selected_real_kind(6), &
                            dbl_kind  = selected_real_kind(13), &
                            r16_kind  = selected_real_kind(26)
      real (kind=dbl_kind), parameter :: &
        spval_dbl = 1.0e30_dbl_kind    ! special value (double precision)
      real alon, C2RAD,  dlon1, deq, dsig, &
     &    phq, phs, PI, R2RAD, RAD, th0, th, &
     &    dphi_min, dphi_max, dphi_avg, dphi, dx
      integer i, j, nih1, njh1, njmax, jeq, jm, i_offset, i_th
      integer after, before, rate
      real delta_time, total_time, startup_time
      real southern_time, northern_time, metrics_time, shutdown_time
      real processing_time, combination_time
     
      character(len=char_len_long) :: &
           gridin, gridout, kmtout, gridin_format, gridout_format
      real(kind=dbl_kind) :: my_pi, rad_to_deg

      character(len=char_len_long) :: &
          varname
      integer (kind=int_kind) :: &
         my_status        ! status variable from netCDF routine 
      integer (kind=int_kind) ::  &
           fid_in, fid_out           ! unit number
      integer (kind=int_kind) :: &
           varid, nrec, nbits
      integer (kind=int_kind) :: &
           nx_global, ny_global, iob, job

      real (kind=dbl_kind) :: &
         amin, amax         ! min and max values of input array

      real(kind=dbl_kind), allocatable    :: ulat(:,:)
      real(kind=dbl_kind), allocatable    :: ulon(:,:)
      real(kind=dbl_kind), allocatable    :: htn(:,:)
      real(kind=dbl_kind), allocatable    :: hte(:,:)
      real(kind=dbl_kind), allocatable    :: hus(:,:)
      real(kind=dbl_kind), allocatable    :: huw(:,:)
      real(kind=dbl_kind), allocatable    :: kmt(:,:)
      integer(kind=int_kind), allocatable :: kmti(:,:)
      real(kind=dbl_kind), allocatable    :: sinrot(:,:)
      real(kind=dbl_kind), allocatable    :: cosrot(:,:)
      real(kind=dbl_kind), allocatable    :: angle(:,:)
!
!     Namelists
!     ---------
!
      namelist /nml1  / gridin, gridout, gridin_format, gridout_format, &
                        kmtout, nx_global, ny_global, iob, job   
!
      read(5,nml1)
      write(0,nml1)

      my_pi = atan(1.0)*4
      print*, 'pi = ', my_pi
      rad_to_deg    = 180._dbl_kind/my_pi  ! degree-radian conversion

      allocate(ulat(nx_global, ny_global), stat=my_status)
      allocate(ulon(nx_global, ny_global), stat=my_status)
      allocate(htn(nx_global, ny_global), stat=my_status)
      allocate(hte(nx_global, ny_global), stat=my_status)
      allocate(hus(nx_global, ny_global), stat=my_status)
      allocate(huw(nx_global, ny_global), stat=my_status)
      allocate(kmt(nx_global, ny_global), stat=my_status)
      allocate(kmti(nx_global, ny_global), stat=my_status)
      allocate(sinrot(nx_global, ny_global), stat=my_status)
      allocate(cosrot(nx_global, ny_global), stat=my_status)
      allocate(angle(nx_global, ny_global), stat=my_status)
 
      ulat = 0.0
      ulon = 0.0
      htn = 0.0
      hte = 0.0
      hus = 0.0
      huw = 0.0
      angle = 0.0
      kmti = 0 

      if(gridout_format == 'bin') then
         nbits = 64      
         open(fid_out,file=gridout,recl=nx_global*ny_global*nbits/8, &
                  form='unformatted',access='direct')          
         nrec = 1 
         read(fid_out,rec=nrec) ulat  
         amin = minval(ulat)
         amax = maxval(ulat, mask = ulat /= spval_dbl)
         print*, ' ice_read_global ', fid_out, nrec, amin, amax
         nrec = 2 
         read(fid_out,rec=nrec) ulon  
         nrec = 3 
         read(fid_out,rec=nrec) htn 
         nrec = 4 
         read(fid_out,rec=nrec) hte  
         nrec = 5 
         read(fid_out,rec=nrec) hus  
         nrec = 6 
         read(fid_out,rec=nrec) huw  
         nrec = 7 
         read(fid_out,rec=nrec) angle  
         close(fid_out) 
         nbits = 32      
         open(fid_out,file=kmtout,recl=nx_global*ny_global*nbits/8, &
                  form='unformatted',access='direct')          
            
         nrec = 1 
         read(fid_out,rec=nrec) kmti 
         close(fid_out) 
         print*, 'at ', iob, job
         print*, ulat(iob,job)*rad_to_deg, ulon(iob,job)*rad_to_deg
         print*, htn(iob,job),  hte(iob,job)
         print*, hus(iob,job),  huw(iob,job)
         print*, angle(iob,job), kmti(iob,job)
      else
         print*, 'output CICE grid must be in BINARY format'
         stop
      endif  
!     write (0,*) w
!     write (0,*) ' '
      deallocate(ulat)
      deallocate(ulon)
      deallocate(htn)
      deallocate(hte)
      deallocate(hus)
      deallocate(huw)
      deallocate(kmt)
      deallocate(kmti)
      deallocate(sinrot)
      deallocate(cosrot)
      deallocate(angle)

      end




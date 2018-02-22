
      program gengrid


      use netcdf
 

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
      integer, parameter :: vertex  = 4

      real alon, C2RAD,  dlon1, deq, dsig, &
     &    phq, phs, PI, R2RAD, RAD, th0, th, &
     &    dphi_min, dphi_max, dphi_avg, dphi, dx
      integer i, j, nih1, njh1, njmax, jeq, jm, i_offset, i_th
      integer after, before, rate
      real delta_time, total_time, startup_time
      real southern_time, northern_time, metrics_time, shutdown_time
      real processing_time, combination_time
     
      character(len=char_len_long) :: &
           gridin, gridout, vgridout, kmtout, gridin_format, gridout_format
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
          nn, nm,  nx_global, ny_global, iob, job
      integer*8               :: recl_length, nbytes 
      real(kind=dbl_kind), allocatable    :: ulat(:,:,:)
      real(kind=dbl_kind), allocatable    :: ulon(:,:,:)
      !real(kind=dbl_kind), allocatable    :: ulat(:,:)
      !real(kind=dbl_kind), allocatable    :: ulon(:,:)
      real(kind=dbl_kind), allocatable    :: htn(:,:)
      real(kind=dbl_kind), allocatable    :: hte(:,:)
      real(kind=dbl_kind), allocatable    :: hus(:,:)
      real(kind=dbl_kind), allocatable    :: huw(:,:)
      real(kind=dbl_kind), allocatable    :: kmt(:,:)
      integer(kind=int_kind), allocatable :: kmti(:,:)
      real(kind=dbl_kind), allocatable    :: sinrot(:,:)
      real(kind=dbl_kind), allocatable    :: cosrot(:,:)
      real(kind=dbl_kind), allocatable    :: angle(:,:)
      real(kind=dbl_kind), allocatable    :: anglet(:,:)
      real(kind=dbl_kind), allocatable    :: ht(:,:)
      real (kind=dbl_kind) :: &
         amin, amax         ! min and max values of input array
      real (kind=dbl_kind) :: &
         lon_scale
      real(kind=dbl_kind), allocatable    :: xc(:,:)
      real(kind=dbl_kind), allocatable    :: yc(:,:)
      real(kind=dbl_kind), allocatable    :: xt(:,:)
      real(kind=dbl_kind), allocatable    :: yt(:,:)
      real(kind=dbl_kind), allocatable    :: xe(:,:)
      real(kind=dbl_kind), allocatable    :: ye(:,:)
      real(kind=dbl_kind), allocatable    :: geo_latv(:,:)
      real(kind=dbl_kind), allocatable    :: geo_lonv(:,:)
!
!     Namelists
!     ---------
!
      namelist /nml1  / gridin, gridout, vgridout, gridin_format, &
                        gridout_format,                          &
                        kmtout, nx_global, ny_global, iob, job   
!
      read(5,nml1)
      write(0,nml1)
     print*, '======================================='
     print*, '======================================='
     print*, '======================================='
     print*, 'gengrid for *** NEW *** format of grid_spec.nc'
     print*, '======================================='
     print*, '======================================='
     print*, '======================================='

      my_pi = atan(1.0)*4
      print*, 'pi = ', my_pi
      rad_to_deg    = 180._dbl_kind/my_pi  ! degree-radian conversion

      allocate(ulat(nx_global, ny_global,vertex), stat=my_status)
      allocate(ulon(nx_global, ny_global,vertex), stat=my_status)
      !allocate(ulat(nx_global, ny_global), stat=my_status)
      !allocate(ulon(nx_global, ny_global), stat=my_status)
      allocate(htn(nx_global, ny_global), stat=my_status)
      allocate(hte(nx_global, ny_global), stat=my_status)
      allocate(hus(nx_global, ny_global), stat=my_status)
      allocate(huw(nx_global, ny_global), stat=my_status)
      allocate(kmt(nx_global, ny_global), stat=my_status)
      allocate(kmti(nx_global, ny_global), stat=my_status)
      allocate(sinrot(nx_global, ny_global), stat=my_status)
      allocate(cosrot(nx_global, ny_global), stat=my_status)
      allocate(angle(nx_global, ny_global), stat=my_status)
      allocate(anglet(nx_global, ny_global), stat=my_status)
      allocate(ht(nx_global, ny_global), stat=my_status)
      allocate(xc(nx_global, ny_global), stat=my_status)
      allocate(yc(nx_global, ny_global), stat=my_status)
      allocate(xt(nx_global, ny_global), stat=my_status)
      allocate(yt(nx_global, ny_global), stat=my_status)
      allocate(xe(nx_global+1, ny_global+1), stat=my_status)
      allocate(ye(nx_global+1, ny_global+1), stat=my_status)

      allocate(geo_latv(nx_global+1, ny_global+1), stat=my_status)
      allocate(geo_lonv(nx_global+1, ny_global+1), stat=my_status)
      
      if(gridin_format == 'nc') then

          my_status = nf90_open(gridin, NF90_NOWRITE, fid_in)
          if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot open '//trim(gridin)
          endif
 
         varname = 'x_vert_T' !Geographic longitude of T_cell vertices begin southwest counterclockwis 
         !varname = 'x_C' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif

         my_status = nf90_get_var( fid_in, varid, ulon &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif

         geo_lonv(1:nx_global,1:ny_global) = ulon(:,:,1)   
         geo_lonv(nx_global+1,1:ny_global) = ulon(nx_global,:,2)   
         geo_lonv(nx_global+1,ny_global+1) = ulon(nx_global,ny_global,3)   
         !do i=1, nx_global
         !  geo_lonv(i,ny_global+1) = ulon(nx_global-i+1,ny_global,3)   
         !enddo  
         geo_lonv(1:nx_global,ny_global+1) = ulon(1:nx_global,ny_global,4)


         !do nn=1,nx_global
         !  print*, 'nn = ', nn, ' ulong = ',ulon(nn,job,3)*rad_to_deg
           !print*, 'nn = ', nn, ' ulong = ',ulon(nn,job)*rad_to_deg
         !enddo



         varname = 'y_vert_T' !Geographic latitude of T_cell vertices begin southwest counterclockwise 
         !varname = 'y_C' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif

         my_status = nf90_get_var( fid_in, varid, ulat &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif
  
         geo_latv(1:nx_global,1:ny_global) = ulat(:,:,1)   
         geo_latv(nx_global+1,1:ny_global) = ulat(nx_global,:,2)   
         geo_latv(nx_global+1,ny_global+1) = ulat(nx_global,ny_global,3)   
         !do i=1, nx_global
         !  geo_latv(i,ny_global+1) = ulat(nx_global-i+1,ny_global,3)   
         !enddo  
         geo_latv(1:nx_global,ny_global+1) = ulat(1:nx_global,ny_global,4)

        ! geo_lonv(181,411) = -280.0_8

         print*, '===============================================' 
         print*, geo_lonv(iob,job),     geo_latv(iob,job)
         print*, geo_lonv(iob+1,job),   geo_latv(iob+1,job)
         print*, geo_lonv(iob+1,job+1), geo_latv(iob+1,job+1)
         print*, geo_lonv(iob,job+1),   geo_latv(iob,job+1)
         print*, '************************************************'
         print*, ulon(iob,job,1), ulat(iob,job,1)
         print*, ulon(iob,job,2), ulat(iob,job,2)
         print*, ulon(iob,job,3), ulat(iob,job,3)
         print*, ulon(iob,job,4), ulat(iob,job,4)
         print*, '===============================================' 

         ulon = ulon / rad_to_deg 
         ulat = ulat / rad_to_deg 


         !do nn=1,nx_global/2
         !  nm = nx_global-nn 
         !  print*, 'nn = ', nn, ' ulat = ',ulat(nn,job,3)*rad_to_deg, &
         !          'nm = ', nm, ulat(nm,job,3)*rad_to_deg       
           !print*, 'nn = ', nn, ' ulat = ',ulat(nn,job)*rad_to_deg, &
           !        'nm = ', nm, ulat(nm,job)*rad_to_deg       
         !enddo

         varname = 'ds_02_22_T' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, htn &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif

        htn = htn * 100

         varname = 'ds_20_22_T' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, hte &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif
     
         hte = hte * 100

         varname = 'ds_00_02_C' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, huw &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif
         huw = huw * 100

         varname = 'ds_00_20_C' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, hus &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif
         hus = hus * 100

         varname = 'num_levels' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, kmt &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif
         kmti = nint(kmt)

         varname = 'angle_C' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, sinrot &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif

         angle = sinrot/rad_to_deg

         varname = 'depth_t' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, ht &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif


         call get_field_nc(fid_in, 'angle_T', sinrot)
         call get_field_nc(fid_in, 'x_C'    , xc    )
         call get_field_nc(fid_in, 'y_C'    , yc    )
         call get_field_nc(fid_in, 'x_T'    , xt    )
         call get_field_nc(fid_in, 'y_T'    , yt    )
         call get_field_nc(fid_in, 'x_E'    , xe(1:nx_global, 1:ny_global))
         call get_field_nc(fid_in, 'y_E'    , ye(1:nx_global, 1:ny_global))

          my_status = nf90_close(fid_in)
          if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot close '//trim(gridin)
          endif


      else
         print*, 'input MOM grid must be in NETCDF format'
         stop
      endif  

      do j=1,ny_global
        do i=1,nx_global
           lon_scale    = cos(yt(i,j)/rad_to_deg)
           if (i > 1) then
              anglet(i,j) = atan2(ye(i,j) - ye(i-1,j), (xe(i,j) - xe(i-1,j))*lon_scale)
           endif
           if(i == iob .and. j==job) then
              print*, 'lon_scale = ', lon_scale
              print*, 'yt        = ', yt(i,j)
              print*, 'ye        = ', ye(i,j),  ye(i-1,j), ye(i,j) - ye(i-1,j)
              print*, 'xe        = ', xe(i,j),  xe(i-1,j), xe(i,j) - xe(i-1,j)
           endif
        enddo
      enddo

      print*, 'anglet (deg) from grid spec: ', sinrot(iob, job)
      print*, 'anglet (deg) computed      : ', anglet(iob, job)*rad_to_deg
      print*, 'xc,yc (deg)  from grid spec: ', xc(iob, job), yc(iob, job)
      print*, 'xc,yc (deg)  from vert arr : ', ulon(iob, job, 3)*rad_to_deg, ulat(iob, job, 3)*rad_to_deg


       anglet = sinrot/rad_to_deg
        
     
       amin = minval(ulat)*rad_to_deg
       amax = maxval(ulat, mask = ulat/= spval_dbl)*rad_to_deg
       print*, ' ulat min/max', amin, amax

      if(gridout_format == 'bin') then
         nbits = 64      
         nbytes = nbits/8
         recl_length = nx_global*ny_global*nbytes
         print*, 'recl length = ', recl_length
         open(fid_out,file=gridout,recl=recl_length, &
                  form='unformatted',access='direct')          
         nrec = 1 
         write(fid_out,rec=nrec) ulat(:,:,3)  
         !write(fid_out,rec=nrec) ulat  
         nrec = 2 
         write(fid_out,rec=nrec) ulon(:,:,3)  
         !write(fid_out,rec=nrec) ulon
         nrec = 3 
         write(fid_out,rec=nrec) htn 
         nrec = 4 
         write(fid_out,rec=nrec) hte  
         nrec = 5 
         write(fid_out,rec=nrec) hus  
         nrec = 6 
         write(fid_out,rec=nrec) huw  
         nrec = 7 
         write(fid_out,rec=nrec) angle  
         nrec = 8 
         write(fid_out,rec=nrec) anglet 
         close(fid_out) 
         nbits = 32      
         nbytes = nbits/8
         recl_length = nx_global*ny_global*nbytes
         print*, 'recl length = ', recl_length
         open(fid_out,file=kmtout,recl=recl_length, &
                  form='unformatted',access='direct')          
            
         nrec = 1 
         write(fid_out,rec=nrec) kmti 
         close(fid_out) 
         nbits = 64      
         nbytes = nbits/8
         recl_length = nx_global*ny_global*nbytes
         print*, 'recl length = ', recl_length
         open(fid_out,file=vgridout,recl=recl_length, &
                  form='unformatted',access='direct')          
         nrec = 1 
         write(fid_out,rec=nrec) kmt 
         !nrec = 2 
         !write(fid_out,rec=nrec) ht 
         close(fid_out) 
         print*, 'at ', iob, job
         print*, ulat(iob,job,3)*rad_to_deg, ulon(iob,job,3)*rad_to_deg
         !print*, ulat(iob,job)*rad_to_deg, ulon(iob,job)*rad_to_deg
         print*, 'htn, hte: ', htn(iob,job),  hte(iob,job)
         print*, 'hus, huw: ', hus(iob,job),  huw(iob,job)
         print*, 'angleu  : ', angle(iob,job)*rad_to_deg, angle(iob-1,job-1)*rad_to_deg, &
                 angle(iob,job-1)*rad_to_deg, angle(iob-1,job)*rad_to_deg
         print*, 'anglet  :', anglet(iob,job)*rad_to_deg 
         print*, 'ht, kmt :', ht(iob,job), kmt(iob,job)
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
      deallocate(anglet)
      deallocate(ht)
      deallocate(xc)
      deallocate(yc)
      deallocate(xt)
      deallocate(yt)
      deallocate(xe)
      deallocate(ye)

      deallocate(geo_latv)
      deallocate(geo_lonv)

contains


       subroutine get_field_nc(fid, varname, var)

         integer,      intent(in) :: fid
         character(*), intent(in) :: varname
         real(kind=dbl_kind), dimension(:,:), intent(out) :: var


         integer :: my_status, varid 
       
         my_status = nf90_inq_varid(fid, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var(fid, varid, var)
         if (my_status /= nf90_noerr) then
             print*,  'Error reading variable '//trim(varname)
             stop
         endif

       end subroutine get_field_nc 


      end




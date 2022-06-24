
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
      real (kind=dbl_kind), parameter :: &
        eps = 1.0e-11_dbl_kind    ! special value (double precision)
      integer, parameter :: vertex  = 4

      real alon, C2RAD,  dlon1, deq, dsig, &
     &    phq, phs, PI, R2RAD, RAD, th0, th, &
     &    dphi_min, dphi_max, dphi_avg, dphi
      integer i, j, nih1, njh1, njmax, jeq, jm, i_offset, i_th
      integer after, before, rate
      real delta_time, total_time, startup_time
      real southern_time, northern_time, metrics_time, shutdown_time
      real processing_time, combination_time
     
      character(len=char_len_long) :: &
           gridin, maskin, gridout, vgridout, kmtout, gridin_format, gridout_format
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
      real(kind=dbl_kind), allocatable    :: anglet(:,:)
      real(kind=dbl_kind), allocatable    :: ht(:,:)
      real (kind=dbl_kind) :: &
         amin, amax         ! min and max values of input array
      integer ::  mloc(2)
      real (kind=dbl_kind) :: &
         lon_scale
      real (kind=dbl_kind) :: &
         lpole, rpole, ang
      real(kind=dbl_kind), allocatable    :: xc(:,:)
      real(kind=dbl_kind), allocatable    :: yc(:,:)
      real(kind=dbl_kind), allocatable    :: xt(:,:)
      real(kind=dbl_kind), allocatable    :: yt(:,:)
      real(kind=dbl_kind), allocatable    :: xe(:,:)
      real(kind=dbl_kind), allocatable    :: ye(:,:)
      real(kind=dbl_kind), allocatable    :: tarea(:,:)
      real(kind=dbl_kind), allocatable    :: uarea(:,:)
      real(kind=dbl_kind), allocatable    :: geo_latv(:,:)
      real(kind=dbl_kind), allocatable    :: geo_lonv(:,:)

      real(kind=dbl_kind), allocatable    :: x(:,:)
      real(kind=dbl_kind), allocatable    :: y(:,:)
      real(kind=dbl_kind), allocatable    :: dx(:,:)
      real(kind=dbl_kind), allocatable    :: dy(:,:)
      real(kind=dbl_kind), allocatable    :: angle_dx(:,:)
      real(kind=dbl_kind), allocatable    :: area(:,:)
!
!     Namelists
!     ---------
!
      namelist /nml1  / gridin, gridout, vgridout, gridin_format, &
                        gridout_format, maskin,                   &
                        kmtout, nx_global, ny_global, iob, job   
!
      read(5,nml1)
      write(0,nml1)
     print*, '======================================='
     print*, '======================================='
     print*, '======================================='
     print*, 'gengrid for *** MOM6 *** grid format'
     print*, '======================================='
     print*, '======================================='
     print*, '======================================='

      my_pi = atan(1.0)*4
      print*, 'pi = ', my_pi
      rad_to_deg    = 180._dbl_kind/my_pi  ! degree-radian conversion

      !allocate(ulat(nx_global, ny_global,vertex), stat=my_status)
      !allocate(ulon(nx_global, ny_global,vertex), stat=my_status)
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
      allocate(anglet(nx_global, ny_global), stat=my_status)
      allocate(ht(nx_global, ny_global), stat=my_status)
      allocate(xc(nx_global, ny_global), stat=my_status)
      allocate(yc(nx_global, ny_global), stat=my_status)
      allocate(xt(nx_global, ny_global), stat=my_status)
      allocate(yt(nx_global, ny_global), stat=my_status)
      allocate(xe(nx_global+1, ny_global+1), stat=my_status)
      allocate(ye(nx_global+1, ny_global+1), stat=my_status)
      allocate(tarea(nx_global, ny_global), stat=my_status)
      allocate(uarea(nx_global, ny_global), stat=my_status)


      allocate(x(nx_global+1, ny_global+1), stat=my_status)
      allocate(y(nx_global+1, ny_global+1), stat=my_status)
      allocate(area(2*nx_global,2*ny_global  ), stat=my_status)
      
      if(gridin_format == 'nc') then

         my_status = nf90_open(gridin, NF90_NOWRITE, fid_in)
         if (my_status /= nf90_noerr) then
            print*, &
                   'Cannot open '//trim(gridin)
         endif
 
         call get_field_nc(fid_in, 'lon_corners'    , x    )
         call get_field_nc(fid_in, 'lat_corners'    , y    )
         call get_field_nc(fid_in, 'lon_centers'    , xt   )
         call get_field_nc(fid_in, 'lat_centers'    , yt   )
        
         ulat = y(2:,2:) 
         ulon = x(2:,2:) 

         print*, '************************************************'
         print*, ulon(iob,job), ulat(iob,job)
         print*, '===============================================' 

         ulon = ulon / rad_to_deg 
         ulat = ulat / rad_to_deg 
         xt   = xt   / rad_to_deg 
         yt   = yt   / rad_to_deg 


         call get_field_nc(fid_in, 'htn'    , htn    )
         call get_field_nc(fid_in, 'hte'    , hte    )


         htn = htn * 100

         ! avoid 0 hte's because CICE4 remap transport scheme 
         ! didn't check landmask before performing divisions
         ! on hte's
         where(hte == 0.0)
            hte = 3.11e-10 
         endwhere
         hte = hte * 100

         call get_field_nc(fid_in, 'huw'    , huw    )
         call get_field_nc(fid_in, 'hus'    , hus    )

         huw = huw * 100
         hus = hus * 100

         call get_field_nc(fid_in, 'anglet'    , anglet    )
         call get_field_nc(fid_in, 'angleu'    , angle    )
         angle  = angle/rad_to_deg
         anglet = anglet/rad_to_deg

         call get_field_nc(fid_in, 'areat'    , tarea    )
         call get_field_nc(fid_in, 'areau'    , uarea    )
         !tarea = area(1::2,1::2)
         !uarea = area(2::2,2::2)

         !call get_field_nc(fid_in, 'angle_T', sinrot)
         !call get_field_nc(fid_in, 'x_C'    , xc    )
         !call get_field_nc(fid_in, 'y_C'    , yc    )
         !call get_field_nc(fid_in, 'x_T'    , xt    )
         !call get_field_nc(fid_in, 'y_T'    , yt    )
         !call get_field_nc(fid_in, 'area_T' , tarea )
         !call get_field_nc(fid_in, 'area_C' , uarea )
         !call get_field_nc(fid_in, 'x_E'    , xe(1:nx_global, 1:ny_global))
         !call get_field_nc(fid_in, 'y_E'    , ye(1:nx_global, 1:ny_global))

         my_status = nf90_close(fid_in)
         if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot close '//trim(gridin)
         endif

         my_status = nf90_open(maskin, NF90_NOWRITE, fid_in)
         if (my_status /= nf90_noerr) then
            print*, &
                   'Cannot open '//trim(maskin)
         endif

         call get_field_nc(fid_in, 'mask'    , kmt    )

         kmti = nint(kmt)

         my_status = nf90_close(fid_in)
         if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot close '//trim(maskin)
         endif

      else
         print*, 'input MOM grid must be in NETCDF format'
         stop
      endif  

#if 0
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
#endif


       lpole = ulon(1,ny_global)*rad_to_deg  
       rpole = ulon(nx_global/2,ny_global)*rad_to_deg  
       ang = angle(nx_global-nx_global/8,ny_global)*rad_to_deg  
       if (lpole < 0.0) lpole = lpole + 360.0
       if (rpole < 0.0) rpole = rpole + 360.0
       if (lpole > 180.0 .and. rpole < 180.0) then
          print*, 'WARNING: Non standard tripolar grid!!!'
          print*, 'WARNING: Rotation angle needs careful check!!!'
       else
          if (abs(ang - (-90.0)) > eps) then
             print*, ' '    
             print*, '??????????????????????????????????????????????????????????????'
             print*, 'ERROR: Wrong angle for CICE rotation code!!!'
             print*, 'ERROR: angle is', ang  
             print*, 'ERROR: angle must be -90 degrees at this location!!!'
             print*, 'ERROR: grid files will be produced, BUT...'
             print*, 'ERROR: You should **DOUBLE CHECK** the original MOM6 grid'
             print*, '??????????????????????????????????????????????????????????????'
             print*, ' '    
          endif 
       endif 

        
     
       amin = minval(ulat)*rad_to_deg
       amax = maxval(ulat, mask = ulat/= spval_dbl)*rad_to_deg
       print*, ' ulat min/max', amin, amax

       amin = minval(tarea)
       amax = maxval(tarea)
       mloc = minloc(tarea)
       print*, ' tarea min/max', amin, amax
       print*, ' tarea minloc', mloc(1), mloc(2)

       amin = minval(hus)
       amax = maxval(hus)
       mloc = minloc(hus)
       print*, ' hus min/max', amin, amax
       print*, ' hus minloc', mloc(1), mloc(2)

       amin = minval(huw)
       amax = maxval(huw)
       mloc = minloc(huw)
       print*, ' huw min/max', amin, amax
       print*, ' huw minloc', mloc(1), mloc(2)

       amin = minval(hte)
       amax = maxval(hte)
       mloc = minloc(hte)
       print*, ' hte min/max', amin, amax
       print*, ' hte minloc', mloc(1), mloc(2)

       amin = minval(htn)
       amax = maxval(htn)
       mloc = minloc(htn)
       print*, ' htn min/max', amin, amax
       print*, ' htn minloc', mloc(1), mloc(2)

       amin = minval(anglet)
       amax = maxval(anglet)
       mloc = maxloc(anglet)
       print*, ' anglet min/max', amin, amax
       mloc = maxloc(anglet)
       print*, ' anglet maxloc', mloc(1), mloc(2)
       mloc = minloc(anglet)
       print*, ' anglet minloc', mloc(1), mloc(2)

       amin = minval(angle)
       amax = maxval(angle)
       mloc = maxloc(angle)
       print*, ' angle min/max', amin, amax
       print*, ' angle maxloc', mloc(1), mloc(2)
       mloc = minloc(angle)
       print*, ' angle minloc', mloc(1), mloc(2)


      if(gridout_format == 'bin') then
         nbits = 64      
         nbytes = nbits/8
         recl_length = nx_global*ny_global*nbytes
         print*, 'recl length = ', recl_length
         open(fid_out,file=gridout,recl=recl_length, &
                  form='unformatted',access='direct')          
         nrec = 1 
         write(fid_out,rec=nrec) ulat  
         nrec = 2 
         write(fid_out,rec=nrec) ulon
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
         write(fid_out,rec=nrec) yt 
         nrec = 9 
         write(fid_out,rec=nrec) xt 
         nrec = 10 
         write(fid_out,rec=nrec) tarea
         nrec = 11 
         write(fid_out,rec=nrec) uarea
         nrec = 12 
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
         !nbits = 64      
         !nbytes = nbits/8
         !recl_length = nx_global*ny_global*nbytes
         !print*, 'recl length = ', recl_length
         !open(fid_out,file=vgridout,recl=recl_length, &
         !         form='unformatted',access='direct')          
         !nrec = 1 
         !write(fid_out,rec=nrec) kmt 
         !close(fid_out) 
         print*, 'at ', iob, job
         print*, 'ulon, ulat: ', ulon(iob,job)*rad_to_deg, ulat(iob,job)*rad_to_deg
         print*, 'htn, hte: ', htn(iob,job),  hte(iob,job)
         print*, 'hus, huw: ', hus(iob,job),  huw(iob,job)
         print*, 'angleu  : ', angle(iob,job)*rad_to_deg, angle(iob-1,job-1)*rad_to_deg, &
                 angle(iob,job-1)*rad_to_deg, angle(iob-1,job)*rad_to_deg
         print*, 'anglet  :', anglet(iob,job)*rad_to_deg 
         print*, 'kmt :',  kmti(iob,job)
         print*, 'tarea :',  tarea(iob,job)
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
      deallocate(tarea)
      deallocate(uarea)

      deallocate(x)
      deallocate(y)
      ! deallocate(dx)
      ! deallocate(dy)
      ! deallocate(angle_dx)
      deallocate(area)

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




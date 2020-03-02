
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
      real (kind=dbl_kind) :: &
         lon_scale
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


      allocate(x(2*nx_global+1, 2*ny_global+1), stat=my_status)
      allocate(y(2*nx_global+1, 2*ny_global+1), stat=my_status)
      allocate(dx(2*nx_global,  2*ny_global+1), stat=my_status)
      allocate(dy(2*nx_global+1,2*ny_global  ), stat=my_status)
      allocate(angle_dx(2*nx_global+1, 2*ny_global+1), stat=my_status)
      allocate(area(2*nx_global,2*ny_global  ), stat=my_status)
      
      if(gridin_format == 'nc') then

         my_status = nf90_open(gridin, NF90_NOWRITE, fid_in)
         if (my_status /= nf90_noerr) then
            print*, &
                   'Cannot open '//trim(gridin)
         endif
 
         call get_field_nc(fid_in, 'x'    , x    )
         call get_field_nc(fid_in, 'y'    , y    )
        
         ulat = y(3::2,3::2) 
         ulon = x(3::2,3::2) 
         yt   = y(2::2,2::2)
         xt   = x(2::2,2::2)

         print*, '************************************************'
         print*, ulon(iob,job), ulat(iob,job)
         print*, '===============================================' 

         ulon = ulon / rad_to_deg 
         ulat = ulat / rad_to_deg 
         xt   = xt   / rad_to_deg 
         yt   = yt   / rad_to_deg 


         call get_field_nc(fid_in, 'dx'    , dx    )
         call get_field_nc(fid_in, 'dy'    , dy    )


         htn = dx(1::2, 3::2) * 100
     
         hte = dy(3::2, 1::2) * 100

         huw = dy(2::2, 2::2) * 100

         hus = dx(2::2, 2::2) * 100



         call get_field_nc(fid_in, 'angle_dx'    , angle_dx    )
         angle  = angle_dx(3::2,3::2)/rad_to_deg
         anglet = angle_dx(2::2,2::2)/rad_to_deg

         call get_field_nc(fid_in, 'area'    , area    )
         tarea = area(1::2,1::2)
         uarea = area(2::2,2::2)

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
         !write(fid_out,rec=nrec) ulat(:,:,3)  
         write(fid_out,rec=nrec) ulat  
         nrec = 2 
         !write(fid_out,rec=nrec) ulon(:,:,3)  
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
         print*, ulat(iob,job)*rad_to_deg, ulon(iob,job)*rad_to_deg
         !print*, ulat(iob,job)*rad_to_deg, ulon(iob,job)*rad_to_deg
         print*, 'htn, hte: ', htn(iob,job),  hte(iob,job)
         print*, 'hus, huw: ', hus(iob,job),  huw(iob,job)
         print*, 'angleu  : ', angle(iob,job)*rad_to_deg, angle(iob-1,job-1)*rad_to_deg, &
                 angle(iob,job-1)*rad_to_deg, angle(iob-1,job)*rad_to_deg
         print*, 'anglet  :', anglet(iob,job)*rad_to_deg 
         !print*, 'ht, kmt :', ht(iob,job), kmt(iob,job)
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
      deallocate(dx)
      deallocate(dy)
      deallocate(angle_dx)
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




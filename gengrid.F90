
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
      
      if(gridin_format == 'nc') then

          my_status = nf90_open(gridin, NF90_NOWRITE, fid_in)
          if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot open '//trim(gridin)
          endif
 
         varname = 'geolon_c' 
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

         ulon = ulon / rad_to_deg 

         varname = 'geolat_c' 
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
  
         ulat = ulat / rad_to_deg 

         varname = 'dxtn' 
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

         varname = 'dyte' 
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

         varname = 'dytn' 
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

         varname = 'dxte' 
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

         varname = 'kmt' 
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

         varname = 'sin_rot' 
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

         varname = 'cos_rot' 
         my_status = nf90_inq_varid(fid_in, varname, varid)
         if (my_status /= nf90_noerr) then
           print*, & 
            'Cannot find variable '//trim(varname)
         else
            print*, trim(varname), ' id is ', varid  
         endif
         my_status = nf90_get_var( fid_in, varid, cosrot &
               !, start=(/1,1,nrec/), &
               !count=(/nx_global,ny_global,1/) &
                 )
         if (my_status /= nf90_noerr) then
           print*, & 
            'Error reading variable '//trim(varname)
           stop
         endif

         where(cosrot /= 0.0)
             sinrot = sinrot / cosrot
         elsewhere
             sinrot = -99.0
         endwhere 

          where(sinrot == -99.0) 
             angle = 90.0/rad_to_deg
          elsewhere
             angle = atan(sinrot)
          endwhere    

          my_status = nf90_close(fid_in)
          if (my_status /= nf90_noerr) then
             print*, &
                   'Cannot close '//trim(gridin)
          endif


      else
         print*, 'input MOM grid must be in NETCDF format'
         stop
      endif  
     

      if(gridout_format == 'bin') then
         nbits = 64      
         open(fid_out,file=gridout,recl=nx_global*ny_global*nbits/8, &
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
         close(fid_out) 
         nbits = 32      
         open(fid_out,file=kmtout,recl=nx_global*ny_global*nbits/8, &
                  form='unformatted',access='direct')          
            
         nrec = 1 
         write(fid_out,rec=nrec) kmti 
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




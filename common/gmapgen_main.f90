#include "Component_def.h"

program gmapgen_main
  use dc_types, only: DP, TOKEN
    
  use grid_mapping_util, only: &
       & gen_gridmapfile_lonlat2lonlat
  
  implicit none

  character(TOKEN) :: &
       & gmapfile_ao, gmapfile_oa
  
  integer :: IMAXA, JMAXA, NMAXA
  integer :: IMAXO, JMAXO, NMAXO

  real(DP), allocatable :: x_LonA(:), y_LatA(:)
  real(DP), allocatable :: x_LonO(:), y_LatO(:)

  !
  !
  gmapfile_ao = "gmaplonlat_ATM2OCN.dat"  
  gmapfile_oa = "gmaplonlat_OCN2ATM.dat"
  
  IMAXA =  AGCM_NX; JMAXA = AGCM_NY; NMAXA = AGCM_NMAX
  IMAXO =  OGCM_NX; JMAXO = OGCM_NY; NMAXO = OGCM_NMAX

  !
  !
  call get_LonLatGrid(x_LonA, y_LatA, IMAXA, JMAXA, NMAXA)
  call get_LonLatGrid(x_LonO, y_LatO, IMAXO, JMAXO, NMAXO)

  !
  !
!!$  write(*,*) "=Atm:"
!!$  write(*,*) "*Lon=", x_LonA
!!$  write(*,*) "*Lat=", y_LatA
!!$  write(*,*) "=Ocn:"
!!$  write(*,*) "*Lon=", x_LonO
!!$  write(*,*) "*Lat=", y_LatO

  !
  !
  call gen_gridmapfile_lonlat2lonlat( gmapfile_ao, &
       & x_LonA, y_LatA, x_LonO, y_LatO )

  call gen_gridmapfile_lonlat2lonlat( gmapfile_oa, &
       & x_LonO, y_LatO, x_LonA, y_LatA )

  !
  !
  call check_mappingTable( gmapfile_ao, IMAXA, IMAXO )
  call check_mappingTable( gmapfile_oa, IMAXO, IMAXA )
  
contains
  subroutine get_LonLatGrid(x_Lon, y_Lat, iMax, jMax, nMax)
    use w_module, only: &
         & w_Initial, w_Finalize, &
         & xy_Lon, xy_Lat

    use w_zonal_module, only: &
         & w_Initial_zonal => w_Initial, &
         & w_Finalize_zonal => w_Finalize, &       
         & xy_Lon_zonal => xy_Lon, &
         & xy_Lat_zonal => xy_Lat
    
    integer, intent(in) :: iMax, jMax, nMax
    real(DP), intent(inout), allocatable :: x_Lon(:), y_Lat(:)

    if(iMax==1) then
       call w_Initial_zonal(nMax, iMax, jMax)
    else
       call w_Initial(nMax, iMax, jMax)     
    end if

    allocate(x_Lon(iMax), y_Lat(jMax))

    if(iMax==1) then
       x_Lon(:) = xy_Lon_zonal(:,1); y_Lat(:) = xy_Lat_zonal(0,:)       
       call w_Finalize_zonal()
    else
       x_Lon(:) = xy_Lon(:,1); y_Lat(:) = xy_Lat(0,:)       
       call w_Finalize()
    end if
  end subroutine get_LonLatGrid
  
  subroutine check_mappingTable(gmapfilename, GNXS, GNXR)
    use grid_mapping_util, only: set_mappingTable_interpCoef
    
    character(*), intent(in) :: gmapfilename
    integer, intent(in) :: GNXS, GNXR

    integer, allocatable, dimension(:) :: send_index, recv_index
    real(DP), allocatable, dimension(:) :: coef

    call set_mappingTable_interpCoef( gmapfilename, GNXS, GNXR, &
         & send_index, recv_index, coef )

    write(*,*) "* Set mapping table and coeffecient for interpolation.. file=", trim(gmapfilename)
    write(*,*) "  -- Grid Mapping for 1-4 operation to interpolate ---------"
    write(*,*) "  recv_index=", recv_index(1:4), "send_index=", send_index(1:4)
    write(*,*) "  coef=", coef(1:4)
    
  end subroutine check_mappingTable
  
end program gmapgen_main

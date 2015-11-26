!-------------------------------------------------------------
! Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
!-------------------------------------------------------------
!> @brief a template module
!! 
!! @author Kawai Yuta
!!
!!
module mod_atm

  ! モジュール引用; Use statements
  !

  !* gtool
  
  use dc_types, only: &
       & DP, TOKEN, STRING

  use dc_message, only: &
       & MessageNotify
  
  !* DCPAM
  
  use dcpam_main_mod, only: &
       & agcm_main_init => dcpam_main_Init,   &
       & agcm_main_final => dcpam_main_Final, &
       & agcm_setup => MainInit, &
       & agcm_shutdown => MainTerminate, &       
       & agcm_advance_timestep => dcpam_advance_timestep, &
       & agcm_update_surfprop => dcpam_UpdateSurfaceProperties

  !* DCPCM

  use component_field, only: &
       & component_field_type
  
  use field_common, only: &
       & ATM, ATM_GRID_2D, ATM_GRID_3D, &
       & GNXA, GNYA, GNZA, GN25,        &
       & OCN, OCN_GRID, GNXO, GNYO, &
       & GMAP_AO_FILENAME, GMAP_OA_FILENAME, &
       & JCUP_LOG_LEVEL, JCUP_LOG_STDERROR

#include "../common/Component_def.h"
  
  ! 宣言文; Declareration statements
  !    
  implicit none
  private

  ! 公開手続き
  ! Public procedure
  !    
  public :: atm_init
  public :: atm_run
  public :: atm_fin

  ! 非公開変数
  ! Private variable
  !
  
  integer :: itime(6)
  real(DP) :: RestartTimeSec

  integer :: tstep
  logical :: loop_end_flag
  
  integer :: delta_t

  integer :: DIV_X, DIV_Y
  integer :: my_comm, my_group, my_rank, my_size

  type(component_field_type) :: field
  type(component_field_type) :: field3d

  integer, parameter :: OCN_coupling_cycle = ATMOCN_COUPLING_CYCLE

contains

  subroutine atm_init()

    !
    !
    use jcup_interface, only: &
         & jcup_get_mpi_parameter, &
         & jcup_init_time

    use field_def, only: &
         & cal_mn

    !* dcpam5

    use gridset, only: a_jmax

    use timeset, only: InitialDate, DelTime, RestartTime
    
    
    !
    !

    ! Initialize coupler
    !

    call MessageNotify('M', 'atm component', &
         & 'Initialize coupler..')
    
    ! Initialize a ATM component for Jcup
    call init_jcup_ATM()    

    ! Get some information associated with MPI.
    call jcup_get_mpi_parameter(ATM, my_comm, my_group, my_size, my_rank)

    !  DCPAM Intiialize *******************
    !
    call MessageNotify('M', 'atm component', &
         & 'Initialize DPAM5 .. (MPI_MY_COMM=%d, MY_RANK=%d)', &
         & i=(/my_comm, my_rank/))

    call agcm_main_init(my_comm)
    call agcm_setup()

    itime(:) = (/ InitialDate%year, InitialDate%month, InitialDate%day, &
         &        InitialDate%hour, InitialDate%min, int(InitialDate%sec)  /)
    RestartTimeSec = RestartTime
    delta_t = AGCM_DELTIME
    
    !*****************************************************
    
    !
    !
    
    !    call cal_mn(my_size, DIV_X, DIV_Y)
    DIV_X = 1; DIV_Y = size(a_jmax)
    
    write(*,*) "ATM: rank=", my_rank, "DIV_X,Y=", DIV_X, DIV_Y, "mysize=", my_size
    

    ! Initialize grid for Jcup
    call init_jcup_grid()

    ! Initialize variables for JCup
    call init_jcup_var()

    ! Initialize an module to interpolate and grid mapping between model componets
    ! for JCup
    call init_jcup_interpolate()
    
    !
    !

    write(*,*) "atm: rank=", my_rank, "init_time=(/", itime, "/)", &
         & "RestartTimeSec=", RestartTimeSec
     
    tstep = 0;
!     loop_end_flag = .false.
    !    call agcm_advance_timestep(tstep, loop_end_flag)
    call jcup_init_time(itime)
    call output_prepare()
    
    call set_and_put_data(tstep)


    
  end subroutine atm_init

  subroutine atm_run(loop_flag)

    use jcup_interface, only: &
         & jcup_set_time, jcup_inc_time

    !* DCPAM
    use timeset, only: DelTime
    
    logical, intent(inout) :: loop_flag

    tstep = 1; loop_end_flag = .false.
    do while(.not. loop_end_flag)

       call jcup_set_time(ATM, itime, int(delta_t))!DelTime))

!!$       write(*,*) "* COUPLER Get: atm my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t              
       call get_and_write_data(tstep)

       if (my_rank==0 .or. JCUP_LOG_LEVEL >=1 ) then
          write(*,*) "-> atm my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t
       end if
       call agcm_advance_timestep(tstep, loop_end_flag)
!!$       write(*,*) "<- atm my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t

!!$       write(*,*) "* COUPLER Put: atm my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t              
       call set_and_put_data(tstep)
       
       call jcup_inc_time(ATM, itime)
       tstep = tstep + 1

       !       if(tstep == 2*24 * 365 * 30 + 1) loop_end_flag = .true.
!       if(tstep == 2*24 *181 + 1) loop_end_flag = .true.       
       if(tstep == 2*24 *731 + 1) loop_end_flag = .true.       
       
    end do
    loop_flag = .false.
    
  end subroutine atm_run

  subroutine atm_fin()
    use jcup_interface, only: &
         & jcup_coupling_end

    write(*,*) 'atm fin: my_rank=', my_rank
    call jcup_coupling_end(itime, .false.)
    
    write(*,*) ' = DCPAM fin: my_rank=', my_rank    
    call agcm_shutdown()
    write(*,*) ' --------- DCPAM fin: my_rank=', my_rank        

    write(*,*) '-----atm fin: my_rank=', my_rank
    
  end subroutine atm_fin

  !=================================================

  !
  !
  subroutine init_jcup_ATM()
    use jcup_interface, only: &
         & jcup_set_new_comp, jcup_initialize
    
    call jcup_set_new_comp(ATM)
    call jcup_initialize(ATM, LOG_LEVEL=JCUP_LOG_LEVEL, LOG_STDERR = JCUP_LOG_STDERROR)
    
  end subroutine init_jcup_ATM

  !
  !
  subroutine init_jcup_grid()
    use jcup_interface, only: &
         & jcup_def_grid, jcup_end_grid_def

    use field_def, only: &
         & init_field_def, set_field_def,   &
         & get_local_field, cal_grid_index

    use component_field, only: &
         & init_field

    !* DCPAM5
    use gridset, only: a_jmax

    !
    integer :: lis, lie, ljs, lje
    real(DP), allocatable :: grid_index(:)

    !
    call init_field_def(1)
    call set_field_def( ATM, ATM_GRID_2D, GNXA, GNYA, 1, 2, DIV_X, DIV_Y, &
         & lnx_field=(/ GNXA /), lny_field=a_jmax )   ! halo=2   
!    call set_field_def(ATM, ATM_GRID_3D, GNXA, GNYA, GNZA, 2, DIV_X, DIV_Y)

    call get_local_field(component_name=ATM, grid_name=ATM_GRID_2D, &
         & local_is=lis, local_ie = lie, local_js=ljs, local_je=lje)
    call init_field(field, ATM_GRID_2D, lis, lie, ljs, lje, 1, 1) ! halo=1
    call init_field(field3d, ATM_GRID_3D, lis, lie, ljs, lje, 1, GNZA) ! halo=1
    write(*,*) "ATM: rank=", my_rank, "li=", lis, lie, " ,lj=", ljs, lje

    call gen_grid_index()
!    call cal_grid_index(ATM, ATM_GRID_2D, field%grid_index)
!    write(*,*) "ATM: grid_index=", field%grid_index

!    call cal_grid_index(ATM, ATM_GRID_3D, field3d%grid_index)

    call jcup_def_grid(field%grid_index, ATM, ATM_GRID_2D, GN25)
!    call jcup_def_grid(field3d%grid_index, ATM, ATM_GRID_3D)
    
    call jcup_end_grid_def()
    
  end subroutine init_jcup_grid

  subroutine init_jcup_var()
    use jcup_interface, only: &
         & jcup_def_varp, jcup_def_varg, jcup_end_var_def

    use component_field, only: &
         & init_field_data

    !
    call init_field_data(field, num_of_25d=GN25, num_of_varp=13, num_of_varg=3)    
    call init_field_data(field3d, num_of_25d=1, num_of_varp=1, num_of_varg=1)    

    call jcup_def_varp(field%varp(1)%varp_ptr, ATM, "TauXAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(2)%varp_ptr, ATM, "TauYAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(3)%varp_ptr, ATM, "SensFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(4)%varp_ptr, ATM, "LatentFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(5)%varp_ptr, ATM, "SWDWRFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(6)%varp_ptr, ATM, "LWDWRFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(7)%varp_ptr, ATM, "SWUWRFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(8)%varp_ptr, ATM, "LWUWRFlxAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(9)%varp_ptr, ATM, "RainAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(10)%varp_ptr, ATM, "SnowAtm", ATM_GRID_2D)
    call jcup_def_varp(field%varp(11)%varp_ptr, ATM, "DSurfHFlxDTsAtm", ATM_GRID_2D) 
    call jcup_def_varp(field%varp(12)%varp_ptr, ATM, "SurfAirTempAtm", ATM_GRID_2D) 
    call jcup_def_varp(field%varp(13)%varp_ptr, ATM, "DSurfLatentFlxDTsAtm", ATM_GRID_2D) 
!!$    call jcup_def_varp(field%varp(11)%varp_ptr, ATM, "SurfTempTransCoefAtm", ATM_GRID_2D) 
!!$    call jcup_def_varp(field%varp(12)%varp_ptr, ATM, "SurfQVapTransCoefAtm", ATM_GRID_2D) 

    call jcup_def_varg(field%varg(1)%varg_ptr, ATM, "SurfTempOcn", ATM_GRID_2D, 1, &
         & SEND_MODEL_NAME=OCN, SEND_DATA_NAME="SurfTempOcn", &
         & RECV_MODE="SNP", INTERVAL=OCN_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    call jcup_def_varg(field%varg(2)%varg_ptr, ATM, "SurfAlbedoOcn", ATM_GRID_2D, 1,  &
         & SEND_MODEL_NAME=OCN, SEND_DATA_NAME="SurfAlbedoOcn", &
         & RECV_MODE="SNP", INTERVAL=OCN_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)

    call jcup_def_varg(field%varg(3)%varg_ptr, ATM, "SurfSnowOcn", ATM_GRID_2D, 1,  &
         & SEND_MODEL_NAME=OCN, SEND_DATA_NAME="SurfSnowOcn", &
         & RECV_MODE="SNP", INTERVAL=OCN_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    
    call jcup_end_var_def()
    
  end subroutine init_jcup_var

  subroutine init_jcup_interpolate()

    use jcup_interface, only: &
         & jcup_set_mapping_table
    use jcup_mpi_lib, only: jml_finalize

    use field_def, only : cal_mn, &
         & set_grid_mapping, set_grid_mapping_3d

    use interpolation_data_latlon_mod, only: &
         & init_interpolation => interpolation_data_latlon_Init, &
         & set_operation_index, &
         & set_A_to_O_coef, set_O_to_A_coef

    use grid_mapping_util, only: &
         & set_mappingTable_interpCoef

    !
    !
    integer, allocatable, dimension(:) :: &
       & send_grid_ao, recv_grid_ao, &
       & send_grid_oa, recv_grid_oa
    real(DP), allocatable :: coefS_ao_global(:), coefS_oa_global(:)

    character(TOKEN) :: gmapfile_ao, gmapfile_oa
    
    !
    call init_interpolation(2, 1, 1)

    
    ! ATM -> OCN grid mapping    **************************x
    if(my_rank==0) then
       call set_mappingTable_interpCoef( &
            & GMAP_AO_FILENAME, GNXA, GNXO, &
            & send_grid_ao, recv_grid_ao, coefS_ao_global)
       write(*,*) "A20:", size(send_grid_ao), size(recv_grid_ao), size(coefS_ao_global)
    end if
    call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, OCN, OCN_GRID, 1, &
         & send_grid_ao, recv_grid_ao)

    ! OCN -> ATM grid mapping   *****************************    
    if(my_rank==0) then
       call set_mappingTable_interpCoef( &
            & GMAP_OA_FILENAME, GNXO, GNXA, &
            & send_grid_oa, recv_grid_oa, coefS_oa_global)
       write(*,*) "send_grid_oa:", send_grid_oa
       write(*,*) "recv_grid_oa:", recv_grid_oa       
    end if
    call jcup_set_mapping_table(ATM, OCN, OCN_GRID, ATM, ATM_GRID_2D, 1, &
         & send_grid_oa, recv_grid_oa)
    call set_operation_index(ATM, OCN, 1)

    if(my_rank==0) then
       call set_A_to_O_coef(1, coefS_ao_global)
    else
       call set_A_to_O_coef(1)
    end if

    if(my_rank==0) then
       call set_O_to_A_coef(1, coefS_oa_global)
    else
       call set_O_to_A_coef(1)       
    end if

  end subroutine init_jcup_interpolate

  !=================================================
  
  subroutine set_and_put_data(step)

    use jcup_interface, only: &
         & jcup_put_data
    use field_def, only: &
         & set_send_data_2d

    use axesset, only: x_Lon, y_Lat
    use gridset,only: imax, jmax

    use constants, only: LatentHeat, CpDry

    use composition, only: IndexH2OVap
    
    use dcpam_main_mod, only: &
         & xy_SurfMomFluxX, xy_SurfMomFluxY, &
         & xyr_RadLDwFlux, xyr_RadSDwFlux, &
         & xyr_RadLUwFlux, xyr_RadSUwFlux, &
         & xyr_RadLFluxA, xyr_RadSFlux, &
         & xyra_DelRadLUwFlux, &
         & xyr_HeatFlux, xyrf_QMixFlux, &
         & xy_Rain, xy_Snow, &
         & xy_SurfVelTransCoef, xy_SurfTempTransCoef, xy_SurfQVapTransCoef, &
         & xyz_DUDt, xyz_DVDt, xyz_DTempDtVDiff, xyzf_DQMixDt, xy_DSurfTempDt, &
         & xyz_Exner, xyr_Exner, xy_SurfHumidCoef, xy_SnowFrac, xyr_Press, &
         & xyra_DelRadLUwFlux, xyra_DelRadLDwFlux, &
         & xy_SurfTemp, xyz_TempN

    use saturate, only: &
      & xy_CalcQVapSatOnLiq,       &
      & xy_CalcQVapSatOnSol,       &
      & xy_CalcDQVapSatDTempOnLiq, &
      & xy_CalcDQVapSatDTempOnSol
    
    integer, intent(in) :: step

    real(DP), dimension(0:iMax-1,jMax) :: &
         & xy_TauXAtm, xy_TauYAtm, xy_SensAtm, xy_LatentAtm, xy_LDWRFlxAtm, xy_LUWRFlxAtm, &
         & xy_SurfQVapSatOnLiq, xy_SurfQVapSatOnSol, xy_SurfQVapSat, &
         & xy_SurfDQVapSatDTempOnLiq, xy_SurfDQVapSatDTempOnSol, xy_SurfDQVapSatDTemp, &
         & xy_DSurfHFlxDTs, xy_DSurfLatentFlxDTs, xy_SurfAirTemp

    integer :: p, j, m, n

    xy_SurfQVapSatOnLiq  = &
      & xy_CalcQVapSatOnLiq( xy_SurfTemp, xyr_Press(:,:,0) )
    xy_SurfQVapSatOnSol  = &
      & xy_CalcQVapSatOnSol( xy_SurfTemp, xyr_Press(:,:,0) )
    xy_SurfQVapSat       = &
      &   ( 1.0_DP - xy_SnowFrac ) * xy_SurfQVapSatOnLiq &
      & + xy_SnowFrac              * xy_SurfQVapSatOnSol
    xy_SurfDQVapSatDTempOnLiq = &
      & xy_CalcDQVapSatDTempOnLiq( xy_SurfTemp, xy_SurfQVapSatOnLiq )
    xy_SurfDQVapSatDTempOnSol = &
      & xy_CalcDQVapSatDTempOnSol( xy_SurfTemp, xy_SurfQVapSatOnSol )
    xy_SurfDQVapSatDTemp = &
      &   ( 1.0_DP - xy_SnowFrac ) * xy_SurfDQVapSatDTempOnLiq &
      & + xy_SnowFrac              * xy_SurfDQVapSatDTempOnSol

    xy_TauXAtm(:,:) = xy_SurfMomFluxX! - 0d0*xy_SurfVelTransCoef*xyz_DUDt(:,:,1)*2d0*delta_t
    xy_TauYAtm(:,:) = xy_SurfMomFluxY! - 0d0*xy_SurfVelTransCoef*xyz_DVDt(:,:,1)*2d0*delta_t
    xy_SensAtm(:,:) = xyr_HeatFlux(:,:,0)! - &
         !&    0d0*CpDry*xyr_Exner(:,:,0)*xy_SurfTempTransCoef&
         !& * (xyz_DTempDtVDiff(:,:,1)/xyz_Exner(:,:,1) - xy_DSurfTempDt/xyr_Exner(:,:,0)) &
         !& * 2d0*delta_t
    xy_LatentAtm(:,:) = LatentHeat*( &
         & xyrf_QMixFlux(:,:,0,IndexH2OVap) - &
         &   0d0*xy_SurfHumidCoef*xy_SurfQVapTransCoef*( &
         &     xyzf_DQMixDt(:,:,1,IndexH2OVap) - xy_SurfDQVapSatDTemp*xy_DSurfTempDt &
         &   )* 2d0*delta_t )

    xy_DSurfLatentFlxDTs(:,:) = LatentHeat*xy_SurfHumidCoef*xy_SurfQVapTransCoef*xy_SurfDQVapSatDTemp
    xy_DSurfHFlxDTs(:,:) = &
         &   CpDry*xy_SurfTempTransCoef  &
         & + xy_DSurfLatentFlxDTs        


    xy_SurfAirTemp(:,:) = xyr_Exner(:,:,0)/xyr_Exner(:,:,1)*xyz_TempN(:,:,1)

    xy_LDWRFlxAtm(:,:) = xyr_RadLDwFlux(:,:,0) + 0d0*2d0*delta_t*( &
         &    xy_DSurfTempDt * xyra_DelRadLDwFlux(:,:,0,0)            &
         & +  xyz_DTempDtVDiff(:,:,1) * xyra_DelRadLDwFlux(:,:,0,1)   &
         & )
    xy_LUWRFlxAtm(:,:) = xyr_RadLUwFlux(:,:,0) + 0d0*2d0*delta_t*( &
         &    xy_DSurfTempDt * xyra_DelRadLUwFlux(:,:,0,0)            &
         & +  xyz_DTempDtVDiff(:,:,1) * xyra_DelRadLUwFlux(:,:,0,1)   &
         & )
    
    call atm_set_send_2d(1, -xy_TauXAtm )
    call atm_set_send_2d(2, -xy_TauYAtm )    
    call atm_set_send_2d(3, -xy_SensAtm )
    call atm_set_send_2d(4, -xy_LatentAtm )
    call atm_set_send_2d(5, xyr_RadSDwFlux(:,:,0) )
    call atm_set_send_2d(6, xy_LDWRFlxAtm )    
    call atm_set_send_2d(7, xyr_RadSUwFlux(:,:,0) )
    call atm_set_send_2d(8, xy_LUWRFlxAtm )    
    call atm_set_send_2d(9, xy_Rain )
    call atm_set_send_2d(10, xy_Snow )
    call atm_set_send_2d(11, xy_DSurfHFlxDTs )
    call atm_set_send_2d(12, xy_SurfAirTemp )
    call atm_set_send_2d(13, xy_DSurfLatentFlxDTs )
!!$    call atm_set_send_2d(11, xy_SurfTempTransCoef)
!!$    call atm_set_send_2d(12, xy_SurfHumidCoef*xy_SurfQVapTransCoef)
    
!!$    if(my_rank==7) then
!!$       write(*,*) "---- tstep=", tstep, ",xyr_HeatFlux(0,1,0)=",sum(xyr_HeatFlux(:,1,0))/dble(iMax), ", lat=", y_Lat(1)*180/acos(-1d0)
!!$    end if
  contains
    subroutine atm_set_send_2d(varpID, send_data)
      integer, intent(in) :: varpID
      real(DP), intent(in) :: send_data(:,:)
      
      call jcup_put_data(field%varp(varpID)%varp_ptr, pack(send_data, MaSK=field%mask2d))
    end subroutine atm_set_send_2d
    
    subroutine atm_set_send(varpID, code)
      integer, intent(in) :: varpID, code
      
      call set_send_data_2d(ATM, ATM_GRID_2D, field%send_2d(:,:), step, code)
      call jcup_put_data(field%varp(varpID)%varp_ptr, pack(field%send_2d, Mask=field%mask2d))
    end subroutine atm_set_send
  end subroutine set_and_put_data

  subroutine get_and_write_data(tstep)

    use jcup_interface, only: &
         & jcup_get_data
    use field_def, only: write_data_2d

    use gridset, only: iMax, jMax
    
    integer, intent(in) :: tstep

    real(DP) :: OutputCurrentTime
    real(DP), dimension(0:iMax-1,jMax) :: xy_SurfTemp, xy_SurfAlbedo, xy_SurfSnow

    !
    OutputCurrentTime = RestartTimeSec + (tstep - 1)*delta_t
    
    ! Get oceanic surface temerature send by OGCM.
    !

    call atm_get_write(1, 'SurfTempOcn', xy_SurfTemp)
    call atm_get_write(2, 'SurfAlbedoOcn', xy_SurfAlbedo)
    call atm_get_write(3, 'SurfSnowOcn', xy_SurfSnow)

    !
    !
    if(mod((tstep-1)*delta_t, OCN_coupling_cycle) == 0) then

       call agcm_update_surfprop( &
            & xy_SurfTempRecv=xy_SurfTemp, xy_SurfAlbedoRecv=xy_SurfAlbedo, &
            & xy_SurfSnowRecv=1d3*xy_SurfSnow &
            & )
    end if

  contains
    subroutine atm_get_write(vargID, vargName, xy_getdata)
      integer, intent(in) :: vargID
      character(*), intent(In) :: vargname
      real(DP), intent(inout) :: xy_getdata(:,:)

      
      field%buffer1d(:) = 0d0
      call jcup_get_data(field%varg(vargID)%varg_ptr, field%buffer1d)
      
      field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)      
      call output_var(OutputCurrentTime, vargName, field%recv_2d)
      if(mod((tstep-1)*delta_t, OCN_coupling_cycle) == 0) xy_getdata(:,:) = field%recv_2d
    end subroutine atm_get_write
    
  end subroutine get_and_write_data

  !***************
  
  subroutine output_prepare()
    use dc_types
    use gtool_historyauto

    character(TOKEN) :: dims_XYT(3)

    dims_XYT = (/ 'lon ', 'lat ', 'time'  /)
    call HistoryAutoAddVariable('SurfTempOcn', &
         & dims=dims_XYT, longname='surface temperature calculated ocean and sea-ice model', units='K') 

    call HistoryAutoAddVariable('SurfAlbedoOcn', &
         & dims=dims_XYT, longname='surface albedo calculated by ocean and sea-ice model', units='1') 

    call HistoryAutoAddVariable('SurfSnowOcn', &
         & dims=dims_XYT, longname='surface snode depth  calculated by ocean and sea-ice model', units='m') 
    
    call HistoryAutoAddVariable('CheckVar', &
         & dims=dims_XYT, longname='CheckVar', units='1') 

  end subroutine output_prepare

  subroutine output_var(CurrentTime, varname, data2d)
    use dc_types
    use gtool_historyauto
    real(DP), intent(in) :: CurrentTime
    character(*), intent(in) :: varName
    real(DP), intent(in) :: data2d(:,:)

    call HistoryAutoPut(CurrentTime, varname, data2d)
  end subroutine output_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine gen_grid_index()

    !* DCPAM5
    use gridset, only: a_jmax

    
    integer :: i, j, g_js
    integer :: counter
    
    g_js = sum(a_jmax)/2 - sum(a_jmax(0:my_rank))/2

!    write(*,*) "ATM grid_index: (rank=", my_rank, ") :: jc", jc, "a_jmax=", a_jmax, "lb;", lbound(a_jmax)    write(*,*) "ATM grid_index: (rank=", my_rank, ") :: g_js", g_js
    counter = 0
    do j=1, a_jmax(my_rank)/2
       do i=1, GNXA
          counter = counter + 1          
          field%grid_index(counter) = i + (g_js + j - 1)*GNXA
       end do
    end do

    g_js = sum(a_jmax)/2
    if(my_rank > 0) g_js = g_js + sum(a_jmax(0:my_rank-1))/2
    do j=1, a_jmax(my_rank)/2
       do i=1, GNXA
          counter = counter + 1          
          field%grid_index(counter) = i + (g_js + j - 1)*GNXA
       end do
    end do

    write(*,*) "ATM grid_index: (rank=", my_rank, ") ::", field%grid_index
  end subroutine gen_grid_index
  
end module mod_atm

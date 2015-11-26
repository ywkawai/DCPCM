!-------------------------------------------------------------
! Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
!-------------------------------------------------------------
!> @brief a template module
!! 
!! @author Kawai Yuta
!!
!!
module mod_ocn

  ! モジュール引用; Use statements
  !

  !* gtool
  
  use dc_types, only: &
       & DP
  
  use dc_message, only: &
       & MessageNotify

  !* Dennou-OGCM
  
  use DSOGCM_main_mod, only: &
       & ogcm_main_Init => DSOGCM_main_Init, &
       & ogcm_main_Final => DSOGCM_main_Final, &
       & ogcm_setup => DSOGCM_main_setup, &
       & ogcm_shutdown => DSOGCM_main_shutdown, &
       & ogcm_advance_timestep => DSOGCM_main_advance_timestep

  !* DCPCM
  
  use field_common, only: &
       & OCN, OCN_GRID, GNXO, GNYO, GN25, JCUP_LOG_LEVEL
  
  use component_field, only: &
       & component_field_type

#include "../common/Component_def.h"
  
  ! 宣言文; Declareration statements
  !  
  implicit none
  private


  ! 公開手続き
  ! Public procedure
  !  
  public :: ocn_init
  public :: ocn_run
  public :: ocn_fin


  ! 非公開変数
  ! Private variable
  !
  
  integer :: itime(6)
  real(DP) :: RestartTimeSec
  integer :: delta_t = 180

  integer :: DIV_X, DIV_Y
  integer :: my_comm, my_group, my_size, my_rank

  type(component_field_type) :: field

  integer :: ATM_coupling_cycle
contains

  subroutine ocn_init()

    !
    !
    use jcup_interface, only: &
         & jcup_set_new_comp, jcup_initialize, jcup_get_mpi_parameter, &
         & jcup_def_grid, jcup_end_grid_def, &
         & jcup_def_varp, jcup_def_varg, jcup_end_var_def, &
         & jcup_set_mapping_table, &
         & jcup_init_time
    use jcup_mpi_lib, only: jml_finalize
         
    use field_def, only : init_field_def, set_field_def, cal_mn, get_local_field, cal_grid_index, &
         set_grid_mapping, set_grid_mapping_3d
    
    use component_field, only : init_field, init_field_data

    use field_common, only: &
         & ATM, ATM_GRID_2D, GNXA, GNYA, &
         & JCUP_LOG_STDERROR

    use interpolation_data_latlon_mod, only: &
         & init_interpolation => interpolation_data_latlon_Init, &
         & set_operation_index, &
         & set_A_to_O_coef, set_O_to_A_coef

    use mpi

    use TemporalIntegSet_mod, only: &
         & InitDate, RestartTime
    
    
    !
    !
    integer, allocatable :: grid_index(:)
    integer :: lis, lie, ljs, lje
    integer :: i

    logical :: loop_end_flag
    
    !
    !

    
    ! Initialize coupler
    !
    call MessageNotify('M', 'ocn component', &
         & 'Initialize coupler..')
    
    call jcup_set_new_comp(OCN)
    call jcup_initialize(OCN, LOG_LEVEL=JCUP_LOG_LEVEL, LOG_STDERR = JCUP_LOG_STDERROR)

    call jcup_get_mpi_parameter(OCN, my_comm, my_group, my_size, my_rank)

    call MessageNotify('M', 'ocnx component', &
         & 'Initialize Dennou-OGCM .. (MPI_MY_COMM=%d, MY_RANK=%d)', &
         & i=(/my_comm, my_rank/))

    call ogcm_main_Init()
    call ogcm_setup()

    !
    itime = (/ InitDate%year, InitDate%month, InitDate%day, InitDate%hour, InitDate%min, int(InitDate%sec) /)
    RestartTimeSec = RestartTime
    delta_t = OGCM_DELTIME
    ATM_coupling_cycle = ATMOCN_COUPLING_CYCLE
    
    !
    !
    
    call init_interpolation(2, 1, 2)

    !call cal_mn(my_size, DIV_X, DIV_Y)
    DIV_X = 1; DIV_Y = my_size
    write(*,*) "OCN: rank=", my_rank, "DIV_X,Y=", DIV_X, DIV_Y, "mysize=", my_size
    
    call init_field_def(1)
    call set_field_def(component_name=OCN, grid_name=OCN_GRID, &
         & g_nx=GNXO, g_ny=GNYO, g_nz=1, hallo=2, div_x=DIV_X, div_y=DIV_Y)
    call get_local_field(component_name=OCN, grid_name=OCN_GRID, &
         & local_is=lis, local_ie = lie, local_js=ljs, local_je=lje)
    write(*,*) "OCN: rank=", my_rank, "li=", lis, lie, " ,lj=", ljs, lje

    !
    !
    call init_field(field, OCN_GRID, lis, lie, ljs, lje, 1, 1) ! halo=1

    call cal_grid_index(OCN, OCN_GRID, field%grid_index)
    !   write(*,*) "OCN: grid_index=", field%grid_index
    
    call jcup_def_grid(field%grid_index, OCN, OCN_GRID)

    call jcup_end_grid_def()

    !
    !

    call init_field_data(field, num_of_25d=GN25, num_of_varp=3, num_of_varg=13)

    call jcup_def_varp(field%varp(1)%varp_ptr, OCN, "SurfTempOcn", OCN_GRID)
    call jcup_def_varp(field%varp(2)%varp_ptr, OCN, "SurfAlbedoOcn", OCN_GRID)
    call jcup_def_varp(field%varp(3)%varp_ptr, OCN, "SurfSnowOcn", OCN_GRID)
    
    call jcup_def_varg(field%varg(1)%varg_ptr, OCN, "TauXAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="TauXAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    call jcup_def_varg(field%varg(2)%varg_ptr, OCN, "TauYAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="TauYAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    call jcup_def_varg(field%varg(3)%varg_ptr, OCN, "SensFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SensFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(4)%varg_ptr, OCN, "LatentFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="LatentFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(5)%varg_ptr, OCN, "SWDWRFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SWDWRFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(6)%varg_ptr, OCN, "LWDWRFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="LWDWRFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(7)%varg_ptr, OCN, "SWUWRFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SWUWRFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(8)%varg_ptr, OCN, "LWUWRFlxAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="LWUWRFlxAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(9)%varg_ptr, OCN, "RainAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="RainAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=3)
    call jcup_def_varg(field%varg(10)%varg_ptr, OCN, "SnowAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SnowAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=3)
    call jcup_def_varg(field%varg(11)%varg_ptr, OCN, "DSurfFlxDTsAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="DSurfHFlxDTsAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=4)
    call jcup_def_varg(field%varg(12)%varg_ptr, OCN, "SurfAirTempAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SurfAirTempAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=4)
    call jcup_def_varg(field%varg(13)%varg_ptr, OCN, "DSurfLatentFlxDTsAtm", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="DSurfLatentFlxDTsAtm", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=4)
    
    call jcup_end_var_def()

    !
    !

    ! ATM -> OCN grid mapping    **************************
    call jcup_set_mapping_table(OCN, ATM, ATM_GRID_2D, OCN, OCN_GRID, 1)
    call set_operation_index(OCN, ATM, 1)
    
    ! OCN -> ATM grid mapping   *****************************        
    call jcup_set_mapping_table(OCN, OCN, OCN_GRID, ATM, ATM_GRID_2D, 1)


    !
    !
    call set_A_to_O_coef(1)
    call set_O_to_A_coef(1)

    
    !
    !
    
    write(*,*) "ocn: rank=", my_rank, "init_time=(/", itime, "/)", &
         & "RestartTimeSec=", RestartTimeSec
    call jcup_init_time(itime)

    call output_prepare()

    call ogcm_advance_timestep(0, loop_end_flag)
    call set_and_put_data(0)
    
  end subroutine ocn_init

  subroutine ocn_run(loop_flag)

    use jcup_interface, only: &
         & jcup_set_time, jcup_inc_time
    
    logical, intent(inout) :: loop_flag

    integer :: tstep
    logical :: loop_end_flag

    tstep = 1; loop_end_flag = .false.
    do while(.not. loop_end_flag)
       call jcup_set_time(OCN, itime, delta_t)

!!$       write(*,*) "* COUPLER Get: ocn my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t       
       call get_and_write_data(tstep)

       if(my_rank==0 .or. JCUP_LOG_LEVEL>=1) then
          write(*,*) "-> ocn my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t
       end if
       call ogcm_advance_timestep(tstep, loop_end_flag)
!!$       write(*,*) "<- ocn my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t

!!$       write(*,*) "* COUPLER Put: ocn my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t              
       call set_and_put_data(tstep)

       
       call jcup_inc_time(OCN, itime)
       tstep = tstep + 1

!!$       if(tstep == (24/1) * 365 * 30 + 1) loop_end_flag = .true.
!       if(tstep == (24/3) * 181 + 1) loop_end_flag = .true.       
       if(tstep == (24/3) * 731 + 1) loop_end_flag = .true.       

    end do

    loop_flag = .false.
  end subroutine ocn_run

  subroutine set_and_put_data(step)

    !* JCup
    use jcup_interface, only: &
         & jcup_put_data
    use field_def, only: &
         & set_send_data_2d

    !* Dennou-OGCM
    use UnitConversion_mod, only: &
         & degC2K
    
    use GridSet_mod, only: &
         & iMax, jMax

    use VariableSet_mod, only: &
         & xyz_PTempEddN, z_PTempBasic

    !* Sea-ice
    use SeaIceConstants_mod, only: &
         & AlbedoSnow, AlbedoIce, AlbedoOcean
    
    use VarSetSeaice_mod, only: &
         & xy_SIceSurfTempN, xy_SnowThickN, xy_IceThickN
    
    integer, intent(in) :: step
    real(DP), dimension(0:iMax-1,jMax) :: xy_SurfTemp, xy_SurfAlbedo

    where(xy_SnowThickN > 0d0)
       xy_SurfTemp = degC2K(xy_SIceSurfTempN)
       xy_SurfAlbedo = AlbedoSnow
    elsewhere(xy_IceThickN > 0d0)
       xy_SurfTemp = degC2K(xy_SIceSurfTempN)
       xy_SurfAlbedo = AlbedoIce
    elsewhere
       xy_SurfTemp = z_PTempBasic(0) + xyz_PTempEddN(:,:,0)
       xy_SurfAlbedo = AlbedoOcean
    end where
    
    call jcup_put_data(field%varp(1)%varp_ptr, pack(xy_SurfTemp, mask=field%mask2d))
    call jcup_put_data(field%varp(2)%varp_ptr, pack(xy_SurfAlbedo, mask=field%mask2d))
    call jcup_put_data(field%varp(3)%varp_ptr, pack(xy_SnowThickN, mask=field%mask2d))
    
  end subroutine set_and_put_data

  subroutine get_and_write_data(tstep)

    use jcup_interface, only: &
         & jcup_get_data
    use field_def, only: write_data_2d

    use Constants_mod, only: &
         & LatentHeat
    
    use GridSet_mod, only: &
         & iMax, jMax, lMax, xyz_Lat

    use BoundaryCondO_mod, only: &
         & xy_WindStressU, xy_WindStressV, &
         & xy_SWDWRFlx, xy_SWUWRFlx, xy_LWDWRFlx, xy_LWUWRFlx, &
         & xy_SensDWHFlx, xy_LatentDWHFlx, &
         & xy_Wrain, xy_Wsnow, xy_Wevap, &
         & xy_DSurfHFlxDTs, xy_DSurfLatentFlxDTs, xy_SurfAirTemp

    use VariableSet_mod, only: &
         & xyz_PTempEddN, z_PTempBasic

    use SpmlUtil_mod, only: &
         & xy_w, w_xy, &
         & w_VorDiv2VectorCosLat, w_VectorCosLat2VorDiv_2
    
    integer, intent(in) :: tstep

    real(DP), parameter :: DensFreshWater = 1d3
    
    real(DP) :: xy_Dummy(0:iMax-1,jMax)
    real(DP) :: OutputCurrentTime

!!$    real(DP), dimension(0:iMax-1,jMax) :: &
!!$         & xy_SWRFlx, xy_LWRFlx, xy_SensHFlx, xy_LatentHFlx
    
    !
    if(tstep == 1) then
       call MessageNotify('M', 'ocn component..', &
            & 'tstep=1. !The coupler skip to set data of surface fluxes which are sent by AGCM .. !' &
            & )
       return
    end if
    
    OutputCurrentTime = RestartTimeSec + (tstep - 1)*delta_t 
    
    call ocn_get_write(1, 'TauXAtm', xy_WindStressU)
    call ocn_get_write(2, 'TauYAtm', xy_WindStressV)
    call ocn_get_write(3, 'SensFlxAtm', xy_SensDWHFlx)
    call ocn_get_write(4, 'LatentFlxAtm', xy_LatentDWHFlx)
    call ocn_get_write(5, 'SWDWRFlxAtm', xy_SWDWRFlx)
    call ocn_get_write(6, 'LWDWRFlxAtm', xy_LWDWRFlx)
    call ocn_get_write(7, 'SWUWRFlxAtm', xy_SWUWRFlx)
    call ocn_get_write(8, 'LWUWRFlxAtm', xy_LWUWRFlx)    
    call ocn_get_write(9, 'RainAtm', xy_Wrain)    
    call ocn_get_write(10, 'SnowAtm', xy_Wsnow)
    call ocn_get_write(11, 'DSurfHFlxDTsAtm', xy_DSurfHFlxDTs)
    call ocn_get_write(12, 'SurfAirTempAtm', xy_SurfAirTemp)
    call ocn_get_write(13, 'DSurfLatentFlxDTsAtm', xy_DSurfLatentFlxDTs)
    
    !
    !$omp parallel workshare
!    xy_SurfHFlx_ns(:,:) = xy_SensHFlx + xy_LatentHFlx + xy_LWRFlx
!    xy_SurfHFlx_sr(:,:) = xy_SWRFlx
    xy_Wrain(:,:) = xy_Wrain/DensFreshWater
    xy_Wsnow(:,:) = xy_Wsnow/DensFreshWater
    xy_Wevap(:,:) = - (xy_LatentDWHFlx/LatentHeat)/DensFreshWater
    !$omp end parallel workshare
    
!!$    call output_var(OutputCurrentTime, 'EvapAtm', xy_Wevap)
!!$    call output_var(OutputCurrentTime, 'FwFlxAtm', xy_Wrain + xy_Wsnow - xy_Wevap)
    
  contains
    subroutine ocn_get_write(vargID, vargName, xy_getdata)
      integer, intent(in) :: vargID
      character(*), intent(in) :: vargname
      real(DP), intent(inout) :: xy_getdata(:,:)
      
      field%buffer1d(:) = 0d0
      call jcup_get_data(field%varg(vargID)%varg_ptr, field%buffer1d)
      
      field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
      call output_var(OutputCurrentTime, vargName, field%recv_2d)
      if(mod((tstep-1)*delta_t, ATM_coupling_cycle) == 0) xy_getdata(:,:) = field%recv_2d
    end subroutine ocn_get_write
    
  end subroutine get_and_write_data
  
  subroutine ocn_fin()
    use jcup_interface, only: &
         & jcup_coupling_end

    write(*,*) 'ocn fin: my_rank=', my_rank
    call ogcm_shutdown()
    call ogcm_main_Final()
    call jcup_coupling_end(itime, .true.)
    write(*,*) '-- ocn fin: my_rank=', my_rank    
    
  end subroutine ocn_fin

  !***************
  
  subroutine output_prepare()
    use dc_types
    use gtool_historyauto

    character(TOKEN) :: dims_XYT(3)

    dims_XYT = (/ 'lon ', 'lat ', 'time' /)
    
    call HistoryAutoAddVariable('TauXAtm', &
         & dims=dims_XYT, longname='surface momentum flux', units='kg.m-1.s-2')

    call HistoryAutoAddVariable('TauYAtm', &
         & dims=dims_XYT, longname='surface momentum flux', units='kg.m-1.s-2')

    call HistoryAutoAddVariable('SensFlxAtm', &
         & dims=dims_XYT, longname='SensFlx', units='W.m-2')
    
    call HistoryAutoAddVariable('LatentFlxAtm', &
         & dims=dims_XYT, longname='LatentFlxAtm', units='W.m-2')
    
    call HistoryAutoAddVariable('LWDWRFlxAtm', &
         & dims=dims_XYT, longname='downward long wave radiation', units='W.m-2')
    
    call HistoryAutoAddVariable('SWDWRFlxAtm', &
         & dims=dims_XYT, longname='downward short wave radiation', units='W.m-2')

    call HistoryAutoAddVariable('LWUWRFlxAtm', &
         & dims=dims_XYT, longname='upward long wave radiation', units='W.m-2')
    
    call HistoryAutoAddVariable('SWUWRFlxAtm', &
         & dims=dims_XYT, longname='upward short wave radiation', units='W.m-2')
    
    call HistoryAutoAddVariable('DSurfHFlxDTsAtm', &
         & dims=dims_XYT, longname='DSurfHFlxDTsAtm', units='W.m-2.K-1')

    call HistoryAutoAddVariable('DSurfLatentFlxDTsAtm', &
         & dims=dims_XYT, longname='DSurfLatentFlxDTsAtm', units='W.m-2.K-1')
    
    call HistoryAutoAddVariable('RainAtm', &
         & dims=dims_XYT, longname='rain', units='kg.m-2.s-1')
    call HistoryAutoAddVariable('SnowAtm', &
         & dims=dims_XYT, longname='snow', units='kg.m-2.s-1')
    call HistoryAutoAddVariable('EvapAtm', &
         & dims=dims_XYT, longname='Evaporation at surface', units='kg.m-2.s-1')

    call HistoryAutoAddVariable('FwFlxAtm', &
         & dims=dims_XYT, longname='Fresh water flux', units='m.s-1')

    call HistoryAutoAddVariable('SurfAirTempAtm', &
         & dims_XYT, longname='surface air temperaturet', units='K')
    
  end subroutine output_prepare

  subroutine output_var(CurrentTime, varname, data2d)
    use dc_types
    use gtool_historyauto
    real(DP), intent(in) :: CurrentTime
    character(*), intent(in) :: varName
    real(DP), intent(in) :: data2d(:,:)

    call HistoryAutoPut(CurrentTime, varname, data2d)
  end subroutine output_var
  
end module mod_ocn

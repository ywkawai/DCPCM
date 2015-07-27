module mod_ocn

  use dc_message, only: &
       & MessageNotify
  
  use field_common, only: &
       & OCN, OCN_GRID, GNXO, GNYO, GN25
  
  use component_field, only: &
       & component_field_type

  use DSOGCM_main_mod, only: &
       & ogcm_main_Init => DSOGCM_main_Init, &
       & ogcm_main_Final => DSOGCM_main_Final, &
       & ogcm_setup => DSOGCM_main_setup, &
       & ogcm_shutdown => DSOGCM_main_shutdown, &
       & ogcm_advance_timestep => DSOGCM_main_advance_timestep
  
  implicit none
  
  private

  public :: ocn_init
  public :: ocn_run
  public :: ocn_fin

  integer :: itime(6)
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
         & ATM, ATM_GRID_2D, GNXA, GNYA

    use interpolation_data_latlon_mod, only: &
         & init_interpolation => interpolation_data_latlon_Init, &
         & set_operation_index, &
         & set_A_to_O_coef, set_O_to_A_coef

    use mpi
    
    !
    !
    integer, allocatable :: grid_index(:)
    integer :: lis, lie, ljs, lje
    integer :: i

    logical :: loop_end_flag
    
    !
    !

    itime = (/ 2000, 1, 1, 0, 0, 0 /)
    delta_t = 3600
    ATM_coupling_cycle = 3600
    
    ! Initialize coupler
    !
    call MessageNotify('M', 'ocn component', &
         & 'Initialize coupler..')
    
    call jcup_set_new_comp(OCN)
    call jcup_initialize(OCN, LOG_LEVEL=2, LOG_STDERR = .true.)

    call jcup_get_mpi_parameter(OCN, my_comm, my_group, my_size, my_rank)

    call MessageNotify('M', 'ocnx component', &
         & 'Initialize Dennou-OGCM .. (MPI_MY_COMM=%d, MY_RANK=%d)', &
         & i=(/my_comm, my_rank/))

    call ogcm_main_Init()
    call ogcm_setup()
    
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

    call init_field_data(field, num_of_25d=GN25, num_of_varp=1, num_of_varg=8)

    call jcup_def_varp(field%varp(1)%varp_ptr, OCN, "OSST", OCN_GRID)
    
    call jcup_def_varg(field%varg(1)%varg_ptr, OCN, "TAUX", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="TAUX", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    call jcup_def_varg(field%varg(2)%varg_ptr, OCN, "TAUY", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="TAUY", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=1)
    call jcup_def_varg(field%varg(3)%varg_ptr, OCN, "SENSFLX", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SENSFLX", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(4)%varg_ptr, OCN, "LATENTFLX", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="LATENTFLX", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(5)%varg_ptr, OCN, "SWDWRFLX", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SWDWRFLX", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(6)%varg_ptr, OCN, "LWDWRFLX", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="LWDWRFLX", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=2)
    call jcup_def_varg(field%varg(7)%varg_ptr, OCN, "RAIN", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="RAIN", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=3)
    call jcup_def_varg(field%varg(8)%varg_ptr, OCN, "SNOW", OCN_GRID, 1, &
         & SEND_MODEL_NAME=ATM, SEND_DATA_NAME="SNOW", &
         & RECV_MODE="AVR", INTERVAL=ATM_coupling_cycle, TIME_LAG=-1, MAPPING_TAG=1, EXCHANGE_TAG=3)

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
    write(*,*) "ocn: rank=", my_rank, "init_time.."
    call jcup_init_time(itime)
    call ogcm_advance_timestep(0, loop_end_flag)
    call set_and_put_data(0)

    call output_prepare()
    
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

       call get_and_write_data(tstep)

       call ogcm_advance_timestep(tstep, loop_end_flag)
       call set_and_put_data(tstep)
       write(*,*) "ocn my_rank=", my_rank, "tstep=", tstep, "time=", tstep*delta_t
       
       call jcup_inc_time(OCN, itime)
       tstep = tstep + 1

       if(tstep == 24 * 365 + 1) loop_end_flag = .true.
    end do

    loop_flag = .false.
  end subroutine ocn_run

  subroutine set_and_put_data(step)
    use jcup_interface, only: &
         & jcup_put_data
    use field_def, only: &
         & set_send_data_2d

    use GridSet_mod, only: &
         & xyz_Lat
    use VariableSet_mod, only: &
         & xyz_PTempEddN, z_PTempBasic
    
    integer, intent(in) :: step

!    call set_send_data_2d(OCN, OCN_GRID, z_PTempBasic(0)+xyz_PTempEddA(:,:,0), step, 1)
    call jcup_put_data(field%varp(1)%varp_ptr, pack(z_PTempBasic(0)+xyz_PTempEddN(:,:,0), MaSK=field%mask2d))
!    call jcup_put_data(field%varp(1)%varp_ptr, pack(xyz_Lat(:,:,0)*180d0/acos(-1d0), MaSK=field%mask2d))
    
  end subroutine set_and_put_data

  subroutine get_and_write_data(tstep)
    use jcup_interface, only: &
         & jcup_get_data
    use field_def, only: write_data_2d

    integer, intent(in) :: tstep
    
    call ocn_get_write(1, 'TAUX')
    call ocn_get_write(2, 'TAUY')
    call ocn_get_write(3, 'SENSFLX')
    call ocn_get_write(4, 'LATENTFLX')
    call ocn_get_write(5, 'SWDWRFLX')
    call ocn_get_write(6, 'LWDWRFLX')
    call ocn_get_write(7, 'RAIN')    
    call ocn_get_write(8, 'SNOW')

  contains
    subroutine ocn_get_write(vargID, vargName)
      integer, intent(in) :: vargID
      character(*), intent(In) :: vargname

      field%buffer1d(:) = 0d0
      call jcup_get_data(field%varg(vargID)%varg_ptr, field%buffer1d)
      field%recv_2d = unpack(field%buffer1d, field%mask2d, field%recv_2d)
      call write_data_2d(OCN, OCN_GRID, vargName, field%recv_2d)

      call output_var((tstep-1)*3600d0, vargName, field%recv_2d)
      
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

    dims_XYT = (/ 'lon', 'lat', 't  ' /)
    
    call HistoryAutoAddVariable('TAUX', &
         & dims=dims_XYT, longname='surface momentum flux', units='kg.m-1.s-2')

    call HistoryAutoAddVariable('TAUY', &
         & dims=dims_XYT, longname='surface momentum flux', units='kg.m-1.s-2')

    call HistoryAutoAddVariable('SENSFLX', &
         & dims=dims_XYT, longname='SensFlx', units='W.m-2')
    
    call HistoryAutoAddVariable('LATENTFLX', &
         & dims=dims_XYT, longname='LatentFlx', units='W.m-2')
    
    call HistoryAutoAddVariable('LWDWRFLX', &
         & dims=dims_XYT, longname='RadLDWFlx', units='W.m-2')
    
    call HistoryAutoAddVariable('SWDWRFLX', &
         & dims=dims_XYT, longname='RadSDWFlx', units='W.m-2')
    
    call HistoryAutoAddVariable('RAIN', &
         & dims=dims_XYT, longname='rain', units='kg.m-2.s-1')
    call HistoryAutoAddVariable('SNOW', &
         & dims=dims_XYT, longname='snow', units='kg.m-2.s-1')
    
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

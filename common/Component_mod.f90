!-------------------------------------------------------------
! Copyright (c) 2013-2015 Yuta Kawai. All rights reserved.
!-------------------------------------------------------------
!> @brief a template module
!! 
!! @author Yuta Kawai
!!
!!
module Component_mod 

  ! モジュール引用; Use statements
  !
  use dc_types, only: &
       & DP, TOKEN, STRING
  
  ! 宣言文; Declareration statements
  !
  implicit none
  private

  ! 公開手続き
  ! Public procedure
  !
  public :: Component_Init, Component_Final
  public :: Component_GetGrid
  public :: Component_PrintInfo
  public :: Component_Jcup_init
  
  type, public :: CompGrid
     integer :: GNX, GNY, GNZ
     integer :: LNX, LNY, LNZ
     integer, pointer :: grid_index(:)
     character(TOKEN) :: name
  end type CompGrid
  
  type, public  :: Component
     character(TOKEN) :: name
     integer :: id
     integer :: itime(6)
     type(CompGrid), pointer :: grid(:)

     ! MPI information
     integer :: my_rank, my_comm, my_size, my_group
     
  end type Component

  ! 非公開手続き
  ! Private procedure
  !

  ! 非公開変数
  ! Private variable
  !
  character(*), parameter:: module_name = 'Component_mod' !< Module Name

  integer, public :: COMPGRID2D_ID = 1
  integer, public :: COMPGRID3D_ID = 2
  
contains

  
  !>
  !!
  !!
  subroutine Component_Init(this, comp_name, comp_id, GNX, GNY, GNZ)

    type(Component), intent(inout) :: this
    character(*), intent(in) :: comp_name
    integer, intent(in) :: comp_id
    integer, intent(in) :: GNX, GNY, GNZ
    
    ! 実行文; Executable statements
    !

    !
    this%name = comp_name
    this%id = comp_id
    
    !
    !
    
    allocate(this%grid(2))

    !
    this%grid(1)%name = trim(this%name) // "_2D_grid"
    this%grid(1)%GNX = GNX
    this%grid(1)%GNY = GNY
    this%grid(1)%GNZ = 1

    !
    this%grid(2)%name = trim(this%name) // "_3D_grid"
    this%grid(2)%GNX = GNX
    this%grid(2)%GNY = GNY
    this%grid(2)%GNZ = GNZ
    
  end subroutine Component_Init

  !>
  !!
  !!
  subroutine Component_Final(this)
    type(Component), intent(inout) :: this
    
    ! 実行文; Executable statements
    !

  end subroutine Component_Final

  subroutine Component_GetGrid(this, gridID, grid)
    type(Component), intent(in) :: this
    integer, intent(in) :: gridId
    type(CompGrid), pointer, intent(out) :: grid

    grid => this%grid(gridID)
    
  end subroutine Component_GetGrid

  subroutine Component_Jcup_init(this, default_time_unit, log_level)
    use jcup_interface, only: &
         & jcup_set_new_comp, jcup_initialize, &
         & jcup_get_mpi_parameter

    type(Component), intent(inout) :: this
    character(*), intent(in), optional :: default_time_unit
    integer, intent(in), optional :: log_level

    character(TOKEN) :: default_time_unit_
    integer :: log_level_

    default_time_unit_ = 'SEC'
    log_level_ = 0

    if(present(default_time_unit)) default_time_unit_ = default_time_unit
    if(present(log_level)) log_level_ = log_level
    
    call jcup_set_new_comp(trim(this%name))
    call jcup_initialize(trim(this%name), default_time_unit_, log_level_)
    call jcup_get_mpi_parameter(trim(this%name), &
         & this%my_comm, this%my_group, this%my_size, this%my_rank)
    
  end subroutine Component_Jcup_init

  subroutine Component_Jcup_def_grid(this)
    use jcup_interface, only: &
         & jcup_def_grid, jcup_end_grid_def
    
    type(Component), intent(in) :: this

    integer, pointer :: grid2d_index(:)

    grid2d_index => null()
    call calc_grid_index_2D(this%grid(COMPGRID2D_ID), grid2d_index)
    call jcup_def_grid(grid2d_index, trim(this%name), this%grid(COMPGRID2D_ID)%name)

  end subroutine Component_Jcup_def_grid
  
  subroutine Component_PrintInfo(this)
    use dc_message, only: MessageNotify
    type(Component), intent(in) :: this

    call MessageNotify('M', module_name, &
         & 'name=%a, id=%d, mpi_rank=%d', &
         & ca=(/ trim(this%name) /), i=(/this%id, this%my_rank /) &
         & )
    
  end subroutine Component_PrintInfo

  subroutine calc_grid_index_2D(this, grid_index)
    type(CompGrid), intent(in) :: this
    integer, pointer, intent(inout) :: grid_index(:)

    integer :: i, j, n
    
    if(associated(grid_index)) deallocate(grid_index)

    allocated(grid_index(this%GNX*this%GNY))

    do j=1, this%GNY
       do i=1, this%GNX
          n = i+(j-1)*this%GNY
          grid_index(n) = n
       end do
    end do
    
  end subroutine calc_grid_index_2D
  
end module Component_mod


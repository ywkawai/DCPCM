!-------------------------------------------------------------
! Copyright (c) 2013-2015 Yuta Kawai. All rights reserved.
!-------------------------------------------------------------
!> @brief a template module
!! 
!! @author Yuta Kawai
!!
!!
module DCPCMComp_mod 

  ! モジュール引用; Use statements
  !

  !* gtool5

  use dc_types, only: DP, TOKEN, STRING

  use dc_message, only: MessageNotify

  !* JCup
  use component_field
  
  ! 宣言文; Declareration statements
  !
  implicit none
  private

  ! 公開手続き
  ! Public procedure
  !
  type, public :: DCPCMComp

     character(TOKEN) :: name
     integer :: id
     integer :: itime(6)
     type(component_field_type), pointer :: fields(:)

     integer :: mpi_rank
     integer :: mpi_comm
     integer :: mpi_size
     integer :: mpi_group
     
  end type DCPCMComp

  public :: DCPCMComp_Init, DCPCMComp_Final
  public :: DCPCMComp_Print
  
  ! 非公開手続き
  ! Private procedure
  !

  ! 非公開変数
  ! Private variable
  !
  character(*), parameter:: module_name = 'DCPCMComp_mod' !< Module Name

contains

  !>
  !!
  !!
  subroutine DCPCMComp_Init(this, &
       & name, log_level, log_stderr    &
    )

    use jcup_interface, only: &
         & jcup_set_new_comp, jcup_initialize, &
         & jcup_get_mpi_parameter

    type(DCPCMComp), intent(inout) :: this
    character(*), intent(in) :: name
    integer, intent(in), optional :: log_level
    logical, intent(in), optional :: log_stderr
    
    ! 実行文; Executable statements
    !

    call MessageNotify('M', module_name, &
         & "A component '%a' in DCPCM is initialized.", &
         & ca=(/ trim(this%name) /) )

    this%name = name

    ! Initialziation for JCup

    call jcup_set_new_comp(this%name)
    call jcup_initialize(this%name, &
         & LOG_LEVEL=log_level, LOG_STDERR=log_stderr)

    !
    call jcup_get_mpi_parameter(this%name, &
         & this%mpi_comm, this%mpi_group, this%mpi_size, this%mpi_rank)

  end subroutine DCPCMComp_Init

  !>
  !!
  !!
  subroutine DCPCMComp_Final(this, MPIShutdownFlag)

    use jcup_interface, only: &
         & jcup_coupling_end
    
    ! 実行文; Executable statements
    !
    type(DCPCMComp), intent(inout) :: this
    logical, intent(in), optional :: MPIShutdownFlag

    call jcup_coupling_end(this%itime, MPIShutdownFlag)

    call MessageNotify('M', module_name, &
         & "A component of DCPCM (name='%a', rank=%d) has been finalized.", &
         & ca=(/ this%name /), i=(/ this%mpi_rank /))
    
  end subroutine DCPCMComp_Final

!!!!!!!!!!!!!!!!!!!

  !> @brief 
  !!
  !!
  subroutine DCPCMComp_Print(this)
    
    ! 宣言文; Declaration statement
    !
    type(DCPCMComp), intent(in) :: this
    
    ! 局所変数
    ! Local variables
    !
    
    
    ! 実行文; Executable statement
    !

    call MessageNotify('M', module_name // "::Print()", "------------------")
    call MessageNotify('M', module_name, &
         & "name=%a, ", &
         & ca=(/ this%name /) &
         & )
    call MessageNotify('M', module_name, &
         & "MPI information: comm=%d, group=%d, rank=%d, size=%d", &
         & i=(/ this%mpi_comm, this%mpi_group, this%mpi_rank, this%mpi_size /) &
         & )
    
  end subroutine DCPCMComp_Print

  
end module DCPCMComp_mod


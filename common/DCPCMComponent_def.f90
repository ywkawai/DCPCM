module DCPCMComponent_def

  !* gtool5

  use dc_types, only: DP, TOKEN, STRING

  use dc_message, only: MessageNotify
  
  !* Jcup
  use field_def
  
  implicit none
  private

  type, public  :: Component
     character(TOKEN) :: name
     integer :: id
     integer :: itime(6)
     type(component_field), pointer :: fields(:)

     ! MPI information
     integer :: my_rank, my_comm, my_size, my_group
     
  end type 

  public :: Component_Init, Component_Final
  public :: Component_GetGrid
  public :: Component_PrintInfo
  public :: Component_Jcup_init
  
  
contains
  !> @brief 
  !!
  !!
  subroutine DCPCMComp_Init(this, name, nCompField)
    
    ! 宣言文; Declaration statement
    !
    type(DCPCMComp), intent(inout) :: this
    character(*), intent(in) :: name
    integer, intent(in) :: nCompField
    
    ! 局所変数
    ! Local variables
    !
    
    
    ! 実行文; Executable statement
    !

    this%name = name
    this%nCompField = nCompField



    ! Print the information of intiialized DCPCMCompenent
    !
  end subroutine DCPCMComp_Init


end module DCPCMComp_def

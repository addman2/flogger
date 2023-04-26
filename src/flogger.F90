module class_Logger
  implicit none
  private
  !
  ! Type Handler
  type, private :: Handler
    character(len=64) :: verbosity
    integer*4 :: outunit
    integer*4 :: outtype
    character(len=512) :: outname
  end type Handler
  !
  ! Class Logger
  type, public :: Logger
     integer*4 :: loglvl = 0
     integer*4, allocatable, dimension(:) :: loglevels
     character(len=64), allocatable, dimension(:) :: logkeywords
     type(handler), allocatable, dimension(:) :: handlers
  contains
     procedure, public  :: logg          => Logger_log
     procedure, public  :: add_handler   => Logger_addh
     procedure, public  :: setlvl        => Logger_setlvl
     procedure, public  :: get_logcheck  => Loggerp_check
     procedure, public  :: prnt          => Logger_print
  end type Logger
  !
  interface get_logcheck
     module procedure Loggerp_check
  end interface get_logcheck
  !
  interface get_loglevels
     module procedure Loggerp_get_loglevels
  end interface get_loglevels
  !
  interface get_logkeywords
     module procedure Loggerp_get_logkeywords
  end interface get_logkeywords
  !
  interface logger
    module procedure constructor
  end interface logger
  !
contains
  !
  ! Class methods
  !
  function constructor()
    !
    ! Constructor
    !
    class(Logger), allocatable :: constructor
    character(len=1024), dimension(7) :: list
    !
    ! These verbosity level can be edited
    ! e.g. if verbosity level 11 is set it means messages with VERBOSITY_LOW and
    ! VERBOSITY_MEDIUM will be printed
    !
    list = [ character(len=1024) ::  "-31 : TEST"&
                                    ,"10  :       VERBOSITY_LOW "&
                                    ,"11  :       VERBOSITY_LOW VERBOSITY_MEDIUM"&
                                    ,"12  :       VERBOSITY_LOW VERBOSITY_MEDIUM VERBOSITY_HIGH"&
                                    ,"112 : DEBUG VERBOSITY_LOW VERBOSITY_MEDIUM VERBOSITY_HIGH"&
                                    ,"1012:       VERBOSITY_LOW VERBOSITY_MEDIUM VERBOSITY_HIGH TIME"&
                                    ,"1112: DEBUG VERBOSITY_LOW VERBOSITY_MEDIUM VERBOSITY_HIGH TIME"]
    allocate(constructor)
    !
    ! Allocate default handler
    call constructor%add_handler("", 6, 0)
    !
    call get_loglevels(list, size(list), constructor%loglevels)
    call get_logkeywords(list, size(list), constructor%logkeywords)
    !
  end function constructor
  !
  ! Private methods and module functions
  !
  subroutine Loggerp_get_loglevels(list, n, loglevels)
    implicit none
    integer*4, allocatable, dimension(:), intent(out) :: loglevels
    integer*4, intent(in) :: n
    character(len=1024), dimension(n), intent(in) :: list
    integer :: ii, ind
    !
    allocate(loglevels(n))
    do ii = 1, n
      ind = index(list(ii), ":")
      read(list(ii)(:ind-1), *) loglevels(ii)
    end do
  end subroutine Loggerp_get_loglevels
  !
  subroutine Loggerp_get_logkeywords(list, n, logkeywords)
    implicit none
    character(len=64), allocatable, dimension(:), intent(out) :: logkeywords
    integer*4, intent(in) :: n
    character(len=1024), dimension(n), intent(in) :: list
    integer :: ii, ind
    !
    allocate(logkeywords(n))
    do ii = 1, n
      ind = index(list(ii), ":")
      logkeywords(ii) = list(ii)(ind+1:)
    end do
  end subroutine Loggerp_get_logkeywords
  !
  subroutine Logger_addh(this, verbosity, outunit, outtype)
    implicit none
    class(Logger), intent(inout) :: this
    character(len=64), intent(in) :: verbosity
    integer*4, intent(in) :: outunit, outtype
    type(handler), allocatable, dimension(:) :: tmp_handlers
    integer*4 :: ii
    !
    if (.not.allocated(this%handlers)) then
      allocate(this%handlers(1))
    else
      allocate(tmp_handlers(size(this%handlers)))
      tmp_handlers(:) = this%handlers(:)
      deallocate(this%handlers)
      allocate(this%handlers(size(tmp_handlers)+1))
      do ii = 1, size(tmp_handlers)
        this%handlers(ii) = tmp_handlers(ii)
      end do
    end if
    !
    this%handlers(size(this%handlers))%verbosity = verbosity
    this%handlers(size(this%handlers))%outunit = outunit
    this%handlers(size(this%handlers))%outtype = outtype
    this%handlers(size(this%handlers))%outname = ""
  end subroutine Logger_addh
  !
  function Loggerp_check(this, tp) result(answer)
    implicit none
    class(Logger), intent(in) :: this
    character(*), intent(in) :: tp
    logical :: answer
    !
    integer,allocatable, target, dimension(:) :: inds
    integer*4 :: ind, ii, jj, sz
    logical :: word = .false.
    !
    answer = .false.
    inds = findloc(this%loglevels, this%loglvl)
    ind = inds(1)
    !
    sz = len(trim(this%logkeywords(ind)))
    jj = 1
    do ii = 1, sz
      if (word) then
        if ((this%logkeywords(ind)(ii:ii).eq." ").or.(ii.eq.sz)) then
          if (trim(tp).eq.trim(this%logkeywords(ind)(jj:ii))) then
            answer = .true.
          end if
          word = .false.
        end if
      else
        if (this%logkeywords(ind)(ii:ii).ne." ") then
          jj = ii
          word = .true.
        end if
      end if
    end do
    !
  end function Loggerp_check
  !
  subroutine Logger_log(this, tp, msg, rank)
    implicit none
    class(Logger), intent(in) :: this
    character(*) :: tp
    character(len=*) :: msg
    integer*4, optional :: rank
    integer*4 :: ii
    !
    if (present(rank)) then
      if (rank.ne.0) then
        return
      end if
    end if
    !
    if (.not.this%get_logcheck(tp)) then
      return
    end if
    !
    do ii = 1, size(this%handlers)
      call this%prnt(this%handlers(ii), msg)
    end do
  end subroutine Logger_log
  !
  subroutine Logger_print(this, h, msg)
    implicit none
    class(Logger), intent(in) :: this
    type(Handler), intent(in) :: h
    character(len=*) :: msg
    !
    if (trim(h%verbosity).eq."") then
      write (h%outunit, *) msg
    end if
  end subroutine Logger_print
  !
  subroutine Logger_setlvl(this, lvl)
    ! Log level setter
    class(Logger), intent(inout) :: this
    integer*4 :: lvl
    this%loglvl = lvl
  end subroutine Logger_setlvl
end module class_Logger

module logging
  use class_Logger
  implicit none
  private :: int2str, real2str
  interface operator(//)
    module procedure int2str, real2str, reald2str, logical2str
  end interface
  interface operator(**)
    module procedure fmt_int, fmt_real, fmt_reald, fmt_realdd
  end interface
  interface operator(.trm.)
    module procedure fmt_trim
  end interface
  !
contains
  !
  function int2str( x, y ) result(res)
    implicit none
    character(*), intent(in)  :: x
    integer*4, intent(in)     :: y
    character(:), allocatable :: res
    !
    character(len=30) ret
    write ( ret, * ) y
    res = x // trim(adjustL( ret ))
  end
  !
  function real2str( x, y ) result(res)
    implicit none
    character(*), intent(in)  :: x
    real*4, intent(in)        :: y
    character(:), allocatable :: res
    !
    character(len=30) ret
    write ( ret, * ) y
    res = x // trim(adjustL( ret ))
  end
  !
  function reald2str( x, y ) result(res)
    implicit none
    character(*), intent(in)  :: x
    real*8, intent(in)        :: y
    character(:), allocatable :: res
    !
    character(len=30) ret
    write ( ret, * ) y
    res = x // trim(adjustL( ret ))
  end
  !
  function logical2str( x, y ) result(res)
    implicit none
    character(*), intent(in)  :: x
    logical*4, intent(in)     :: y
    character(:), allocatable :: res
    !
    character(len=30) ret
    if (y) then
      write ( ret, * ) ' True'
    else
      write ( ret, * ) ' False'
    end if
    res = x // trim(adjustL( ret ))
  end
  !
  function fmt_trim( x ) result(res)
    implicit none
    character(*), intent(in)  :: x
    character(:), allocatable :: res
    !
    res = trim(adjustL( x ))
  end
  !
  function fmt_int( x, y ) result(res)
    implicit none
    integer, intent(in)     :: x
    character(*), intent(in)  :: y
    character(:), allocatable :: res
    !
    character(len=30) ret
    write ( ret, '(A,'//y//')' ) "|", x
    res = trim(adjustL( ret ))
    res = res(2:)
  end
  !
  function fmt_real( x, y ) result(res)
    implicit none
    real*4, intent(in)        :: x
    character(*), intent(in)  :: y
    character(:), allocatable :: res
    !
    real*16                   :: x_mod
    character(len=12)         :: y_mod
    character(len=12)         :: appendix = ""
    character(len=30)         :: ret
    !
    x_mod = x
    y_mod = y
    !
    call conv(x_mod, y_mod, appendix)
    !
    if (check_trim(y_mod)) then
      write ( ret, '('//y_mod//',A)' ) x_mod, trim(appendix)
      res = trim(adjustL( ret ))
    else
      write ( ret, '(A,'//y_mod//',A)' ) "|", x_mod, trim(appendix)
      res = trim(adjustL( ret ))
      res = res(2:)
    end if
  end
  !
  function fmt_reald( x, y ) result(res)
    implicit none
    real*8, intent(in)        :: x
    character(*), intent(in)  :: y
    character(:), allocatable :: res
    !
    real*16                   :: x_mod
    character(len=12)         :: y_mod
    character(len=12)         :: appendix = ""
    character(len=30)         :: ret
    !
    x_mod = x
    y_mod = y
    !
    call conv(x_mod, y_mod, appendix)
    !
    if (check_trim(y_mod)) then
      write ( ret, '('//y_mod//',A)' ) x_mod, trim(appendix)
      res = trim(adjustL( ret ))
    else
      write ( ret, '(A,'//y_mod//',A)' ) "|", x_mod, trim(appendix)
      res = trim(adjustL( ret ))
      res = res(2:)
    end if
  end
  !
  function fmt_realdd( x, y ) result(res)
    implicit none
    real*16, intent(in)       :: x
    character(*), intent(in)  :: y
    character(:), allocatable :: res
    !
    real*16                   :: x_mod
    character(len=12)         :: y_mod
    character(len=12)         :: appendix = ""
    character(len=30)         :: ret
    !
    x_mod = x
    y_mod = y
    !
    call conv(x_mod, y_mod, appendix)
    !
    if (check_trim(y_mod)) then
      write ( ret, '('//y_mod//',A)' ) x_mod, trim(appendix)
      res = trim(adjustL( ret ))
    else
      write ( ret, '(A,'//y_mod//',A)' ) "|", x_mod, trim(appendix)
      res = trim(adjustL( ret ))
      res = res(2:)
    end if
  end
  !
  function check_trim( x ) result(res)
    implicit none
    character(*), intent(inout)  :: x
    logical                      :: res
    !
    integer*4 :: ii
    !
    res = .False.
    do ii = 1, len(x)
      if (x(ii:ii).eq."@") then
        x = trim(x(1:ii-1)) // trim(x(ii+1:len(x)))
        res = .True.
        return
      end if
    end do
  end
  !
  subroutine conv( x, operand, appendix )
    implicit none
    real*16, intent(inout)          :: x
    character(len=*), intent(inout) :: operand, appendix
    character(len=12)               :: conversion, tmp
    integer*4                       :: ii, endop
    conversion = ""
    !
    if (operand(1:1).ne."!") then
      return
    end if
    !
    ii = 2
    do ii = 2, 12
      if (operand(ii:ii).eq."!") then
        endop = ii
      end if
    end do
    !
    conversion = operand(2:endop-1)
    tmp = operand(endop+1:len(operand))
    operand = tmp
    !
    select case (trim(conversion))
     case("H2eV")
       x = x * 27.211386245988
       appendix = " eV"
     case("B2Ang")
       x = x * 0.529177249
       appendix = " Ang"
    end select
  end
  !
end module logging

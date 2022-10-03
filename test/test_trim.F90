program test_logger
  !
  ! Test suite
  use logging
  implicit none

  integer*4 :: ii
  real*8 :: x = 1
  logical :: t
  character(len=15) :: operand = "!H2eV!@mama", appendix
  !
  t  = check_trim(operand)
  if (trim(operand).ne."!H2eV!mama") STOP 1
  if (.not.t) STOP 1
  !
  t  = check_trim(operand)
  if (t) STOP 1
end program test_logger

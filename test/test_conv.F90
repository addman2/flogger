program test_logger
  !
  ! Test suite
  use logging
  implicit none
  !
  integer*4 :: ii
  real*16 :: x = 1
  character(len=10) :: operand = "!H2eV!5.2f", appendix
  !
  call conv(x, operand, appendix)
  !
  if (trim(operand).ne."5.2f") STOP 1
  if (abs(x-27.21138).gt.0.001) STOP 1
  if (trim(appendix).ne." eV") STOP 1
end program test_logger

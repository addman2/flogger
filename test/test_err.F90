program test_logger
  !
  ! Test suite
  use logging
  implicit none
  !
  type(Logger) :: l
  !
  l = Logger()
  !
  call l%setlvl(11)
  call l%logg("ERROR", "Error message!")
  !
end program test_logger

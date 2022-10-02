program test_logger
  !
  ! Test suite
  use logging
  implicit none

  integer*4 :: ii
  type(Logger) :: l
  l = Logger()
  call l%setlvl(11)
  call l%logg("VERBOSITY_LOW", "Hello", 0)
  call l%logg("VERBOSITY_LOW", "this is " // .TRUE., 0)
  call l%logg("VERBOSITY_LOW", "Hello "//5, 0)
  call l%logg("VERBOSITY_LOW", "Hello "//5.1234567, 0)
  call l%logg("VERBOSITY_LOW", "Hello "//5.1234567**'f8.3', 0)
  call l%logg("DEBUG", "Hello "//5.1234567**'f8.3', 0)
  do ii = 1, 3
    print *, l%loglevels(ii)
    print *, l%logkeywords(ii)
  end do
end program test_logger

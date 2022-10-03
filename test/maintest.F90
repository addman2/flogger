program test_logger
  !
  ! Test suite
  use logging
  implicit none
  !
  integer*4 :: ii
  character(len=6) :: animal
  logical, parameter :: T = .True.
  type(Logger) :: l
  !
  animal = "Girafe"
  !
  l = Logger()
  !
  ! Setting verbosity level to 11 (messages with VERBOSITY_LOW and
  !                                VERBOSITY_HIGH will be printed)
  !
  ! logg method has three arguments. First one specifies verbosity level.
  ! In princpile that can be done via enum. Second the message and the third one
  ! is the rank so only rank.eq.0 will show the message. For serial codes this
  ! is unnecessary and it should be always zero. Infact I can be an optional
  ! parameter TODO.
  !
  call l%setlvl(11)
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "Hello this is a cool software suite!", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "====================================", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "It is for logging and formating outputs,&
                              & here are some examples:", 0)
  !
  ! This package makes formating a little bit easier. Appending operator now
  ! works for any kind of basic variable.
  !
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "This flag is " // .TRUE., 0)
  call l%logg("VERBOSITY_LOW", "This flag is " // .FALSE., 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "Hello I am " // animal, 0)
  call l%logg("VERBOSITY_LOW", "I have " // 5 // " friends", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  !
  ! One can format output by specifying **'ftm'. fmt should have the same
  ! format as one would use in write (5,'fmt').
  !
  call l%logg("VERBOSITY_LOW", "The total run time is "// 5.1234567&
                          & // " seconds", 0)
  call l%logg("VERBOSITY_LOW", "On your bank balance is "&
                          & //  5.123456789 ** 'f8.2'&
                          & // " sestertii", 0)
  !
  ! One can specify special formats like here for eV and Ang
  !
  call l%logg("VERBOSITY_LOW", "Hydrogen ionization energy is "&
                          & // 0.5 ** '!H2eV!f8.3', 0)
  call l%logg("VERBOSITY_LOW", "My favorite bond length is "&
                          & // 2.91 ** '!B2Ang!f8.3', 0)
  !
  ! By adding extra @ letter one can trim the output
  !
  call l%logg("VERBOSITY_LOW", "My second favorite bond length is "&
                          & // 0.7 ** '!B2Ang!@f8.3', 0)
  !
  ! I defined special operator for triming, be aware it has the highest
  ! precendens priority of all operator in fortran
  !
  call l%logg("VERBOSITY_LOW", .trm."       ", 0)
  call l%logg("VERBOSITY_LOW", .trm."     Poetry:  ", 0)
  call l%logg("VERBOSITY_LOW", .trm."     Nel mezzo del cammin di ...  ", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  !
  ! This messages will not be printed, because of verbosity level
  !
  call l%logg("VERBOSITY_HIGH", "this is a secret message!", 0)
  call l%logg("DEBUG", "Internal counter is on " // 98, 0)
  !
  ! Beutifull thing would be to have some handlers that would stear
  ! output to different places like stdout, stderr, etc ...
  !
  call l%logg("VERBOSITY_LOW", "That's all Folks!", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  call l%logg("VERBOSITY_LOW", "====================================", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  !
  call l%logg("VERBOSITY_LOW", "Here goes some internal things", 0)
  call l%logg("VERBOSITY_LOW", "", 0)
  !
  do ii = 1, 3
    print *, "My log level is ", l%loglevels(ii)
    print *, "My keyword is ", l%logkeywords(ii)
  end do
end program test_logger

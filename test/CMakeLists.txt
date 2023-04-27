list(APPEND TESTERS maintest)
list(APPEND TESTERS test_conv)
list(APPEND TESTERS test_trim)
list(APPEND TESTERS test_err)

foreach(TESTER IN LISTS TESTERS)
  add_executable(${TESTER})

  target_include_directories(${TESTER} PUBLIC ${CMAKE_BINARY_DIR})
  
  target_sources(${TESTER}
      PRIVATE
      ${TESTER}.F90
      )

  target_link_libraries( ${TESTER}
      flogger
      )

  add_test(
      NAME "${TESTER}"
      COMMAND $<TARGET_FILE:${TESTER}>
      )
  
endforeach()

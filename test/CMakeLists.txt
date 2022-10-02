add_executable(tester)

target_include_directories(tester PUBLIC ${CMAKE_BINARY_DIR})

target_sources(tester
    PRIVATE
    test.F90
    )

add_test(
    NAME "The_test"
    COMMAND $<TARGET_FILE:tester>
    )

target_link_libraries( tester
    flogger
    )

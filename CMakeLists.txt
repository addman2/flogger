cmake_minimum_required(VERSION 3.15.0 FATAL_ERROR)

project(flogger Fortran) 

include(CTest)

message(STATUS "")
message(STATUS "------------------------------------------------------------")
message(STATUS "Yello I am flogger, nice to meet you!")
message(STATUS "------------------------------------------------------------")
message(STATUS "")

set(LIBNAME flogger)

add_library(${LIBNAME})
install(TARGETS ${LIBNAME})
set_target_properties(${LIBNAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})
target_include_directories(${LIBNAME} PUBLIC ${CMAKE_BINARY_DIR})

add_subdirectory(src)
add_subdirectory(test)

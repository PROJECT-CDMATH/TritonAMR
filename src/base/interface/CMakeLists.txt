# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_interface . *.f90)
ADD_LIBRARY(interface SHARED ${src_base_interface})
ADD_DEPENDENCIES(interface modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS interface DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

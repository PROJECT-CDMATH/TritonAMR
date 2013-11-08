# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_amr . *.f90)
ADD_LIBRARY(amr SHARED ${src_base_amr})
ADD_DEPENDENCIES(amr modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS amr DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

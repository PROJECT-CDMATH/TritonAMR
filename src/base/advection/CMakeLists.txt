# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_advection . *.f90)
ADD_LIBRARY(advection SHARED ${src_base_advection})
ADD_DEPENDENCIES(advection modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS advection DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

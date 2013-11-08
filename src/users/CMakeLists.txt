# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_users . *.f90)
ADD_LIBRARY(users SHARED ${src_users})
ADD_DEPENDENCIES(users modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS users DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

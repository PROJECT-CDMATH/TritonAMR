# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_modules . *.f90)
ADD_LIBRARY(modules SHARED ${src_modules})

INSTALL(TARGETS modules DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

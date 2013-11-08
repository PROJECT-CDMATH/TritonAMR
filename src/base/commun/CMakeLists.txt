# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_commun . *.f90)
ADD_LIBRARY(commun SHARED ${src_base_commun})
ADD_DEPENDENCIES(commun modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS commun DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

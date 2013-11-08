# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_diffusion . *.f90)
ADD_LIBRARY(diffusion SHARED ${src_base_diffusion})
ADD_DEPENDENCIES(diffusion modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS diffusion DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

# nom du projet
PROJECT(TritonAMR C CXX Fortran)
# version minimum de cmake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

FILE(GLOB src_base_visualisation amr_plt_paraview_NODE_without_VTK.f90 s_vertex_U.f90 s_vertex_V.f90 s_vertex_W.f90 s_vertex_YY.f90)
ADD_LIBRARY(visualisation SHARED ${src_base_visualisation})
ADD_DEPENDENCIES(visualisation modules)
#TARGET_LINK_LIBRARIES(modules)

INSTALL(TARGETS visualisation DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

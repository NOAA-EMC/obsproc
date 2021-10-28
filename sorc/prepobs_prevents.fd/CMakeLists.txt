if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -assume noold_ldout_format -fixed")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  prevents.f
  gblevents_cdas.f
  ${CMAKE_CURRENT_SOURCE_DIR}/../glbevents.fd/gblevents.f
)

set(exe_name prepobs_prevents)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bufr::bufr_4
  nemsio::nemsio
  w3emc::w3emc_4
  w3nco::w3nco_4
  sp::sp_4
  sigio::sigio
  bacio::${bacio_name}
  NetCDF::NetCDF_Fortran)

if(MKL_FOUND)
  target_link_libraries(
    ${exe_name}
    ${MKL_LIBRARIES})
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

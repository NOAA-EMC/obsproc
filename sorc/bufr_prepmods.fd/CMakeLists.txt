list(APPEND fortran_src
  bufr_prepmods.f
  clonlat.f
  mnemonics.f
  obname.f
  raddate.f
  ssmipn.f
  tideg.f
  mbuoyb.f
)

set(exe_name bufr_prepmods)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

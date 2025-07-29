list(APPEND parm_files
  prepobs_cqcbufr.core.parm
  prepobs_cqcbufr.gfs.parm
  prepobs_prepacqc.core.parm
  prepobs_prepacqc.gfs.parm
  prepobs_prepdata.core.parm
  prepobs_prepdata.gfs.parm
  prepobs_prepssmi.core.parm
  prepobs_prepssmi.gfs.parm
  prepobs_profcqc.core.parm
  prepobs_profcqc.gfs.parm
  syndat_syndata.core.parm
  syndat_syndata.gfs.parm
  transfer_airnow.list
  transfer_hourly.list.old0203
  prepobs_cqcbufr.gdas.parm
  prepobs_prepacqc.gdas.parm
  prepobs_prepssmi.gdas.parm
  prepobs_profcqc.gdas.parm
  syndat_syndata.gdas.parm

  )

install(FILES ${parm_files}
        DESTINATION "parm")

list(APPEND parm_files
  prepobs_cqcbufr.core.parm
  prepobs_prepacqc.core.parm
  prepobs_prepdata.core.parm
  prepobs_prepssmi.core.parm
  prepobs_profcqc.core.parm
  syndat_syndata.core.parm
  transfer_airnow.list
  transfer_hourly.list.old0203
  )

install(FILES ${parm_files}
        DESTINATION "parm")

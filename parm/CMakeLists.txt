list(APPEND parm_files
  prepobs_cqcbufr.cdas.parm
  prepobs_cqcbufr.gdas.parm
  prepobs_cqcbufr.gfs.parm
  prepobs_cqcbufr.rap.parm
  prepobs_cqcbufr.rtma3d.parm
  prepobs_cqcbufr.urma3d.parm
  prepobs_prepacqc.cdas.parm
  prepobs_prepacqc.gdas.parm
  prepobs_prepacqc.gfs.parm
  prepobs_prepacqc.rap.parm
  prepobs_prepacqc.rtma3d.parm
  prepobs_prepacqc.urma3d.parm
  prepobs_prepdata.cdas.parm
  prepobs_prepdata.gdas.parm
  prepobs_prepdata.gfs.parm
  prepobs_prepdata.rap.parm
  prepobs_prepdata.rtma.parm
  prepobs_prepdata.urma.parm
  prepobs_prepdata.rtma3d.parm
  prepobs_prepdata.urma3d.parm
  prepobs_prepssmi.cdas.parm
  prepobs_prepssmi.gdas.parm
  prepobs_prepssmi.gfs.parm
  prepobs_prepssmi.rap.parm
  prepobs_prepssmi.rtma.parm
  prepobs_prepssmi.rtma3d.parm
  prepobs_prepssmi.urma3d.parm
  prepobs_prevents.cdas.parm
  prepobs_profcqc.gdas.parm
  prepobs_profcqc.gfs.parm
  prepobs_profcqc.rap.parm
  prepobs_profcqc.rtma3d.parm
  prepobs_profcqc.urma3d.parm
  syndat_syndata.gdas.parm
  syndat_syndata.gfs.parm
  wave_dcodquikscat.rtma.parm
  wave_dcodquikscat.rtma3d.parm
  )

install(FILES ${parm_files}
        DESTINATION "parm")

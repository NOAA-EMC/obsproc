list(APPEND scripts_files
  exairnow_dump.sh.ecf
  excdas_dump.sh.ecf
  excdas_makeprepbufr.sh.ecf
  exdump_alert.sh.ecf
  exdump_post.sh
  exdump_monitor.sh
  exglobal_dump.sh.ecf
  exglobal_makeprepbufr.sh.ecf
  exmods.sh.ecf
  exnam_dump.sh.ecf
  exnam_makeprepbufr.sh.ecf
  exprep_post.sh
  exrap_dump.sh
  exrap_makeprepbufr.sh
  exrtma_dump.sh.ecf
  exrtma_dump.sh.ecf.correct
  exrtma_makeprepbufr.sh.ecf
  exurma_dump.sh.ecf
  exurma_makeprepbufr.sh.ecf
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

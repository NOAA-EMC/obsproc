list(APPEND scripts_files
  exairnow_dump.sh
  excdas_dump.sh
  excdas_makeprepbufr.sh
  exdump_alert.sh
  exdump_monitor.sh
  exdump_post.sh
  exglobal_dump.sh
  exglobal_makeprepbufr.sh
  exmods.sh
  exnam_dump.sh
  exnam_makeprepbufr.sh
  exrap_dump.sh
  exrap_makeprepbufr.sh
  exrtma_dump.sh
  exrtma_makeprepbufr.sh
  exurma_dump.sh
  exurma_makeprepbufr.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

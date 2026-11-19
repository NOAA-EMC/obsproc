list(APPEND scripts_files
  excdas_dump.sh
  excdas_makeprepbufr.sh
  exdump_alert.sh
  exdump_monitor.sh
  exdump_post.sh
  exglobal_dump.sh
  exglobal_makeprepbufr.sh
  exmods.sh
  exrap_dump.sh
  exrap_makeprepbufr.sh
  exrtma_dump.sh
  exrtma_makeprepbufr.sh
  exurma_dump.sh
  exurma_makeprepbufr.sh
  exrtma3d_dump.sh
  exrtma3d_makeprepbufr.sh
  exurma3d_dump.sh
  exurma3d_makeprepbufr.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

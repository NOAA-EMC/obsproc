list(APPEND scripts_files
  exdump_alert.sh
  exdump_monitor.sh
  exdump_post.sh
  exglobalcore_dump.sh
  exglobalcore_makeprepbufr.sh
  exmods.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

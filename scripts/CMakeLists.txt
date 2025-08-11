list(APPEND scripts_files
  exdump_post.sh
  exglobalcore_dump.sh
  exglobalcore_makeprepbufr.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

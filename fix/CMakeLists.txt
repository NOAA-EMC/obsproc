# Collect list of fix files to install.
list(APPEND fix_files
  bufrtab.mods
  prepobs_anow.bufrtable
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")

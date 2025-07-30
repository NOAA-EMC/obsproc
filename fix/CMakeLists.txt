# Collect list of fix files to install.
list(APPEND fix_files
  bufrtab.mods
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")

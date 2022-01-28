# Collect list of fix files to install.
list(APPEND fix_files
  bufrtab.mods
  gdascounts_avg.awk
  gdascounts_base.shtml
  gdascounts_html
  gdascounts_types
  prepobs_anow.bufrtable
  satellite_counts.tbl
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")

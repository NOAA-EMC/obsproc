list(APPEND ush_files
  bufr_avgdata.sh
  bufr_datacount.sh
  bufr_prepmods.sh
  bufr_remorest.sh
  dumpmb
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")

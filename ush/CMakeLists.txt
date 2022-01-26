list(APPEND ush_files
  bufr_avgdata.sh
  bufr_datacount.sh
  bufr_prepmods.sh
  bufr_remorest.sh
  build_mainpage.sh
  dumpmb
  gdas_counts.sh
  gdas_countstat.sh
  gdas_summary.sh
  gdascounts_combine.sh
  global_postevents.sh
  mstr_shp_stn_lst_update.sh
  mstr_shp_stn_lst_update2.sh
  satellite_daily.pl
  satellite_html.pl
  satellite_summary.sh
  )

install(PROGRAMS ${ush_files}
        DESTINATION "ush")

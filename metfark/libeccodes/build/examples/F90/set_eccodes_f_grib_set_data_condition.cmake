  if(  HAVE_FORTRAN )
    set(_eccodes_f_grib_set_data_condition TRUE)
  else()
    set(_eccodes_f_grib_set_data_condition FALSE)
  endif()
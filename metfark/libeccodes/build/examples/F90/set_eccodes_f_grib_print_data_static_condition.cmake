  if(  HAVE_FORTRAN )
    set(_eccodes_f_grib_print_data_static_condition TRUE)
  else()
    set(_eccodes_f_grib_print_data_static_condition FALSE)
  endif()

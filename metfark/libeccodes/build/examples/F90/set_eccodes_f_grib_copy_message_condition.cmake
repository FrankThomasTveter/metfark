  if(  HAVE_FORTRAN )
    set(_eccodes_f_grib_copy_message_condition TRUE)
  else()
    set(_eccodes_f_grib_copy_message_condition FALSE)
  endif()
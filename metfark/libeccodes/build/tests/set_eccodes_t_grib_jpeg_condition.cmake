  if(  HAVE_JPEG )
    set(_eccodes_t_grib_jpeg_condition TRUE)
  else()
    set(_eccodes_t_grib_jpeg_condition FALSE)
  endif()
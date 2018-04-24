
function(EXEC_CHECK)
     execute_process(COMMAND ${ARGV} RESULT_VARIABLE CMD_RESULT)
     if(CMD_RESULT)
           message(FATAL_ERROR "Error running ")
     endif()
endfunction()

exec_check( "/usr/bin/cmake" --build "/metfark/metfark/metfark/libeccodes/build" --target __get_data_eccodes_download_metars_metar_txt/fast )
exec_check( "/usr/bin/cmake" --build "/metfark/metfark/metfark/libeccodes/build" --target __get_data_eccodes_download_metars_metar_txt_dump_ref/fast )
exec_check( "/usr/bin/cmake" --build "/metfark/metfark/metfark/libeccodes/build" --target __get_data_eccodes_download_metars_metar_txt_ls_ref/fast )

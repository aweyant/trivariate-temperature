#!/bin/bash

cd data/livneh_tmin/

file_list=(tmin*.nc)

# SECTION NOT NEEDED
#'for file in "${file_list[@]}"
#'do
#'   ncap2 -O -s '@units="days since 1900-1-1 00:00:00";time=udunits(time,@units);time@units=@units' $file file
#'done

ncrcat -h --no_tmp_fl ${file_list[@]} complete.tmin.nc

cd ../livneh_tmax

file_list=(tmax*.nc)
ncrcat -h --no_tmp_fl ${file_list[@]} complete.tmax.nc


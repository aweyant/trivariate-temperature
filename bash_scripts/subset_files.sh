#!/bin/bash

cd data/livneh_tmin/

file_list=(tmin*.nc)

for file in "${file_list[@]}"
do
   ncea -O -d lat,27.0,52.0 -d lon,235.0,248.0 $file $file
done

cd ../livneh_tmax
file_list=(tmax*.nc)

for file in "${file_list[@]}"
do
   ncea -O -d lat,27.0,52.0 -d lon,235.0,248.0 $file $file
done

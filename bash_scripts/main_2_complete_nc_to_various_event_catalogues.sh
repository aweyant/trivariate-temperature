#!/bin/bash

cd ../trivariate_temperature_R_project

Rscript ./create_events_from_nc.R ../data/livneh_tmin/complete.tmin.nc 95 percentile tmin

Rscript ./create_events_from_nc.R ../data/livneh_tmin/complete.tmin.nc 97.5 percentile tmin

Rscript ./create_events_from_nc.R ../data/livneh_tmin/complete.tmin.nc 99 percentile tmin

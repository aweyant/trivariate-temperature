cd ../data/ghcnd


readarray -t arr < <( awk -F ' ' '{ print $1 }' ghcnd-stations-ca-identifying-columns.txt )

#for i in "${arr[@]}"
#do
#	wget https://www.ncei.noaa.gov/data/daily-summaries/access/$i.csv
#done

#for i in "${arr[@]}"
#do
#	echo "\"$i\", $(awk -F ',' 'NR==2{print $2 }' $i.csv)" >> ghcnd-stations-ca-first-record.txt
#done

#cd ..

#Rscript ../trivariate_temperature_R_project/associate_stations_with_start_date.R

#cd ghcnd

readarray -t arr < ghcnd-pre-84-stations-list.txt

for i in "${arr[@]}"
do
	echo "$i, $(awk -F ',' 'NR==1{print}' $i)" >> temp.csv
done

sed '/TMAX/ w temp2.csv' temp.csv

awk -F ',' '{print $1 }' temp2.csv > ghcnd-pre-84-stations-list.txt


#echo "${arr[@]}"
#cat "${arr[@]}" >> ghcnd-stations-ca-pre-1984.csv

#sed -i '/STATION/ d' ghcnd-stations-ca-pre-1984.csv

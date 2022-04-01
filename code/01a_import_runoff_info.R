library(data.table)

runoff_stations <- fread('C:/Users/User/Desktop/project/eda_rhine/data/raw/runoff_stations.csv')

runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, id := factor(id)]
runoff_stations[, lat := round(lat, 3)]
runoff_stations[, lon := round(lon, 3)]
runoff_stations[, altitude := round(altitude, 0)]

saveRDS(runoff_stations, './data/runoff_stations_raw.rds')

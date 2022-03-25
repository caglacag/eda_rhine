list.files('data/raw')
library(data.table)

runoff_stations <- fread('./data/raw/runoff_stations.csv')

runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, id := factor(id)]
runoff_stations[, lat := round(lat, 3)]
runoff_stations[, lon := round(lon, 3)]
runoff_stations[, altitude := round(altitude, 0)]

saveRDS(runoff_stations, './data/runoff_stations_raw.rds')
library(data.table)

runoff_stations <- readRDS('./data/runoff_stations_raw.rds')

raw_path <- './data/raw/runoff_day/'
fnames <- list.files(raw_path)
n_station <- length(fnames)
id_length <- 7
runoff_day_raw <- data.table()
id_sname <- runoff_stations[, .(id, sname)]

for(file_count in 1:n_station){
  temp_dt <- fread(paste0(raw_path, fnames[file_count]))
  station_id <- substr(fnames[file_count], 1, id_length)
  temp_dt <- cbind(id = factor(station_id), temp_dt)
  temp_dt <- id_sname[temp_dt, on = 'id', ]
  runoff_day_raw <- rbind(runoff_day_raw, temp_dt)
}

runoff_day_raw[, 'hh:mm' := NULL]
colnames(runoff_day_raw)[3:4] <- c('date', 'value')
runoff_day_raw[, date := as.Date(date)]

saveRDS(runoff_day_raw, './data/runoff_day_raw.rds')
library(data.table)
library(ggplot2)

runoff_stations <- readRDS('./data/runoff_stations_raw.rds')
runoff_day <- readRDS('./data/runoff_day_raw.rds')

rees_runoff_day <- runoff_day[sname == 'REES']

ggplot(data = rees_runoff_day) +
  geom_line(aes(x = date, y = value))

ggplot(data = rees_runoff_day) +
  geom_point(aes(x = date, y = value))

ggplot(data = rees_runoff_day, 
       aes(x = date, y = value)) +
  geom_point() +
  geom_line()

rees_dier_runoff_day <- runoff_day[sname == 'REES' | sname == 'DIER']

ggplot(data = rees_dier_runoff_day) +
  geom_line(aes(x = date, y = value, col = sname))

ggplot(data = runoff_day, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~sname) + 
  theme_bw()

missing_values <- runoff_day[value < 0, .(missing = .N), by = sname]

sample_size <- runoff_day[, .(size = .N), by = sname]
runoff_stations <- runoff_stations[sample_size, on = 'sname']
runoff_stations <- missing_values[runoff_stations, on = 'sname']
runoff_stations[is.na(missing), missing := 0]
runoff_stations[, missing := missing / size]
runoff_stations[, missing := round(missing, 3)]
setcolorder(runoff_stations,                       #making 'missing' last column
            c(setdiff(names(runoff_stations), 'missing'), 'missing'))

runoff_day <- runoff_day[value >= 0]  

rees_runoff_day <- runoff_day[sname == 'REES']
ggplot(rees_runoff_day, aes(x = date, y = value)) +
  geom_line() + 
  geom_point() +
  theme_bw()

station_time <- runoff_day[, .(start = min(year(date)),end = max(year(date))), by = sname]

max_year <- 2016
min_year <- max_year - (30 * 2)

runoff_stations <- runoff_stations[station_time, on  = 'sname']
runoff_stations <- runoff_stations[start <=  min_year & 
                                     end >= max_year & 
                                     size >= 30 * 2 * 365]
runoff_day <- runoff_day[id %in% runoff_stations$id]
runoff_day <- runoff_day[year(date) <= 2016]

saveRDS(runoff_stations, './data/runoff_stations.rds')
saveRDS(runoff_day, './data/runoff_day.rds')
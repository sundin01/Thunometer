
data_combination <- suppressWarnings(function(){
  print('Checkpoint 1')
  measurement_files <- list.files("data/Thun/")
  measurement_files <- purrr::map(as.list(paste("data/Thun/",measurement_files,sep = "")),~read_delim(., delim = ','))
  measurement_files <- bind_rows(measurement_files, .id = "column_label")
  measurement_files <- measurement_files |>
    mutate(hour = hour(time),
           month = month(time),
           day = day(time),
           year = year(time))

  measurement_files <- measurement_files |>
    group_by(hour, day, month, year) |>
    dplyr::summarise(across(where(is.numeric), mean))

  measurement_files <-  measurement_files |> pivot_longer(cols = starts_with("L"), names_to = "Log_Nr",values_to = "temperature") |>
    mutate(Log_Nr = as.numeric(str_replace(Log_Nr, "Log_", ""))) |>
    drop_na()

  print('Checkpoint 2')

  meteoswiss <- read_delim("../data/Meteo_Schweiz/order_123870_data.txt",delim = ";")
  meteoswiss <- meteoswiss |>
    mutate(time = as.POSIXct(as.character(time), format = "%Y%m%d%H%M"),
           temp = as.numeric(tre200s0),
           rain = as.numeric(rre150z0),
           rad = as.numeric(gre000z0),
           winds = as.numeric(fkl010z0),
           windd = as.numeric(dkl010z0),
           pressure = as.numeric(prestas0),
           humidity = as.numeric(ure200s0))|>
    dplyr::select(time, temp, rain, rad, winds, windd, pressure, humidity) |>
    mutate(time = time+hours(2)) |>
    drop_na()

  print('Checkpoint 3')
  meteoswiss <- meteoswiss |>
    mutate(hour = hour(time),
           month = month(time),
           day = day(time),
           year = year(time))
  meteoswiss <- meteoswiss |>
    dplyr::group_by(hour, day, month, year) |>
    dplyr::summarise(across(where(is.numeric), mean),.groups = 'drop') |>
    mutate(rain = rain*6) # to get sum... was mean

  print('Checkpoint 4')
  meteoswiss <- meteoswiss|>
    mutate(timestamp = ymd_h(paste(year,month,day,hour,sep = "-"))) |>
    arrange(timestamp)|>
    mutate(
      mean_temp_6_hours = zoo::rollmean(temp, k = 6, fill = NA, align = "right"),
      mean_temp_12_hours = zoo::rollmean(temp, k = 12, fill = NA, align = "right"),
      mean_temp_1_day = zoo::rollmean(temp, k = 24 * 1, fill = NA, align = "right"),
      mean_temp_3_days = zoo::rollmean(temp, k = 24 * 3, fill = NA, align = "right"),
      mean_temp_5_days = zoo::rollmean(temp, k = 24 * 5, fill = NA, align = "right"),
      mean_pressure_6_hours = zoo::rollmean(pressure, k = 6, fill = NA, align = "right"),
      mean_pressure_12_hours = zoo::rollmean(pressure, k = 12, fill = NA, align = "right"),
      mean_pressure_1_day = zoo::rollmean(pressure, k = 24 * 1, fill = NA, align = "right"),
      mean_pressure_3_days = zoo::rollmean(pressure, k = 24 * 3, fill = NA, align = "right"),
      mean_pressure_5_days = zoo::rollmean(pressure, k = 24 * 5, fill = NA, align = "right"),
      mean_winds_6_hours = zoo::rollmean(winds, k = 6, fill = NA, align = "right"),
      mean_winds_12_hours = zoo::rollmean(winds, k = 12, fill = NA, align = "right"),
      mean_winds_1_day = zoo::rollmean(winds, k = 24 * 1, fill = NA, align = "right"),
      mean_winds_3_days = zoo::rollmean(winds, k = 24 * 3, fill = NA, align = "right"),
      mean_winds_5_days = zoo::rollmean(winds, k = 24 * 5, fill = NA, align = "right"),
      mean_windd_6_hours = zoo::rollmean(windd, k = 6, fill = NA, align = "right"),
      mean_windd_12_hours = zoo::rollmean(windd, k = 12, fill = NA, align = "right"),
      mean_windd_1_day = zoo::rollmean(windd, k = 24 * 1, fill = NA, align = "right"),
      mean_windd_3_days = zoo::rollmean(windd, k = 24 * 3, fill = NA, align = "right"),
      mean_windd_5_days = zoo::rollmean(windd, k = 24 * 5, fill = NA, align = "right"),
      mean_windd_5_days = zoo::rollmean(winds, k = 24 * 5, fill = NA, align = "right"),
      mean_rad_6_hours = zoo::rollmean(rad, k = 6, fill = NA, align = "right"),
      mean_rad_12_hours = zoo::rollmean(rad, k = 12, fill = NA, align = "right"),
      mean_rad_1_day = zoo::rollmean(rad, k = 24 * 1, fill = NA, align = "right"),
      mean_rad_3_days = zoo::rollmean(rad, k = 24 * 3, fill = NA, align = "right"),
      mean_rad_5_days = zoo::rollmean(rad, k = 24 * 5, fill = NA, align = "right"),
      mean_humidity_6_hours = zoo::rollmean(humidity, k = 6, fill = NA, align = "right"),
      mean_humidity_12_hours = zoo::rollmean(humidity, k = 12, fill = NA, align = "right"),
      mean_humidity_1_day = zoo::rollmean(humidity, k = 24 * 1, fill = NA, align = "right"),
      mean_humidity_3_days = zoo::rollmean(humidity, k = 24 * 3, fill = NA, align = "right"),
      mean_humidity_5_days = zoo::rollmean(humidity, k = 24 * 5, fill = NA, align = "right"),
      sum_precipitation_6_hours = zoo::rollsum(rain, k = 6, fill = NA, align = "right"),
      sum_precipitation_12_hours = zoo::rollsum(rain, k = 12, fill = NA, align = "right"),
      sum_precipitation_1_day = zoo::rollsum(rain, k = 24 * 1, fill = NA, align = "right"),
      sum_precipitation_3_days = zoo::rollsum(rain, k = 24 * 3, fill = NA, align = "right"),
      sum_precipitation_5_days = zoo::rollsum(rain, k = 24 * 5, fill = NA, align = "right"),
      sum_precipitation_10_days = zoo::rollsum(rain, k = 24 * 10, fill = NA, align = "right")) |>
    mutate_at(vars(starts_with("sum_precip")), ~ ifelse(. < 0.1, 0, .))

  print('Checkpoint 5')
  combined = inner_join(measurement_files,meteoswiss,by = c("hour","day","month","year"))

  print('Checkpoint 6')
  measurement_metadata <- read_csv2("../data/Messnetz_Thun_Steffisburg_Uebersicht.csv")
  measurement_metadata <- measurement_metadata|>
    filter(!is.na(Code_grafana))|>
    filter(is.na(End))|>
    rename(Log_Nr = Log_NR)|>
    distinct(Code_grafana, .keep_all = TRUE)|>
    arrange(Code_grafana, desc(Log_Nr))

  print('Checkpoint 7')
  combined = inner_join(combined, measurement_metadata, by = "Log_Nr")

  combined <- combined |> ungroup()

  combined <- combined |>
    dplyr::mutate(ID = row_number())

  print('Checkpoint 8a')
  tiff_names <- list.files("../data/Tiffs/")
  print('Checkpoint 8b')
  tiff_paths <- paste0("../data/Tiffs/",tiff_names)
  print('Checkpoint 8c')
  tiffs <- terra::rast(tiff_paths)


  print('Checkpoint 9')
  spat_points <- combined |> dplyr::select(c(LV_03_E,LV_03_N))
  print('Checkpoint 9a')
  extracted <- terra::extract(tiffs,spat_points)
  print('Checkpoint 9b')
  combined <- inner_join(combined,extracted,by = "ID")
  print('Checkpoint 9c')
  combined <- combined |>
    dplyr::select(-ID)
  print('Checkpoint 9d')
  write_csv(combined,"../data/Combined.csv")
  print('Checkpoint 10')
  return(combined)
})



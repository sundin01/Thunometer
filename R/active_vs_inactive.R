tables <- read_csv('../data/tables.csv', show_col_types = F)
meta <- read_delim('../data/Messnetz_Thun_Steffisburg_Uebersicht.csv', delim = ';', show_col_types = F)
meta_with_missing <- meta |>
  left_join(tables, by = "Code_grafana", relationship = "many-to-many")

meta_na <- meta_with_missing |>
  filter(is.na(Code_grafana))

# Filter out rows with NA in Code_grafana, then deduplicate
meta_no_na <- meta_with_missing |>
  filter(!is.na(Code_grafana)) |>
  arrange(Code_grafana, desc(Log_NR)) |>
  distinct(Code_grafana, .keep_all = TRUE)

# Add one NA row back if there were any NAs originally
if (nrow(meta_na) > 0) {
  meta_final <- bind_rows(meta_no_na, meta_na)
  write_csv(meta_final, '../data/meta_final.csv')
} else {
  meta_final <- meta_no_na
  write_csv(meta_final, '../data/meta_final.csv')
}

#-------------------------------------------------------------------------------
# Make a map
#-------------------------------------------------------------------------------

# Define the bounding box using latitude and longitude boundaries
bbox <- list(min_lon = 7.583333, min_lat = 46.716667, max_lon = 7.65, max_lat = 46.766667)


plot <- leaflet() |>
  addTiles() |>
  fitBounds(lng1 = bbox$min_lon, lat1 = bbox$min_lat, lng2 = bbox$max_lon, lat2 = bbox$max_lat) |>
  addCircleMarkers(
    data = meta_final,
    ~OST_CHTOPO, ~NORD_CHTOPO,
    label = ~Log_NR,
    radius = 5,
    color = ~ifelse(is.na(Code_grafana), "red", "green"),
    fill = TRUE,
    fillOpacity = 0.7)

print(plot)

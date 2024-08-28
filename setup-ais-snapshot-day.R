
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(janitor)
library(glue)
library(gt)

rm(list = ls())

create_popups <- function(df) {
  title <- "<b><u>Fieldwork Detail</u></b><br>"
  df <- df %>% st_set_geometry(NULL)
  cols <- names(df)
  col_labels <- make_clean_names(cols, case = "sentence")
  lapply(1:nrow(df), function(r) {
    row <- df[r,]
    details <-
      lapply(1:length(cols), function(c) {
        paste0("<b>", col_labels[c], ":</b> ", row[c])
      }) %>%
      paste0(collapse = "<br>")
    paste0(title, details)
  }) %>% paste0()
}

to_sf <- function(df) {
  df %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
}

str_wrap_html <- function(str) {
  str_wrap(str) %>% str_replace_all("\n", "<br>")
}


# Load data ----

wi_counties <- readRDS("shp/counties.rds") %>%
  clean_names() %>%
  rmapshaper::ms_simplify(.1)

wi <- wi_counties %>%
  st_union()

wbic_ais <-
  read_excel("data-ais-snapshot-day/WatersWithAIS.xlsx") %>%
  clean_names() %>%
  select(
    wbic = waterbody_id_code_wbic,
    waterbody_name,
    invasive_species
  ) %>%
  mutate(
    wbic = as.numeric(wbic),
    county_name = if_else(is.na(wbic), waterbody_name, NA) %>%
      str_replace_all("\\s\\(", "_") %>%
      str_split_i("_", 1) %>%
      str_replace_all(" County", ""),
    .after = waterbody_name
  ) %>%
  fill(county_name) %>%
  drop_na(wbic)

wbic_names <- wbic_ais %>% distinct(wbic, waterbody_name)

county_ais <- wbic_ais %>%
  reframe(
    invasive_species = paste(invasive_species, collapse = ", ") %>%
      str_split(", ") %>%
      unlist() %>%
      sort(),
    .by = c(county_name, wbic, waterbody_name)
  )

snapshot_years <- 2014:2024
ais_results <-
  paste0("data-ais-snapshot-day/SSD_", snapshot_years, ".xlsx") %>%
  lapply(read_excel, na = c("", "NA"), guess_max = 1e6) %>%
  bind_rows() %>%
  clean_names() %>%
  mutate(across(contains("_date"), ~parse_date_time(.x, "mdy IMs p"))) %>%
  select(
    fsn = fieldwork_seq_no,
    parameter_code = dnr_parameter_code,
    parameter_name = dnr_parameter_description,
    result = result_value_no,
    datetime = start_date_time,
    station_id,
    station_name = primary_station_name,
    station_type = station_type_code,
    latitude = station_latitude,
    longitude = station_longitude,
    wbic,
    projects,
    group_desc,
    fieldwork_comment
  ) %>%
  mutate(date = as_date(datetime), year = year(date), .after = datetime) %>%
  filter(year %in% snapshot_years) %>%
  drop_na(latitude, longitude, parameter_code, parameter_name) %>%
  mutate(across(c(fsn, parameter_code, station_id, wbic, latitude, longitude), as.numeric)) %>%
  left_join(wbic_names) %>%
  relocate(waterbody_name, .after = wbic) %>%
  arrange(fsn, parameter_code) %>%
  to_sf() %>%
  st_join(wi_counties, join = st_nearest_feature) %>%
  st_drop_geometry()

fieldwork_events <- ais_results %>%
  select(-c("parameter_name", "parameter_code", "result")) %>%
  distinct()

ais_pts <- ais_results %>%
  filter(parameter_code == 20043) %>%
  summarize(
    n_species = n_distinct(result),
    species_found = paste(sort(unique(result)), collapse = ", "),
    .by = -c("parameter_name", "parameter_code", "result")
  ) %>%
  # add back fieldwork with no species found
  bind_rows(fieldwork_events) %>%
  distinct(fsn, .keep_all = T) %>%
  replace_na(list(n_species = 0, species_found = "None")) %>%
  to_sf() %>%
  mutate(popup = create_popups(.))

stn_counties <- ais_pts %>%
  distinct(station_id, county_name)

fieldwork_by_county <- fieldwork_events %>%
  summarize(
    years_monitored = paste(sort(unique(year)), collapse = ", "),
    years = n_distinct(year),
    waterbodies = n_distinct(wbic),
    stations = n_distinct(station_id),
    fieldwork = n_distinct(fsn),
    .by = county_name
  )

county_ais_summary <- ais_results %>%
  filter(parameter_code == 20043) %>%
  summarize(
    years_monitored = paste(sort(unique(year)), collapse = ", "),
    species_found = paste(sort(unique(result)), collapse = ", "),
    years = n_distinct(year),
    waterbodies = n_distinct(wbic),
    stations = n_distinct(station_id),
    fieldwork = n_distinct(fsn),
    species = n_distinct(result),
    .by = county_name
  ) %>%
  bind_rows(fieldwork_by_county) %>%
  distinct(county_name, .keep_all = T) %>%
  replace_na(list(species_found = "None", species = 0))

dnr_parameter_names <- ais_results %>%
  count(parameter_code, parameter_name) %>%
  filter(n > 1) %>%
  drop_na() %>%
  select(-n)

dnr_parameters <- ais_results %>%
  select(parameter_code) %>%
  left_join(dnr_parameter_names) %>%
  drop_na(parameter_name) %>%
  count(parameter_code, parameter_name)

results_wide <- ais_results %>%
  filter(parameter_code %in% dnr_parameters$parameter_code) %>%
  select(station_id, fsn, date, parameter_name, result) %>%
  distinct() %>%
  pivot_wider(names_from = parameter_name, values_from = result, values_fn = ~ paste(.x, collapse = ", "))

ais_groups <-
  ais_results %>%
  reframe(
    name = unlist(str_split(group_desc, " , ")),
    .by = c(year, fsn)
  ) %>%
  distinct() %>%
  mutate(group_size = n_distinct(name), .by = fsn)

annual_counts <- ais_results %>%
  summarize(n_fieldwork = n_distinct(fsn), .by = c(station_id, year)) %>%
  arrange(year)

year_labels <- annual_counts %>%
  summarize(n_stations = n_distinct(station_id), n_fieldwork = sum(n_fieldwork), .by = year) %>%
  mutate(year_label = str_glue("{year}\nStations: {n_stations}\nFieldwork events: {n_fieldwork}")) %>%
  select(year, year_label)



# Stations ----

stn_pts <- ais_pts %>%
  distinct(
    station_id,
    station_name,
    latitude,
    longitude,
    county_name,
    wbic,
    waterbody_name,
    station_type
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  mutate(popup = create_popups(.))

stn_pts_annual <- stn_pts %>%
  left_join(annual_counts) %>%
  left_join(year_labels)

# ggplot() +
#   geom_sf(data = wi) +
#   geom_sf(data = stn_pts)

all_pts <- stn_pts %>%
  left_join({
    fieldwork_events %>%
      summarize(
        years_monitored = paste(sort(year), collapse = ", "),
        .by = station_id
      )
  }) %>%
  select(-county_name) %>%
  st_join(wi_counties %>% select(county_name, geometry)) %>%
  left_join(results_wide) %>%
  mutate(date = as.character(date)) %>%
  mutate(popup = create_popups(.))


## Save image ----

save.image("site/snapshot-day-summary.RData")

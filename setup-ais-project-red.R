
library(tidyverse)
library(sf)
library(leaflet)
library(janitor)
library(glue)
library(gt)

rm(list = ls())


# Load data ----

ais_results <-
  list(
    "data-ais-project-red/WAV Project RED 2008-2014.xlsx",
    "data-ais-project-red/WAV Project RED 2015-2024.xlsx"
  ) %>%
  lapply(readxl::read_excel, na = c("", "NA")) %>%
  bind_rows() %>%
  clean_names() %>%
  rename(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    station_name = primary_station_name) %>%
  mutate(date = as.Date(datetime), year = year(date)) %>%
  select(
    fsn,
    sample_header = sample_header_seq_no,
    parameter_code = dnr_parameter_code,
    parameter_name = dnr_parameter_description,
    result = result_value_no,
    result_value = result_amt,
    datetime, date, year,
    station_id, station_name, station_type = station_type_desc,
    latitude = calc_ll_lat_dd_amt,
    longitude = calc_ll_long_dd_amt,
    county_name,
    wbic, official_waterbody_name,
    plan_id, plan_name,
    fieldwork_comment,
    group_seq_no
  ) %>%
  mutate(across(c(latitude, longitude), ~ if_else(.x == 0, NA, .x))) %>%
  arrange(fsn, sample_header, parameter_code) %>%
  select(-plan_id, -plan_name) %>%
  distinct()

fieldwork_events <- ais_results %>%
  select(-sample_header, -starts_with("parameter"), -starts_with("result")) %>%
  distinct()

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
  readxl::read_excel("data-ais-project-red/WAV Project RED IP.xlsx") %>%
  clean_names() %>%
  select(-sample_comment) %>%
  mutate(full_name = case_when(
    is.na(first_name) ~ last_name,
    is.na(last_name) ~ first_name,
    T ~ paste(first_name, last_name)
  ), .after = last_name) %>%
  select(group_seq_no, everything()) %>%
  arrange(last_name, first_name)

fieldwork_group_crossjoin <- fieldwork_events %>%
  left_join(ais_groups, relationship = "many-to-many")

annual_counts <- fieldwork_group_crossjoin %>%
  summarize(n_fieldwork = n_distinct(fsn), .by = c(station_id, year)) %>%
  arrange(year)

year_labels <- annual_counts %>%
  summarize(n_stations = n_distinct(station_id), n_fieldwork = sum(n_fieldwork), .by = year) %>%
  mutate(year_label = str_glue("{year}\nStations: {n_stations}\nFieldwork events: {n_fieldwork}")) %>%
  select(year, year_label)



# Load points and lines ----

wi_counties <- readRDS("shp/counties.rds") %>%
  clean_names() %>%
  rmapshaper::ms_simplify(.1)

wi <- wi_counties %>%
  st_union()

stn_pts <- read_csv("data-ais-project-red/swims-monitoring-stations.csv.gz") %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    latitude,
    longitude,
    county_name,
    wbic,
    waterbody_name = official_waterbody_name,
    station_type = station_type_code) %>%
  filter(station_id %in% ais_results$station_id) %>%
  filter(latitude > 0, longitude < 0) %>%
  drop_na() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

load("data-ais-project-red/REDlines.Rda")
stn_lines <- lapply(RED, st_as_sf) %>%
  bind_rows() %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    wbic, waterbody_name,
    station_type = station_type_code,
    geometry = geoms
  ) %>%
  filter(station_id %in% ais_results$station_id)

stn_pts_annual <- stn_pts %>%
  left_join(annual_counts) %>%
  left_join(year_labels)

stn_lines_annual <- stn_lines %>%
  left_join(annual_counts) %>%
  left_join(year_labels)


# ggplot() +
#   geom_sf(data = wi) +
#   geom_sf(data = stn_lines)

# ggplot() +
#   geom_sf(data = wi) +
#   geom_sf(data = stn_lines) +
#   geom_sf(data = stn_pts) +
#   geom_sf(data = all_pts, shape = 4)



# Leaflet prep ------------------------------------------------------------

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

all_pts <-
  bind_rows(stn_pts, st_centroid(stn_lines)) %>%
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
  mutate(date = as.character(date))

all_pts$popup = create_popups(all_pts)



# Maps --------------------------------------------------------------------

# all stations
# ggplot() +
#   geom_sf(data = wi_counties, fill = "grey95") +
#   geom_sf(data = wi, fill = NA, lwd = .75) +
#   geom_sf(
#     data = stn_pts,
#     aes(color = glue("Point stations (n = {nrow(stn_pts)})")),
#     shape = 4) +
#   geom_sf(
#     data = stn_lines,
#     aes(color = glue("River sections (n = {nrow(stn_lines)})"))) +
#   scale_color_manual(values = c("darkred", "darkblue")) +
#   guides(
#     color = guide_legend(
#       title = NULL,
#       override.aes = list(
#         size = 3,
#         linetype = c(NA, 1),
#         shape = c(4, NA)))) +
#   theme_void() +
#   theme(
#     legend.position = c(.6, .93),
#     legend.justification = c(0, 1))

# county_counts <- all_pts %>%
#   st_set_geometry(NULL) %>%
#   count(county_name)
#
# ggplot() +
#   geom_sf(data = wi_counties) +
#   geom_sf(data = wi_counties %>%
#       left_join(county_counts), aes(fill = n), alpha = .5) +
#   geom_sf(data = wi, fill = NA, lwd = .75) +
#   geom_sf_text(data = wi_counties, aes(label = county_name), angle = 15, size = 2, alpha = .5) +
#   geom_sf(data = all_pts, shape = 24, fill = "orange", size = 4) +
#   scale_fill_viridis_c(na.value = "grey90") +
#   labs(fill = "Stations in area") +
#   theme_void() +
#   guides(
#     fill = guide_colorsteps(frame.colour = "black", show.limits = T)
#   )

# annual facets
# ggplot() +
#   geom_sf(data = wi_counties, fill = "grey95") +
#   geom_sf(data = wi, fill = NA, lwd = .75) +
#   geom_sf(
#     data = stn_pts_annual,
#     aes(color = glue("Point stations (n = {nrow(stn_pts)})")),
#     shape = 4) +
#   geom_sf(
#     data = stn_lines_annual,
#     aes(color = glue("River sections (n = {nrow(stn_lines)})"))) +
#   facet_wrap(~year_label, ncol = 6) +
#   scale_color_manual(values = c("darkred", "darkblue")) +
#   guides(
#     color = guide_legend(
#       title = NULL,
#       override.aes = list(
#         size = 3,
#         linetype = c(NA, 1),
#         shape = c(4, NA)))) +
#   theme_void() +
#   theme(legend.position = "bottom")


## Save image ----

save.image("site/project-red-summary.RData")

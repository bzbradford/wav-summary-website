
library(tidyverse)
library(sf)
library(leaflet)
library(janitor)
library(gt)
library(readxl)

rm(list = ls())


# Load data ----

# uses data directly from the swims queries
data_dir <- function(f) file.path("data_swims", f)

ais_in <- read_excel(data_dir("7_wav_RED_fw.xlsx"), na = c("", "NA")) %>%
  clean_names() %>%
  select(
    fsn = fieldwork_seq_no,
    group_seq_no,
    sample_header_seq_no,
    parameter_code = dnr_parameter_code,
    parameter_name = dnr_parameter_description,
    result = result_value_no,
    result_value = result_amt,
    datetime = start_date_time,
    fieldwork_comment,
    station_id
  )

names(ais_in)

stns <- read_excel(data_dir("2_wav_all_stns.xlsx")) %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    station_type = station_type_desc,
    latitude = calc_ll_lat_dd_amt,
    longitude = calc_ll_long_dd_amt,
    county_name,
    wbic,
    waterbody = official_waterbody_name,
  ) %>%
  filter(station_id %in% unique(ais_in$station_id))

names(stns)

ais_results <- ais_in %>%
  left_join(stns) %>%
  mutate(
    date = as_date(datetime),
    year = year(date),
    .after = datetime
  ) %>%
  mutate(across(c(latitude, longitude), ~ if_else(.x == 0, NA, .x))) %>%
  arrange(fsn, sample_header_seq_no, parameter_code) %>%
  distinct()

fieldwork_events <- ais_results %>%
  select(
    -sample_header_seq_no,
    -starts_with("dnr_parameter"),
    -starts_with("result")
  ) %>%
  distinct()

dnr_parameters <- ais_results %>%
  count(parameter_code, parameter_name, sort = T) %>%
  filter(n > 100)

results_wide <- ais_results %>%
  filter(parameter_code %in% dnr_parameters$parameter_code) %>%
  select(station_id, fsn, date, parameter_name, result) %>%
  distinct() %>%
  pivot_wider(names_from = parameter_name, values_from = result, values_fn = ~ paste(.x, collapse = ", "))


## interested parties ----

ais_groups <- read_excel(data_dir("1_wav_all_ip_groups.xlsx")) %>%
  clean_names() %>%
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

stn_pts <- stns %>%
  select(
    station_id,
    station_name,
    latitude,
    longitude,
    county_name,
    wbic,
    waterbody,
    station_type
  ) %>%
  filter(station_id %in% ais_results$station_id) %>%
  filter(latitude > 0, longitude < 0) %>%
  drop_na() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


load("data_project_red/REDlines.Rda")

# line_stns <- read_rds("data_swims/line_stns.rds") %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   clean_names() %>%
#   filter(station_id %in% ais_results$station_id)

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

stn_lines_annual <- stn_lines %>%
  left_join(annual_counts) %>%
  left_join(year_labels)

stn_pts_annual <- stn_lines %>%
  st_centroid() %>%
  st_join(select(wi_counties, county_name)) %>%
  bind_rows(stn_pts) %>%
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

save.image("project_red.RData")

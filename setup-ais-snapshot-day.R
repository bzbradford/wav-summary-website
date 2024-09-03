
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(leaflet.extras)
library(janitor)
library(gt)
library(DT)

rm(list = ls())

create_popups <- function(df) {
  df <- df %>% st_set_geometry(NULL)
  cols <- names(df)
  col_labels <- make_clean_names(cols, case = "sentence")
  lapply(1:nrow(df), function(r) {
    row <- df[r,]
    lapply(1:length(cols), function(c) {
      paste0("<b>", col_labels[c], ":</b> ", row[c])
    }) %>%
    paste0(collapse = "<br>")
  }) %>% paste0()
}

to_sf <- function(df) {
  df %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
}

str_wrap_html <- function(str) {
  str_wrap(str) %>% str_replace_all("\n", "<br>")
}

to_summary_list <- function(x, count = FALSE) {
  x <- na.omit(x)
  paste0(
    { if (count) paste0("(", n_distinct(x), ") ") },
    paste(sort(unique(x)), collapse = ", ")
  )
}

n_distinct2 <- function(...) {
  n_distinct(..., na.rm = TRUE)
}

plt_theme <- theme(
  axis.text.x = element_text(face = "bold", size = 10),
  panel.background = element_blank(),
  legend.position = "none"
)

to_dt <- function(.data) {
  .data %>%
    clean_names(case = "title") %>%
    datatable(
      .data,
      extensions = "Buttons",
      options = list(
        lengthMenu = c(5, 10, 25),
        dom = "Bfrtipl",
        buttons = c("copy", "csv", "excel")
      )
    )
}

to_gt <- function(.data) {
  .data %>%
    clean_names(case = "title") %>%
    gt() %>%
    tab_options(table.width = "100%")
}


# Load shapefiles ----

wi_counties <- readRDS("shp/counties.rds") %>%
  clean_names() %>%
  rename(county = county_name) %>%
  rmapshaper::ms_simplify(.1)

wi <- wi_counties %>%
  st_union()

watersheds <- read_rds("shp/dnr_watersheds.rds") %>%
  clean_names()



# WBIC sheet ----

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
    county = if_else(is.na(wbic), waterbody_name, NA) %>%
      str_replace_all("\\s\\(", "_") %>%
      str_split_i("_", 1) %>%
      str_replace_all(" County", ""),
    .after = waterbody_name
  ) %>%
  fill(county) %>%
  drop_na(wbic)

wbic_names <- wbic_ais %>% distinct(wbic, waterbody_name)

wbic_ais_county <- wbic_ais %>%
  reframe(
    invasive_species = paste(invasive_species, collapse = ", ") %>%
      str_split(", ") %>%
      unlist() %>%
      sort(),
    .by = c(county, wbic, waterbody_name)
  )


# Load data ----

snapshot_years <- 2014:2024
ais_results_in <-
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
  st_join(select(watersheds, dnr_watershed_code, dnr_watershed_name)) %>%
  st_drop_geometry()

# # sites missing WBICs
stns_without_wbic <- ais_results_in %>%
  filter(is.na(wbic)) %>%
  summarize(
    fieldwork_count = n_distinct2(fsn),
    years_monitored = to_summary_list(year),
    .by = c(station_id, station_name, station_type, latitude, longitude, county, wbic, waterbody_name)
  ) %>%
  arrange(latitude, longitude)

# use station_id when missing WBIC
ais_results <- ais_results_in %>%
  mutate(
    wbic = coalesce(wbic, station_id),
    waterbody_name = coalesce(waterbody_name, station_name)
  )

# only species IDs
ais_finds <- ais_results %>%
  filter(parameter_code == 20043) %>%
  drop_na(result)


## Fieldwork ----

fieldwork_events <- ais_results %>%
  select(-c("parameter_name", "parameter_code", "result")) %>%
  distinct()


## Volunteers ----

ais_groups <- ais_results %>%
  reframe(
    name = group_desc %>%
      str_split(" , ") %>% unlist() %>%
      str_split(", ") %>% unlist() %>%
      str_split(" and ") %>% unlist(),
    .by = c(year, fsn)
  ) %>%
  distinct() %>%
  mutate(group_size = n_distinct(name), .by = fsn)


## Make points ----

# all sampled locations
all_pts <- ais_results %>%
  distinct(
    station_id, station_name, station_type,
    latitude, longitude,
    wbic, waterbody_name
  ) %>%
  arrange(latitude, longitude) %>%
  to_sf()

# basically fieldwork pts, including when no ais was found
ais_pts <- ais_finds %>%
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


## Fieldwork by county ----

county_ais_summary <- {
  sumfn <- function(.data) {
    summarize(
      .data,
      years_monitored = to_summary_list(year),
      years = n_distinct2(year),
      watersheds = n_distinct2(dnr_watershed_code),
      waterbodies = n_distinct2(wbic),
      stations = n_distinct2(station_id),
      fieldwork = n_distinct2(fsn),
      .by = county
    )
  }

  found <- ais_finds %>% sumfn()
  looked_for <- fieldwork_events %>% sumfn()
  sp_list <- ais_finds %>%
    summarize(
      species = n_distinct2(result),
      species_found = to_summary_list(result),
      .by = county
    )

  bind_rows(found, looked_for) %>%
    distinct(county, .keep_all = T) %>%
    left_join(sp_list) %>%
    replace_na(list(species_found = "None", species = 0)) %>%
    arrange(county)
}


## Fieldwork by watershed ----

watershed_ais_summary <- {
  sumfn <- function(.data) {
    summarize(
      .data,
      years_monitored = to_summary_list(year),
      years = n_distinct2(year),
      waterbodies = n_distinct2(wbic),
      stations = n_distinct2(station_id),
      fieldwork = n_distinct2(fsn),
      .by = c(dnr_watershed_code, dnr_watershed_name)
    )
  }

  found <- ais_finds %>% sumfn()
  looked_for <- fieldwork_events %>% sumfn()
  sp_list <- ais_finds %>%
    summarize(
      species = n_distinct2(result),
      species_found = to_summary_list(result),
      .by = c(dnr_watershed_code, dnr_watershed_name)
    )

  bind_rows(found, looked_for) %>%
    distinct(dnr_watershed_code, dnr_watershed_name, .keep_all = T) %>%
    left_join(sp_list) %>%
    replace_na(list(species_found = "None", species = 0))
}


## New vs existing AIS identifications ----

new_ais_by_site <- ais_finds %>%
  distinct(station_id, year, result) %>%
  mutate(first_report = min(year), .by = c(station_id, result)) %>%
  mutate(id_type = if_else(year == first_report, "new", "existing")) %>%
  summarize(sites = n_distinct(station_id), .by = c(result, year, id_type)) %>%
  mutate(total_annual_sites = sum(sites), .by = c(result, year))

new_ais_by_waterbody <- ais_finds %>%
  drop_na(wbic) %>%
  distinct(wbic, waterbody_name, station_id, year, result) %>%
  mutate(first_report = min(year), .by = c(wbic, result)) %>%
  mutate(id_type = if_else(year == first_report, "new", "existing")) %>%
  summarize(
    waterbodies = n_distinct(wbic),
    waterbody_codes = to_summary_list(wbic),
    waterbody_names = to_summary_list(waterbody_name),
    .by = c(result, year, id_type)) %>%
  mutate(total_annual_waterbodies = sum(waterbodies), .by = c(result, year))

new_ais_by_watershed <- ais_finds %>%
  distinct(dnr_watershed_code, dnr_watershed_name, year, result) %>%
  mutate(first_report = min(year), .by = c(dnr_watershed_code, result)) %>%
  mutate(id_type = if_else(year == first_report, "new", "existing")) %>%
  summarize(
    watersheds = n_distinct(dnr_watershed_code),
    watershed_codes = to_summary_list(dnr_watershed_code, count = F),
    watershed_names = to_summary_list(dnr_watershed_name, count = F),
    .by = c(result, year, id_type)) %>%
  mutate(total_annual_watersheds = sum(watersheds), .by = c(result, year))

new_ais_by_county <- ais_finds %>%
  distinct(county, station_id, year, result) %>%
  mutate(first_report = min(year), .by = c(county, result)) %>%
  mutate(id_type = if_else(year == first_report, "new", "existing")) %>%
  summarize(
    counties = n_distinct(county),
    county_names = paste(sort(unique(county)), collapse = ", "),
    .by = c(result, year, id_type)) %>%
  mutate(total_annual_counties = sum(counties), .by = c(result, year))


## DNR parameters ----

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


## Export data ----

ais_results %>%
  write_csv(str_glue("exports/Snapshot day - all results {Sys.Date()}.csv"))
ais_finds %>%
  write_csv(str_glue("exports/Snapshot day - positive finds {Sys.Date()}.csv"))
ais_groups %>%
  write_csv(str_glue("exports/Snapshot day - volunteer names {Sys.Date()}.csv"))


## Save image ----

save.image("site/snapshot-day-summary.RData")


library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(readxl)

rm(list = ls())


# Load data ----

results <- readxl::read_excel("data_swims/6_wav_hab_fw.xlsx") %>%
  clean_names() %>%
  select(
    fsn = fieldwork_seq_no,
    gsn = group_seq_no,
    msn = monit_station_seq_no,
    datetime = start_date_time,
    parameter_code = dnr_parameter_code,
    parameter_description = dnr_parameter_description,
    result_value = result_amt,
    dyn_form_code,
    fieldwork_comment
  ) %>%
  mutate(
    date = as_date(datetime),
    year = year(date),
    month = month(date),
    .after = datetime
  ) %>%
  mutate(
    parameter_label = paste(parameter_code, parameter_description),
    .before = result_value
  ) %>%
  drop_na(result_value)

# submitters by fieldwork
# ip_groups
people <- read_excel("data_swims/1_wav_all_ip_groups.xlsx") %>%
  clean_names() %>%
  select(
    gsn = group_seq_no,
    last_name,
    first_name,
    organization_name,
    primary_email
  ) %>%
  filter(gsn %in% unique(results$gsn)) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(full_name = paste(first_name, last_name), .before = last_name)


stns <- read_excel("data_swims/2_wav_all_stns.xlsx") %>%
  clean_names() %>%
  select(
    msn = monit_station_seq_no,
    station_id,
    station_name = primary_station_name,
    latitude = calc_ll_lat_dd_amt,
    longitude = calc_ll_long_dd_amt,
    wbic,
    waterbody = official_waterbody_name
  ) %>%
  filter(msn %in% results$msn)

wi_counties <- readRDS("shp/counties.rds") %>%
  janitor::clean_names()

stns.sf <- stns %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(wi_counties, dnr_region, county_name, geometry))

stn_counties <- stns.sf %>%
  st_set_geometry(NULL) %>%
  select(station_id, dnr_region, county_name)



# Fieldwork info ----

fieldwork_info <- results %>%
  distinct(fsn, gsn, msn, datetime, date, year, month, fieldwork_comment) %>%
  left_join(stns) %>%
  left_join(stn_counties) %>%
  arrange(fsn)

fieldwork_times <- results %>%
  distinct(fsn, datetime, date, year, month)

# should show equal numbers if no duplicates
fieldwork_times %>% summarize(n = n(), fsn = n_distinct(fsn))

hab_annual_counts <- fieldwork_info %>%
  group_by(year) %>%
  summarize(
    n_stations = n_distinct(station_id),
    n_events = n_distinct(fsn),
    .groups = "drop")

min_year <- min(fieldwork_info$year)
max_year <- max(fieldwork_info$year)
n_stns <- n_distinct(fieldwork_info$station_id)
n_events <- n_distinct(fieldwork_info$fsn)
stn_fw_years <- fieldwork_info %>%
  count(station_id, year) %>%
  count(station_id, name = "years") %>%
  count(years) %>%
  mutate(pct = n / sum(n), cum_pct = cumsum(pct))
stn_fw_years_top2 <- scales::percent(sum(head(stn_fw_years$pct, 2)))



# Data submitters ----

## fieldwork submitter info ##

hab_submitters <- people %>%
  left_join(fieldwork_info, relationship = "many-to-many") %>%
  group_by(across(full_name:organization_name)) %>%
  summarize(
    n_years = n_distinct(year),
    n_fieldwork = n_distinct(fsn),
    .groups = "drop"
  ) %>%
  arrange(desc(n_fieldwork))

hab_orgs <- people %>%
  left_join(fieldwork_info, relationship = "many-to-many") %>%
  replace_na(list(organization_name = "None/Unknown")) %>%
  group_by(organization_name) %>%
  summarize(
    number_of_volunteers = n_distinct(full_name),
    years_active = n_distinct(year),
    fieldwork_events = n_distinct(fsn),
    most_recent_year = max(year)) %>%
  arrange(desc(fieldwork_events))

n_orgs <- n_distinct(hab_orgs$organization_name) - 1
n_submitters <- n_distinct(hab_submitters$full_name)
n_solo_submitters <- hab_submitters %>%
  filter(is.na(organization_name)) %>%
  nrow()



# Results ----

## Result data
hab_data <- results %>%
  left_join(stns) %>%
  arrange(fsn, parameter_code) %>%
  drop_na(
    result_value,
    datetime, # fieldwork wasn't listed in the interested parties export
    station_id # fieldwork wasn't listed in the station export
  )

# hab_data %>%
#   filter(is.na(station_id) | is.na(datetime)) %>%
#   select(fsn, station_id, station_name, datetime) %>%
#   distinct(fsn, .keep_all = T) %>%
#   mutate(link = glue::glue("https://apps.dnr.wi.gov/swims/Fieldwork/FieldworkDetails?id={fsn}")) %>%
#   write_csv("Unjoined FSNs.csv")

hab_data_more10 <- hab_data %>%
  filter(grepl("MORE10", dyn_form_code, fixed = T))

hab_fsn_more10 <- sort(unique(hab_data_more10$fsn))

hab_data_less10 <- hab_data %>%
  filter(grepl("LESS10", dyn_form_code, fixed = T))

hab_fsn_less10 <- sort(unique(hab_data_less10$fsn))

# fsn where not all LESS10 fields were filled out (incomplete data)
hab_fsn_less10_incomplete <- hab_data_less10 %>%
  filter(parameter_code %in% c(4227:4232)) %>%
  count(fsn, parameter_code) %>%
  count(fsn) %>%
  filter(n < 6) %>%
  pull(fsn)

# complete data
hab_data_less10_complete <- hab_data_less10 %>%
  filter(!(fsn %in% hab_fsn_less10_incomplete))

hab_fsn_less10_complete <- sort(unique(hab_data_less10_complete$fsn))

# assign word scores to numeric scores
# hab_data %>%
#   filter(dnr_parameter_code %in% 4227:4236) %>%
#   distinct(dnr_parameter_code, dnr_parameter_description, result_value) %>%
#   arrange(dnr_parameter_code, result_value) %>%
#   write_csv("habitat-assessment-score-types.csv")

# should have created the csv below with `score_name` column
hab_score_names <- read_csv("data_habitat/habitat-assessment-score-reference.csv") %>%
  select(-parameter_description)

# for plot and some summary stuff
hab_data_named_scores <- hab_data_less10_complete %>%
  filter(parameter_code %in% 4227:4237) %>%
  mutate(across(parameter_description, ~ gsub(".{1}$", "", .x))) %>%
  left_join(hab_score_names, join_by(parameter_code, result_value)) %>%
  mutate(score_name = if_else(
    parameter_code == 4237,
    case_match(
      result_value,
      0:19 ~ "Poor",
      20:59 ~ "Fair",
      60:79 ~ "Good",
      80:100 ~ "Excellent"
    ),
    score_name
  )) %>%
  drop_na(score_name) %>%
  mutate(score_name = factor(score_name, levels = c("Poor", "Fair", "Good", "Excellent")))


# Mapping -----------------------------------------------------------------

county_totals <- fieldwork_info %>%
  summarize(
    n_stations = n_distinct(station_id),
    n_events = n_distinct(fsn),
    .by = c(dnr_region, county_name)
  ) %>%
  arrange(desc(n_stations), county_name)

# for ggplot
wi_counties_hab_counts <- wi_counties %>%
  select(region = dnr_region, county_name, geometry) %>%
  left_join(county_totals, join_by(county_name)) %>%
  mutate(across(county_name, ~paste(.x, "County")))

# for leaflet
create_popup <- function(data, title) {
  data %>% {
    cols <- names(.)
    lapply(1:nrow(.), function(r) {
      row <- .[r,]
      details <-
        lapply(1:length(cols), function(c) {
          paste0("<br><b>", str_to_title(cols[c]), ":</b> ", row[c])
        }) %>%
        paste0(collapse = "")
      paste0(title, details)
    }) %>% paste0()
  }
}

hab_data_wide <- hab_data %>%
  filter(parameter_code %in% 4227:4237) %>%
  select(fsn, name = parameter_label, value = result_value) %>%
  drop_na() %>%
  pivot_wider(values_fn = ~ paste(.x, collapse = " // "))

fw_points <- fieldwork_info %>%
  left_join(hab_data_wide) %>%
  drop_na(latitude, longitude) %>%
  mutate(
    datetime = as.character(datetime),
    date = format(date, "%b %d, %Y"),
    popup = create_popup(pick(everything()), "<b>==== Habitat assessment ====</b>"),
    label = glue::glue("<b>{date}</b> <i>{fieldwork_comment}</i>", .na = "No comment"),
    label = gsub("\n", "<br>", str_wrap(label, 60), fixed = T)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stn_points <- fw_points %>%
  distinct(station_id, station_name, geometry) %>%
  mutate(label = paste(station_id, station_name))

leaflet_counties <- wi_counties %>%
  left_join(county_totals) %>%
  mutate(label = glue::glue("
    <b>{county_name} County</b><br>
    <i>{dnr_region}</i><br>
    {if_else(is.na(n_stations), 0, n_stations)} stations<br>
    {if_else(is.na(n_events), 0, n_events)} habitat assessments"))

leaflet_pal <- colorBin("YlOrRd", domain = county_totals$n_events, bins = 5)


# Save image ----

save.image("habitat.RData")

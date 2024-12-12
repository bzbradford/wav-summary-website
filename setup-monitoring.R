
# Load and process data for the annual monitoring summary doc

library(tidyverse)
library(sf)
library(leaflet)

rm(list = ls())


cur_year <- 2024


# Load data ----

## Stations and monitoring data ----
# Update these from the WAV Dashboard data prep project
stns <- readRDS("data-monitoring/station-list.rds")
baseline <- readRDS("data-monitoring/baseline-data.rds")
nutrient <- readRDS("data-monitoring/tp-data.rds")
therm <- readRDS("data-monitoring/therm-data.rds")
therm_info <- readRDS("data-monitoring/therm-inventory.rds")


## Shapefiles ----

nkes <- readRDS("shp/nkes.rds")
counties <- readRDS("shp/counties.rds") %>%
  select(county_name = CountyName, geometry)
state <- st_union(counties)
wi_bbox <- state %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(10000) %>%
  st_bbox()
huc8 <- readRDS("shp/huc8.rds") %>% st_crop(wi_bbox)
huc10 <- readRDS("shp/huc10.rds") %>% st_crop(wi_bbox)
huc12 <- readRDS("shp/huc12.rds") %>% st_crop(wi_bbox)
n_huc8 <- nrow(huc8)
n_huc10 <- nrow(huc10)
n_huc12 <- nrow(huc12)



# Create nearshore watersheds ----

# extend great lakes watersheds by 1km
great_lakes <- huc12 %>%
  filter(Huc12Name %in% c("Lake Michigan", "Lake Superior")) %>%
  st_transform(crs = 3070) %>%
  st_buffer(1000) %>%
  st_transform(crs = 4326) %>%
  select(geometry) %>%
  mutate(Nearshore = T)

# leaflet() %>% addTiles() %>% addPolygons(data = great_lakes)

huc8_nearshore <- huc8 %>%
  filter(!(Huc8Name %in% c("Lake Michigan", "Lake Superior"))) %>%
  st_join(great_lakes) %>%
  filter(Nearshore)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = filter(huc10_nearshore, Nearshore),
#     label = ~Huc10Name)

huc10_nearshore <- huc10 %>%
  filter(!(Huc10Name %in% c("Lake Michigan", "Lake Superior", "Nodaway Point-Frontal Lake Superior"))) %>%
  st_join(great_lakes) %>%
  filter(Nearshore)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = filter(huc10_nearshore, Nearshore),
#     label = ~Huc10Name)

huc12_nearshore <- huc12 %>%
  filter(!(Huc12Name %in% c("Lake Michigan", "Lake Superior", "Isle Royale"))) %>%
  st_join(great_lakes) %>%
  filter(Nearshore)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = filter(huc12_nearshore, Nearshore),
#     label = ~Huc12Name)



# Process data ----

getStns <- function(df, name = deparse(substitute(df))) {
  df %>%
    distinct(station_id, latitude, longitude) %>%
    mutate(name = name)
}

getStnsByYear <- function(df, name = deparse(substitute(df))) {
  df %>%
    distinct(year, station_id, latitude, longitude) %>%
    mutate(name = name)
}

getPts <- function(df) {
  df %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_join(counties) %>%
    st_join(huc12) %>%
    relocate(geometry, .after = everything()) %>%
    select(-Area)
}

cur_baseline <- filter(baseline, year == cur_year)
cur_nutrient <- filter(nutrient, year == cur_year)
cur_therm <- filter(therm, year == max(year))

# all stations monitored in the dataset
all_pts <-
  bind_rows(
    getStns(baseline),
    getStns(nutrient),
    getStns(therm)) %>%
  mutate(value = T) %>%
  pivot_wider(values_fill = F) %>%
  mutate(
    any = baseline | nutrient | therm,
    cur_baseline = station_id %in% cur_baseline$station_id,
    cur_nutrient = station_id %in% cur_nutrient$station_id,
    cur_therm = station_id %in% cur_therm$station_id,
    cur_any = cur_baseline | cur_nutrient | cur_therm
  ) %>%
  drop_na(latitude, longitude) %>%
  getPts() %>%
  # nke plans overlap to have to dedupe
  st_join(select(nkes, NkePlan = PlanId, geometry)) %>%
  distinct(station_id, .keep_all = T) %>%
  st_join(select(huc8_nearshore, Huc8Nearshore = Nearshore, geometry)) %>%
  st_join(select(huc10_nearshore, Huc10Nearshore = Nearshore, geometry)) %>%
  st_join(select(huc12_nearshore, Huc12Nearshore = Nearshore, geometry))

all_stns <- st_set_geometry(all_pts, NULL)

all_stns_by_year <-
  bind_rows(
    getStnsByYear(baseline),
    getStnsByYear(nutrient),
    getStnsByYear(therm)) %>%
  mutate(value = T) %>%
  pivot_wider(values_fill = F) %>%
  mutate(any = baseline | nutrient | therm) %>%
  left_join(select(all_stns, station_id, county_name:Huc12Nearshore), join_by(station_id)) %>%
  arrange(year, station_id)

all_pts_by_year <- all_stns_by_year %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Get volunteer counts ----

getUniqueNames <- function(str) {
  str <- str %>%
    na.omit() %>%
    str_replace_all(" and ", ",") %>%
    str_replace_all(" & ", ",") %>%
    str_replace_all(" - ", ",") %>%
    str_replace_all("_\\d+\\s?", "") %>%
    paste(collapse = ", ") %>%
    str_split_1(",") %>%
    sapply(str_trim) %>%
    sapply(str_to_title) %>%
    unique() %>%
    sort()
  str[nzchar(str)]
}

baseline_vols <- baseline %>%
  group_by(year) %>%
  summarize(names = list(getUniqueNames(group_desc)), .groups = "drop") %>%
  mutate(n = sapply(names, length))

nutrient_vols <- nutrient %>%
  group_by(year) %>%
  summarize(names = list(getUniqueNames(volunteer_name)), .groups = "drop") %>%
  mutate(n = sapply(names, length))

therm_vols <- therm_info %>%
  group_by(year) %>%
  summarize(names = list(getUniqueNames(contact_name)), .groups = "drop") %>%
  mutate(n = sapply(names, length))

all_vols <-
  bind_rows(
    select(baseline, year, name = group_desc),
    select(nutrient, year, name = volunteer_name),
    select(therm_info, year, name = contact_name)
  ) %>%
  group_by(year) %>%
  summarize(names = list(getUniqueNames(name)), .groups = "drop") %>%
  mutate(n = sapply(names, length))

# export names
baseline_vols %>%
  reframe(name = unlist(names), .by = c(year, n)) %>%
  arrange(desc(year)) %>%
  write_csv("exports/volunteer names - baseline.csv")

nutrient_vols %>%
  reframe(name = unlist(names), .by = c(year, n)) %>%
  arrange(desc(year)) %>%
  write_csv("exports/volunteer names - nutrient.csv")

therm_vols %>%
  reframe(name = unlist(names), .by = c(year, n)) %>%
  arrange(desc(year)) %>%
  write_csv("exports/volunteer names - thermistor.csv")

all_vols %>%
  reframe(name = unlist(names), .by = c(year, n)) %>%
  arrange(desc(year)) %>%
  write_csv("exports/volunteer names.csv")



# Map helpers --------------------------------------------------------------

addBasemaps <- function() {
  list(
    geom_sf(data = counties, fill = alpha("grey", .05), color = alpha("grey", .5)),
    geom_sf(data = state, fill = NA, color = "grey", linewidth = .5)
  )
}

addWatersheds <- function(shp) {
  list(
    geom_sf(data = shp, color = alpha("steelblue", .2), fill = alpha("lightsteelblue", .2))
  )
}

addCurPastPts <- function(cur_pts, past_pts, pt_color, title) {
  list(
    geom_sf(data = past_pts, aes(color = "Previous years"), size = .4),
    geom_sf(data = cur_pts, aes(color = "Current year"), shape = 21, size = 2.5, fill = pt_color),
    scale_color_manual(values = c("black", "black")),
    scale_fill_viridis_c(na.value = "grey90", limits = c(0, NA), option = "cividis"),
    labs(
      title = sprintf("%s (%s)", title, cur_year),
      fill = "Stations in area",
      color = "Station locations"),
    guides(
      fill = guide_colorsteps(frame.colour = "black", show.limits = T),
      color = guide_legend(override.aes = list(
        size = c(4, 2),
        shape = c(21, 16)))),
    theme_void(),
    theme(legend.position = "right", plot.title = element_text(hjust = .5))
  )
}

addPtsInOut <- function(pts_in, pts_out, pt_color, title, yr = cur_year) {
  list(
    geom_sf(data = pts_out, aes(color = "Outside area"), fill = pt_color, size = .4),
    geom_sf(data = pts_in, aes(color = "Inside area"), fill = pt_color, shape = 21, size = 2.5),
    scale_color_manual(values = c("black", "black")),
    scale_fill_viridis_c(na.value = "grey90", limits = c(0, NA), option = "viridis"),
    labs(
      title = sprintf("%s (%s)", title, yr),
      fill = "Stations in area",
      color = "Station locations"),
    guides(
      color = guide_legend(override.aes = list(
        size = c(4, 2),
        shape = c(21, 16))),
      fill = guide_colorsteps(frame.colour = "black", show.limits = T)
    ),
    theme_void(),
    theme(legend.position = "right", plot.title = element_text(hjust = .5))
  )
}


# Save data image ----

save.image("site/annual-monitoring-summary.RData")

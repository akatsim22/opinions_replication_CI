##########
# CREATE IV RESIDUALS MAP
##########


source('code/analysis/load.R')
library(tmap)
library(RColorBrewer)
library(sf)
library(cutr)
library(extrafont)

sunsets = read_dta('data/working/sunset.dta') %>%
  filter(elapdate=='2020-01-01') %>% dplyr::select(fipscode, sunset_time) %>%
  mutate(sunset_time = ymd_hm(paste0('2020-02-01 ', sunset_time)))

data = data %>% filter(elapdate=='2020-03-01')

shp = st_read("data/GIS/cb_2018_us_county_5m/cb_2018_us_county_5m.shp") %>%
  mutate(fipscode = as.numeric(paste0(STATEFP, COUNTYFP)))

model = lm(as.formula(str_interp('${iv}~${base_iv}+${fullcontr}')), data=data, na.action="na.exclude")

data$residuals = resid(model)


shp = shp %>% 
  inner_join(data %>% dplyr::select(fipscode, residuals), by='fipscode') %>%
  inner_join(sunsets, by='fipscode') %>%
  mutate(residual_bin = smart_cut(residuals, i=7, what='groups')) %>%
  filter(!is.na(residual_bin))

aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
map_iv = tm_shape(shp, projection=aea) +
  tm_polygons(col='residual_bin',  palette = rev(brewer.pal(7, 'RdGy')) , midpoint=NA, title = 'Instrument residual') +
  tm_layout(frame = FALSE) +
  tm_layout(fontfamily='LM Roman 10')
tmap_save(map_iv, 'output/figures/map-iv-resid.png', width=8, height=5)

### BNAM data
require(purrr)
require(lubridate)
require(sf)
require(stars)
require(raster)
require(dplyr)


BNAM_raster_asc <- function(year, month, data_type="BtmTemp") {
  BNAM <- raster(paste0("Y:/Projects/OceanographicData/BNAM/Grids/BNAM_1990-2019_monthly/", year, "/", data_type, "_", month, "_", year, ".asc"))
}

df <- data.frame(expand.grid(year= 1990:2019, month = month.abb))
BNAM <- purrr::pmap(df, BNAM_raster_asc)
# for all years/months this is a 4.7Mb list

bbox <- st_bbox(c(xmax=-65, xmin=-70, ymin=41, ymax=43), crs=4326)

crop_BNAM <- function(obj) {
  BNAM_crop <- st_as_stars(obj, crs=4326) %>%
    st_crop(bbox)
}

BNAM_crop <- map(BNAM, crop_BNAM)

save(BNAM_crop, file = "Data/BNAM_BtmTemp_GB_1990-2019.RData")
##########

load("BNAM_BtmTemp_GB_1990-2019.RData")

BT_dat <- map_df(1:nrow(df), function(x) data.frame(year=df$year[x], month=df$month[x], BT = BNAM_crop[[x]][[1]])) %>% tidyr::pivot_longer(cols=starts_with("BT."))

monthly_avg <- BT_dat %>%
  group_by(year, month) %>%
  summarize(BT = mean(value, na.rm=T)) %>%
  arrange(year, month) %>%
  ungroup() %>%
  mutate(monthnum = 1:360)

a <- ggplot() + geom_line(data=monthly_avg, aes(month, BT, group=year, colour=year)) +
  geom_smooth(data=monthly_avg, aes(month, BT, group=1), colour="black", lwd=2)

b <- ggplot() + geom_line(data=monthly_avg, aes(monthnum, BT)) +
  geom_smooth(data=monthly_avg, aes(monthnum, BT, group=1), colour="black", lwd=2)

yearly_avg <- BT_dat %>%
  group_by(year) %>%
  summarize(BT = mean(value, na.rm=T)) %>%
  arrange(year) 

c <- ggplot() + geom_line(data=yearly_avg, aes(year, BT))

  
require(patchwork)

a / b / c

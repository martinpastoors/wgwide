
library(maps)
library(mapdata)
library(lubridate)
library(tidyverse)
library(viridis)
encode_zchords <- function(x, y, dx = 1, dy = 0.5 * dx, invalids = TRUE) {
  
  x.brks <- seq(floor(min(x)),ceiling(max(x)),dx)
  x.ints <- findInterval(x, x.brks, all.inside = TRUE)
  x <- (x.brks[x.ints] + x.brks[x.ints + 1]) / 2
  
  y.brks <- seq(floor(min(y)),ceiling(max(y)),dy)
  y.ints <- findInterval(y, y.brks, all.inside = TRUE)
  y <- (y.brks[y.ints] + y.brks[y.ints + 1]) / 2
  
  if(invalids) {
    x <- ifelse(x >= -180 & x <= 180, x, NA)
    y <- ifelse(y >= -90  & y <= 90 , y, NA)
  }
  
  return(paste(round(x,6), round(y,6), sep = ":"))
  
}

load(url("http://www.hafro.is/~einarhj/data/nsibts_tidy.rda"))
le <- 
  tidy_le %>%
  select(id, latin, length, n) %>% 
  group_by(id, latin, length) %>% 
  summarise(n = n())
st <- 
  tidy_st %>% 
  select(id, date, lon = shootlong, lat = shootlat) %>% 
  mutate(year = year(date))

xlim <- range(st$lon)
ylim <- range(st$lat)
m <- map_data("worldHires", xlim = xlim, ylim = ylim)
ns <-
  m %>% 
  ggplot() +
  theme_bw() +
  geom_polygon(data = m, aes(long, lat, group = group), fill = "grey") +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_x_continuous(NULL, NULL) +
  scale_y_continuous(NULL, NULL)


df <-
  le %>% 
  mutate(b = n * 0.01 * length^3) %>%
  # filter(latin %in% c("Gadus morhua",
  #                     "Pleuronectes platessa",
  #                     "Melanogrammus aeglefinus",
  #                     "Scomber scombrus",
  #                     "Clupea harengus",
  #                     "Merlangius merlangus",
  #                     "Pollachius virens",
  #                     "Limanda limanda")) %>% 
  filter(latin %in% c("Trachurus trachurus")) %>% 
  group_by(id, latin) %>% 
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>% 
  right_join(st) %>% 
  mutate(year = year(date),
         N = ifelse(is.na(N), 0, N),
         B = ifelse(is.na(b), 0, b),
         sq = encode_zchords(lon, lat, dx = 1)) %>% 
  filter(year >= 1990) %>% 
  group_by(sq, year, latin) %>% 
  summarise(N = mean(N),
            B = mean(N)) %>% 
  separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>% 
  filter(!is.na(latin))
ns +
  geom_raster(data = df, aes(lon, lat, fill = B)) +
  scale_fill_viridis(option = "B", direction = -1) +
  facet_wrap(~ year, ncol = 8)


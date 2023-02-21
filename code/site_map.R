# script to produce a site map for vps study (fig. 1)

# load libraries
library(tidyverse)
library(sp)
library(sf)
library(rgdal)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(ggrepel)
library(ggstance) # don't think I'm using this
library(ocedata) 
library(oce)
library(data.table)
library(ncdf4)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(patchwork)
library(ggspatial)

# load the data
nsw_coast <- st_as_sf(readOGR(dsn = "data_raw/clipped_port_stephens.shp"))
habitat <- st_as_sf(readOGR(dsn = "data_raw/taylors_beach_habitat.shp"))
mpa <- st_as_sf(readOGR(dsn = "data_raw/NSW_Marine_Protected_Areas.shp"))
syncref.data <- read_csv("data_raw/Daniel Hewitt/VPS-TaylorsBeach-MudCrab-01-Results-20201130/results/syncref/all.csv")
oyster.leases <- st_as_sf(readOGR("data_raw/Aquaculture_Leases.shp"))
australia <- ne_states(country = "australia", returnclass = "sf")

receivers <- st_as_sf(readOGR(dsn = "data_raw/vps_receivers.shp")) %>% mutate(Gear = if_else(Gear == "VR2 and sync tag", "Receiver", Gear))

nsw_coast <- nsw_coast %>% mutate(water = if_else(code_coast == 1000, "Soft sediment", "Land"))

mpa <- mpa %>% filter(ZONE_TYPE == "Sanctuary Zone (IUCN II)")

# port stephens
inset <- ggplot() +
  geom_sf(data = nsw_coast, aes(fill = water)) +
  geom_sf(data = mpa, aes(fill = ZONE_TYPE, colour = ZONE_TYPE)) +
  scale_colour_manual(values = c("red")) +
  scale_fill_manual(values = c("light grey", "red", "white")) +
  geom_rect(aes(xmin = 152.05, xmax = 152.06, ymin = -32.75, ymax = -32.744), fill = NA, colour = "black") +
  coord_sf(xlim = c(151.95, 152.2), ylim = c(-32.76, -32.625)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        panel.background = element_rect(colour = "white")) +
  geom_segment(aes(x = 152.054, xend = 152.054, y = -32.700, yend = -32.735),
               arrow = arrow(length = unit(0.5, "cm")), colour = "black", size = 1.5)

# fenninghams island creek
a <- ggplot() +
  geom_sf(data = nsw_coast, aes(fill = water)) +
  geom_sf(data = habitat, aes(fill = HABITAT)) +
  geom_sf(data = oyster.leases, fill = NA, lwd = 1, colour = "dark grey") +
  geom_sf(data = receivers, aes(shape = Gear), size = 2) +
  coord_sf(xlim = c(152.05, 152.06), ylim = c(-32.75, -32.744)) +
  scale_fill_manual(breaks = c("Land", "Soft sediment", "Seagrass", "Mangrove", "Saltmarsh", "Phragmites"),
                    values = c("light grey", "light yellow", "seagreen1", "seagreen", "tan2", "yellow")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(colour = "white"),
        legend.position = c(0.9, 0.3)) +
  annotation_custom(grob = ggplotGrob(inset), xmin = 152.056, xmax = 152.06, ymax = -32.744, ymin = -32.746) +
  labs(fill = "Habitat", shape = "Array") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering)

# australia
b <- ggplot() +
  geom_sf(data = australia) +
  geom_point(aes(x = 152.191397 , y = -32.708031), size = 4, col = "white", shape = 23, fill = "black", stroke = 1) +
  coord_sf(xlim = c(142, 154), ylim = c(-42, -10),
           label_axes = list(right = "N")) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(size = 12, colour = "black")) +
  scale_y_continuous(position = "right") +
  labs(y = "Latitude")

final.map <- (a|b) + plot_annotation(tag_levels = "a")

ggsave("figures/final_map.jpeg", plot = final.map, device = "jpeg", width = 28.7, height = 20, units = "cm", dpi = 300)

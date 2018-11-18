library(tidyverse); library(rvest)
library(png)
library(gganimate)

#Get Sea Ice data from NASA ----
url <- "https://climate.nasa.gov/system/internal_resources/details/original/1270_minimum_extents_and_area_north_SBA_reg_20171001_2_.txt"

nasa.seaice <- read_table2(url,skip = 3)

nasa.seaice1 <- nasa.seaice %>% 
  select(Year = YR, Ice_Extent = ICE) %>% 
  filter(Year >= 1980) %>% 
  mutate(Ice_Extent_Perc = Ice_Extent / first(Ice_Extent))

#Polar Bear Image ----
pb.pic.raw <- readPNG("data/PolarBear.png")

pb.pic <- pb.pic.raw[,,4] %>% 
  as.data.frame() %>% 
  mutate(y = n()-row_number()) %>% 
  gather(x, value, 1:(ncol(.)-1)) %>% 
  mutate(x = as.numeric(str_remove_all(x, "V"))) %>% 
  filter(value > 0) %>% 
  #Rescale to 50% of coords
  group_by(x = x %/% 4, y = y %/% 4) %>% 
  summarize(value = max(value)) %>% 
  ungroup() %>% 
  mutate(cluster = kmeans(.[, c(1:2)], 100)$cluster)

pb.pic %>% ggplot(aes(x=x,y=y)) +
  geom_raster() +
  coord_fixed()

#PLot it -----
pb.slices <- sample(1:100, 100, replace = FALSE)

pb.data <- nasa.seaice1 %>% 
  mutate(plot = map(Ice_Extent_Perc, function(comp){
    # keep_k <- pb.slices[1:round(comp*100)]
    keep_k <- 1:round(comp*100)
    
    return(pb.pic %>% filter(cluster %in% keep_k))
  })) %>% 
  unnest(plot)

trex.labels <- trex.specimens %>% 
  mutate(label_p = paste0(round(comp), "%"),
         label_d = paste0("Discovered: ", Discovered, "\n",
                          Location))


pb.data %>% 
  filter(Year %in% seq(1980, 2017, by = 5)) %>% 
  ggplot(aes(x=x, y=y, fill = Ice_Extent_Perc)) +
  geom_raster() +
  scale_fill_gradient(low = "#ff4040", high = "#00436b") +
  # scale_fill_manual(values = c("#2dd49c", "#ff4040", "#5384ff", "#ff25ab", "#ff6141", "#ff9e53")) +
  coord_fixed() +
  # geom_text(data = trex.labels, aes(label = label_p), 
  #           x = 30, y = 30, size = 5, 
  #           color = "#00436b", fontface = "bold") +
  # geom_text(data = trex.labels, aes(label = label_d),
  #           x = 400, y = 40, size = 3) +
  facet_wrap(~Year, ncol = 4) +
  labs(title = "Arctic sea ice minimum by year",
       subtitle = "% of 1980 levels",
       caption = paste0("Data: https://climate.nasa.gov/vital-signs/arctic-sea-ice/\n",
                        "Image: phylopic.org\n",
                        "Ryan Timpe .com")) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom"
  )

pb.data %>% 
  filter(Year %in% seq(1980, 2017, by = 5)) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_raster() +
  # scale_fill_gradient(low = "#ff4040", high = "#00436b") +
  coord_fixed() +
  # geom_text(label = '{frame_state}',
  #           x = 30, y = 30, size = 5,
  #           color = "#00436b", fontface = "bold") +
  # geom_text(data = trex.labels, aes(label = label_d),
  #           x = 400, y = 40, size = 3) +
  # facet_wrap(~Year, ncol = 8) +
  labs(title = "Arctic sea ice minimum by year",
       subtitle = "% of 1980 levels",
       caption = paste0("Data: https://climate.nasa.gov/vital-signs/arctic-sea-ice/\n",
                        "Image: phylopic.org\n",
                        "Ryan Timpe .com")) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom"
  )  +
  transition_states(
    Year, 0.1, 3
  ) 
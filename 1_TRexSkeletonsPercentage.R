library(tidyverse); library(rvest)
library(png)

#Get Specimen table from Wikipedia ----
wiki <- read_html("https://en.wikipedia.org/wiki/Specimens_of_Tyrannosaurus")

trex.specimens <- wiki %>% 
  html_node("table") %>% 
  html_table() %>% 
  #Clean up % columns'
  mutate(comp = str_replace_all(`%`,"<", ""),
         comp = as.numeric(ifelse(nchar(comp) > 2, 
                                  (as.numeric(substr(comp, 1, 2)) + as.numeric(substr(comp, 4, 5)))/2,
                                  comp))) %>% 
  top_n(12, comp) %>% 
  #Clean name
  rename(Name = `Name(Spec #)`) %>% 
  mutate(Name = str_remove(Name, "\n"),
         Name = str_replace(Name, "\\(", " \n\\("),
         Name = factor(Name, levels = Name))

#Get TRex pic ----
trex.pic.raw <- readPNG("data/Tyrannosaurus.png")

trex.pic <- trex.pic.raw[,,4] %>% 
  as.data.frame() %>% 
  mutate(y = n()-row_number()) %>% 
  gather(x, value, 1:(ncol(.)-1)) %>% 
  mutate(x = as.numeric(str_remove_all(x, "V"))) %>% 
  filter(value > 0) %>% 
  #Rescale to 50% of coords
  group_by(x = x %/% 2, y = y %/% 2) %>% 
  summarize(value = max(value)) %>% 
  ungroup()

# Plot the Rexes ----

#Hacking K-means clustering to approximate slices

trex.pic2 <- trex.pic %>% 
  mutate(cluster = kmeans(.[, c(1:2)], 100)$cluster)

trex.data <- trex.specimens %>% 
  mutate(plot = map(comp, function(comp){
    keep_k <- sample(1:100, comp)
    return(trex.pic2 %>% filter(cluster %in% keep_k))
  })) %>% 
  unnest(plot)

trex.labels <- trex.specimens %>% 
  mutate(label_p = paste0(round(comp), "%"),
         label_d = paste0("Discovered: ", Discovered, "\n",
                          Location))


trex.data %>% 
  ggplot(aes(x=x, y=y, fill = Formation)) +
  geom_raster() +
  scale_fill_manual(values = c("#2dd49c", "#ff4040", "#5384ff", "#ff25ab", "#ff6141", "#ff9e53")) +
  coord_fixed() +
  geom_text(data = trex.labels, aes(label = label_p), 
             x = 30, y = 30, size = 5, 
            color = "#00436b", fontface = "bold") +
  geom_text(data = trex.labels, aes(label = label_d),
            x = 400, y = 40, size = 3) +
  facet_wrap(~Name, ncol = 3) +
  labs(title = "Tyrannosaurus rex fossils, % of skeleton discovered",
       subtitle = "Sorted by date discovered, colored by geological formation discovered",
       caption = paste0("Data: https://en.wikipedia.org/wiki/Specimens_of_Tyrannosaurus\n",
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

#Addendum ---
# Got a bunch of complaints about Trix... and I like Tristan

add.specimens <- tibble::tribble(
  ~Name, ~Discovered, ~Formation, ~Location, ~comp,
  "Trix", 2013, "Hell Creek Formation", "Montana", 77,
  "Tristan \n(MB.R.91216)", 2010, "Hell Creek Formation", "Carter County, Montana", 57
)

add.data <- add.specimens %>% 
  mutate(plot = map(comp, function(comp){
    keep_k <- sample(1:100, comp)
    return(trex.pic2 %>% filter(cluster %in% keep_k))
  })) %>% 
  unnest(plot)

add.labels <- add.specimens %>% 
  mutate(label_p = paste0(round(comp), "%"),
         label_d = paste0("Discovered: ", Discovered, "\n",
                          Location))

add.data %>% 
  ggplot(aes(x=x, y=y, fill = Formation)) +
  geom_raster() +
  scale_fill_manual(values = "#ff4040") +
  coord_fixed() +
  geom_text(data = add.labels, aes(label = label_p), 
            x = 30, y = 30, size = 5, 
            color = "#00436b", fontface = "bold") +
  geom_text(data = add.labels, aes(label = label_d),
            x = 400, y = 40, size = 4) +
  facet_wrap(~Name, ncol = 2) +
  labs(title = "Addendum: Tyrannosaurus rex fossils, % of skeleton discovered",
       subtitle = "Sorted by date discovered, colored by geological formation discovered",
       caption = paste0("Data: https://en.wikipedia.org/wiki/Specimens_of_Tyrannosaurus\n",
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


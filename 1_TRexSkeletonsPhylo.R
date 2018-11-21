####
#Quick t-rex chart for Phylopic
####


library(tidyverse); library(rvest)
library(png)

#Phylopid Budget ----

phylo.budget <- tribble(
  ~Expense, ~PerMil,
  "Web Hosting\n4.3%", 43,
  "Development\n51.1%", 511,
  "Migration\n2.9%", 29,
  "Quality Assurance\n2.9%", 29,
  "Manufacturing\n14.2%", 142,
  "Shipping & Handling\n20.6%", 206,
  "Crowdfunding\n4%", 40
)
phylo.budget2 <- phylo.budget%>% 
  uncount(PerMil) %>% 
  mutate(cluster_i = row_number())

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
  ungroup() %>% 
  mutate(cluster = kmeans(.[, c(1:2)], 1000)$cluster) %>% 
  arrange(x, desc(y))

trex.pic.cluster.i <- unique(trex.pic$cluster)

trex.pic2 <- trex.pic %>% 
  rowwise() %>% 
  mutate(cluster_i = which(cluster == trex.pic.cluster.i)) %>% 
  ungroup() %>% 
  right_join(phylo.budget2) %>% 
  mutate(Expense = fct_relevel(Expense, unique(Expense)))


trex.pic2 %>% 
  ggplot(aes(x=x, y=y, fill = Expense)) +
  geom_raster() +
  scale_fill_manual(values = c("#ff4040", "#2dd49c", "#D4AF37", "#ff25ab", "#5384ff", "#ff6141", "#ff9e53")) +
  coord_fixed() +
  labs(title = "PhyloPic 2.0 Projected Budget",
       subtitle = "https://www.indiegogo.com/projects/phylopic-2-0-free-silhouettes-of-all-life-forms/") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face ="bold"),
    legend.text = element_text(size = 10)
    )
ggsave("TRexPhylo.png", device = "png")

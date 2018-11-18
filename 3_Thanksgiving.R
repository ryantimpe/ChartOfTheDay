####
# Random turkey facts
####

library(tidyverse); library(rvest);
library(png)

#Text wrapping function ---
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

#Turkey facts! ----
url <- "http://extension.illinois.edu/turkey/turkey_facts.cfm"

trk.raw <- read_html(url) %>% 
  html_nodes("li") %>% 
  html_text() %>% 
  str_remove_all("\\t|\\n|\\r")

trk.fact <- trk.raw[nchar(trk.raw) > 15][7:82]

#Turkey PNG ----
process_png <- function(pic, scale=2){
  dat <- pic[,,4] %>% 
    as.data.frame() %>% 
    mutate(y = n()-row_number()) %>% 
    gather(x, value, 1:(ncol(.)-1)) %>% 
    mutate(x = as.numeric(str_remove_all(x, "V"))) %>% 
    filter(value > 0) %>% 
    #Rescale to 50% of coords
    group_by(x = x %/% scale, y = y %/% scale) %>% 
    summarize(value = max(value)) %>% 
    ungroup() %>% 
    #Make 100 chunks
    mutate(cluster = kmeans(.[, c(1:2)], 100)$cluster) %>% 
    arrange(x, desc(y))
  
  dat.cluster.i <- unique(dat$cluster)
  
  dat2 <- dat %>% 
    rowwise() %>% 
    mutate(cluster_i = which(cluster == dat.cluster.i)) %>% 
    ungroup() 
  
  return(dat2)
}

process_png_y <- function(pic, feet=6){
  dat <- pic[,,4] %>% 
    as.data.frame() %>% 
    mutate(y = n()-row_number()) %>% 
    gather(x, value, 1:(ncol(.)-1)) %>% 
    mutate(x = as.numeric(str_remove_all(x, "V"))) %>% 
    filter(value > 0)

  xscale <- (max(dat$y)/max(dat$x))^(-1)
  
  dat2 <- dat %>% 
    #Rescale to 1ft == 50px
    mutate(ys = floor(y/max(y)*feet*50), xs = floor(x/max(x)*feet*50*xscale)) %>% 
    group_by(x = xs, y = ys) %>% 
    summarize(value = max(value)) %>% 
    ungroup() %>% 
    #Make 100 chunks
    mutate(cluster = kmeans(.[, c(1:2)], 100)$cluster) %>% 
    arrange(x, desc(y))
  
  dat.cluster.i <- unique(dat2$cluster)
  
  dat3 <- dat2 %>% 
    rowwise() %>% 
    mutate(cluster_i = which(cluster == dat.cluster.i)) %>% 
    ungroup() 
  
  return(dat2)
}

trk1.pic <- readPNG("data/Meleagris.png") %>% process_png()
trk2.pic <- readPNG("data/Turkey2.png") %>% process_png()

# Universals ----
chart_footer <- paste0("Unverified turkey facts: http://extension.illinois.edu/turkey/turkey_facts.cfm\n",
                       "@ Ryan Timpe .com")

chart_footer_bonus <- paste0("@ Ryan Timpe .com")

#Colors 
trk.col.orange <- c("#ff6141")
trk.col.red <- c("#8B0000", "#ff4040")
trk.col.gold <- c("#D4AF37", "#CFB53B", "#C5B358")
trk.col.blue <- c("#00436b")
trk.col.ltblue <- c("#5384ff")

trk.colors <- c(trk.col.orange, trk.col.blue, trk.col.gold[1], trk.col.red[1])
trk.colors2 <- c(trk.col.orange, trk.col.blue, trk.col.gold[1], trk.col.red[1], "#5384ff", "#ff25ab")

#Chart theme
trk_chart_theme <- theme(
  panel.grid = element_blank(),
  strip.background = element_rect(fill = "#00436b"),
  strip.text = element_text(color = "white", face = "bold"),
  plot.background = element_rect(fill = "#fcedcc"),
  plot.title = element_text(size = 16, face = "bold"),
  plot.subtitle = element_text(size = 10),
  panel.background = element_rect(fill = "#e4c8ac"),
  legend.position = "bottom"
)

# Fact 14 ----
fact.index <- 14
fact <- trk.fact[fact.index]
"Turkey hens are usually sold as whole birds. 
Toms are processed into turkey sausage, turkey franks, tenderloins, cutlets and deli meats."

f14 <- tibble::tribble(
  ~Turkey, ~Channel, ~Amount,
  "Hens (female)", "Whole bird", 100,
  "Toms (male)", "Sausage", 20,
  "Toms (male)", "Franks", 20,
  "Toms (male)", "Tenderloins", 20,
  "Toms (male)", "Cutlets", 20,
  "Toms (male)", "Deli meat", 20
) %>% 
  uncount(Amount) %>% 
  group_by(Turkey) %>% 
  mutate(cluster_i = row_number()) %>% 
  ungroup() %>% 
  right_join(trk1.pic %>% mutate(Turkey = "Hens (female)") %>% 
               bind_rows(trk1.pic %>% mutate(Turkey = "Toms (male)")))


f14 %>% 
  ggplot(aes(x=x, y=y, fill = Channel)) +
  geom_raster() +
  scale_fill_manual(values = trk.colors2) +
  coord_fixed() +
  facet_wrap(.~Turkey) +
  geom_text(data = tibble(Turkey = c("Toms (male)","Hens (female)"), 
                          symb = c("\u2642", "\u2640"),
                          Channel = NA), 
            aes(label = symb),
            x = 180, y = 200, size = 15, fontface = "bold",
            color = trk.col.blue) +
  labs(title = "U.S. Turkey consumption, by final product",
       subtitle = wrapper(fact, 90),
       caption = chart_footer) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Fact 15 ----
fact.index <- 15
fact <- trk.fact[fact.index]
"In 2011, 47.4% of turkeys were sold to grocery stores and other retail outlets, 
30% sold in commodity outlets, 15.5% sold to foodservice outlets and 6.2% were exported."

f15 <- tibble::tribble(
  ~Channel, ~Amount,
  "Grocery Stores & Retail", 48,
  "Commodity outlets", 30,
  "Foodservice outlets", 16,
  "Exported", 6
) %>% 
  uncount(Amount) %>% 
  mutate(cluster = row_number()) %>% 
  right_join(trk2.pic)


f15 %>% 
  ggplot(aes(x=x, y=y, fill = Channel)) +
  geom_raster() +
  scale_fill_manual(values = trk.colors) +
  coord_fixed() +
  # geom_text(aes(label = paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = "U.S. Turkey production, by final destination",
       subtitle = wrapper(fact, 90),
       caption = chart_footer) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Fact 19 ----
fact.index <- 19
fact <- trk.fact[fact.index]
"A 15 pound turkey usually has about 70 percent white meat and 30 percent dark meat."

f19 <- tibble::tribble(
  ~`Meat Type`, ~Amount,
  "Dark meat", 30,
  "White meat", 70
) %>% 
  uncount(Amount) %>% 
  mutate(cluster_i = row_number()) %>% 
  right_join(trk2.pic)

f19 %>% 
  ggplot(aes(x=x, y=y, fill = `Meat Type`)) +
  geom_raster() +
  scale_fill_manual(values = c(trk.col.blue, trk.col.gold)) +
  coord_fixed() +
  # geom_text(aes(label = paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = "Whole turkey, by meat type",
       subtitle = wrapper(fact, 90),
       caption = chart_footer) + 
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

  
# Fact 63 ----
fact.index <- 63
fact <- trk.fact[fact.index]
"Turkeys will have 3,500 feathers at maturity."

f63 <- trk2.pic %>% 
  mutate(cluster2 = kmeans(.[,1:2], 3500)$cluster) %>% 
  group_by(cluster2) %>% 
  sample_n(1) %>% 
  ungroup()

f63 %>% 
  ggplot(aes(x=x, y=y)) +
  geom_point( color = trk.col.red[1], size = 3, alpha = 0.5, shape = 18) +
  coord_fixed() +
  # geom_text(aes(paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = wrapper(fact, 90),
       subtitle = "Each point is one feather",
       caption = chart_footer) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Fact 48 ----
fact.index <- 48
fact <- trk.fact[fact.index]
"Wild turkeys can fly for short distances up to 55 mph and can run 20 mph."

f48 <- tribble(
  ~Method, ~Speed, ~Label,
  "Flying", 55, "55 mph",
  "Running", 20, "20 mph",
  "Caged", 1, "#buyorganic"
) %>% 
  mutate(Method = fct_relevel(Method, "Caged", "Running", "Flying"))

f48 %>% 
  ggplot(aes(x = Method, y = Speed, fill = Method)) +
  geom_col(width = 0.5) + 
  scale_fill_manual(values = c("Flying" = trk.col.ltblue, "Running" = trk.col.gold[1],
                               "Caged" = trk.col.red[2])) +
  geom_text(aes(label = Label), hjust = 0, nudge_y = 1, size = 6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 70)) +
  labs(title = "Turkey speed, by mobility method",
       subtitle = wrapper(fact, 90),
       caption = paste0("Images: phylopic.org\n", chart_footer)) + 
  theme_minimal() +
  trk_chart_theme +
  theme(
    axis.text.y =  element_text(size = 12, face = 'bold', color = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 12, face = 'bold', color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.line.x = element_line(color = "black"),
    legend.position = "none"
  )

# Fact 27 - Evolution ----
fact.index <- 27
fact <- trk.fact[fact.index] 
#Hmmm probably 11
fact <- "Turkeys lived eleven million years ago."

f27 <- tribble(
  ~Animal, ~mya,
  "Archaeopteryx", 1500,
  "Tyrannosaurus rex", 680,
  "Turkey", 110,
  "You", 0
)

f27 %>% 
  ggplot(aes(x=mya)) +
  coord_fixed() +
  scale_x_reverse(name = "Millions of Year ago",
                  breaks = c(f27$mya), labels = f27$mya/10) +
  scale_y_continuous(limits = c(0, 300)) +
  geom_raster(data = readPNG("data/Homo.png") %>% process_png(4),
              aes(x=x-50, y=y), fill = trk.col.blue[1]) +
  geom_raster(data = readPNG("data/Meleagris.png") %>% process_png(6),
              aes(x=x+100, y=y), fill = trk.col.gold[1]) +
  geom_raster(data = readPNG("data/Tyrannosaurus.png") %>% process_png(2),
              aes(x=x+400, y=y), fill = trk.col.red[1]) +
  geom_raster(data = readPNG("data/Archaeopteryx.png") %>% process_png(6),
              aes(x=x+1450, y=y), fill = trk.col.orange[1]) +
  geom_text(aes(label = "The earliest birds"), 
            x = -1500, y = 150,  
            size = 6, color = trk.col.orange[1]) +
  geom_text(aes(label = "T. rex"), 
            x = -680, y = 250,  
            size = 6, color = trk.col.red[1]) +
  geom_text(aes(label = "Turkeys\nevolved"), 
            x = -120, y = 150,  
            size = 6, color = trk.col.gold[1]) +
  # geom_text(aes(paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = wrapper(fact, 90),
       subtitle = "T.rex couldn't celebrate Thanksgiving with turkey, but they had plenty of other options.",
       caption = paste0("Images: phylopic.org\n", chart_footer_bonus)) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 12, face = 'bold', color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.line.x = element_line(color = "black"),
    panel.border = element_blank(),
    panel.background = element_blank()
  )
  

# Fact B1 - Jurassic Park Velociraptor ----

fact <- "Actual velociraptors were the size of turkeys"

fb1 <- tribble(
  ~Source, ~Animal, ~Height,
  "Human size", "Homo", 6,
  "Jurassic Park", "Velociraptor", 4.75,
  "Turkey-size", "Meleagris", 2.5,
  "Turkey-size", "Velociraptor", 2
) %>% 
  mutate(Pic = purrr::map2(Animal, Height, function(ani, hght){
    readPNG(paste0("data/", ani, ".png")) %>% 
      process_png_y(hght)
  })) %>% 
  unnest(Pic) %>% 
  group_by(Source, Animal) %>% 
  mutate(x = ifelse(Animal == "Velociraptor", -x, x),
         x = ifelse(Source == "Turkey-size", x + floor(median(x)/4), x)) %>% 
  ungroup()


fb1 %>% 
  ggplot(aes(x=x, y=y, fill = Source)) +
  geom_raster() +
  scale_fill_manual(values = c("Human size" = trk.col.blue, "Turkey-size" = trk.col.gold[2],
                               "Jurassic Park" = trk.col.red[1])) +
  scale_y_continuous(name = "Height (Ft)", breaks = seq(50, 300, by=50), labels = 1:6) +
  coord_fixed() +
  # geom_text(aes(label = paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = fact,
       subtitle = wrapper("Hollywood took some liberties with human-sized velociraptors in Jurassic Park...
                          and they plucked off the feathers.", 150),
       caption = paste0("Images: phylopic.org\n", chart_footer_bonus)) + 
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 14, face = "bold")
  )


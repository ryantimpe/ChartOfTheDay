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
chart_footer <- paste0("Turkey Triva: http://extension.illinois.edu/turkey/turkey_facts.cfm\n",
                       "@ Ryan Timpe .com")

chart_footer_bonus <- paste0("Turkey Triva
                             @ Ryan Timpe .com")

#Colors 
trk.col.orange <- c("#ff6141")
trk.col.red <- c("#8B0000", "#ff4040")
trk.col.gold <- c("#D4AF37", "#CFB53B", "#C5B358")
trk.col.blue <- c("#00436b")
trk.col.ltblue <- c("#5384ff")
trk.col.bkgrnd <- c("#fcedcc")

trk.colors <- c(trk.col.orange, trk.col.blue, trk.col.gold[1], trk.col.red[1])
trk.colors2 <- c(trk.col.orange, trk.col.blue, trk.col.gold[1], trk.col.red[1], "#5384ff", "#ff25ab")

#Chart theme
trk_chart_theme <- theme(
  panel.grid = element_blank(),
  strip.background = element_rect(fill = "#00436b"),
  strip.text = element_text(color = "white", face = "bold"),
  plot.background = element_rect(fill = "#fcedcc"),
  plot.title = element_text(size = 14, face = "bold"),
  plot.subtitle = element_text(size = 10),
  plot.caption = element_text(size = 8),
  panel.background = element_rect(fill = "#fcedcc"),
  legend.position = "bottom"
)

NULL #Just so RStudio stops opening plot 14

# Fact 14 Whole vs product ----
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
  scale_fill_manual(name = "Product", values = trk.colors2) +
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

ggsave("Output/Turkey/1_ConsumptionBySex.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact 14a NGrams ----
library(jsonlite)
#Data manuallly copied out of Google NGram source
#https://books.google.com/ngrams/graph?content=turkey+burger%2Cturkey+sausage%2Cturkey+tenderloin%2Cturkey+cutlet%2Cturkey+leg&year_start=1960&year_end=2008&corpus=15&smoothing=3&share=&direct_url=t1%3B%2Cturkey%20burger%3B%2Cc0%3B.t1%3B%2Cturkey%20sausage%3B%2Cc0%3B.t1%3B%2Cturkey%20tenderloin%3B%2Cc0%3B.t1%3B%2Cturkey%20cutlet%3B%2Cc0%3B.t1%3B%2Cturkey%20leg%3B%2Cc0

f14a <- read_json("data/Turkey/ngrams.json", simplifyVector = TRUE) %>% 
  unnest(timeseries) %>% 
  rename(value = timeseries) %>% 
  group_by(ngram) %>% 
  mutate(Year = row_number() + 1959) %>% 
  mutate(norm = value / value[which(Year == 1980)]* 100) %>% 
  ungroup() %>% 
  filter(Year >= 1980)

f14a %>% 
  ggplot(aes(x=as.factor(Year), y=value, group = ngram)) +
  geom_line(aes(color = ngram), size = 2) +
  scale_color_manual(values = trk.colors2, name = "Ngram") +
  labs(title = "Turkey product popularity, 1980-2008, Google Ngrams",
       subtitle = "Turkey sausuge popularity peaked in 1996; turkey burgers are still on the up.",
       caption = paste0("Data: https://books.google.com/ngrams/\n", chart_footer_bonus),
       y = "Percent usage") + 
  guides(color = guide_legend(nrow=2))+
  theme_minimal() +
  trk_chart_theme +
  theme(
    axis.text.y =  element_text(size = 12, face = 'bold', color = "black"),
    axis.title.y =  element_text(size = 12, face = 'bold', color = "black"),
    axis.text.x = element_text(size = 12, face = 'bold', color = "black", 
                               angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "black")
  )

ggsave("Output/Turkey/1a_ConsumptionByTime.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact 15 Channel ----
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
  ggplot(aes(x=x/2, y=y/2, fill = Channel)) +
  geom_raster() +
  scale_fill_manual(values = trk.colors) +
  coord_fixed() +
  # geom_text(aes(label = paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = "U.S. Turkey production, by final destination",
       subtitle = wrapper(fact, 70),
       caption = chart_footer) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, face="bold"),
    plot.subtitle = element_text(hjust = 0),
    legend.text = element_text(size=10),
    legend.title = element_text(size=10),
    plot.margin = margin(t = 2, r = 60, b = 2, l = 60, unit = "pt")
  )

ggsave("Output/Turkey/2_ProductionByDestination.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact 19 ----
fact.index <- 19
fact <- trk.fact[fact.index]
fact <- "A 15lb turkey is 70% white meat by volume, but white meat is only 67% of the calories."

#Calories 
158*70 / (158*70 + 183*30)

f19 <- tibble::tribble(
  ~`Meat Type`, ~Amount, ~Metric,
  "Dark meat (183 cal / serving)", 30, "Volume",
  "White meat (158 cal / serving)", 70, "Volume",
  "Dark meat (183 cal / serving)", 33, "Calories",
  "White meat (158 cal / serving)", 67, "Calories"
) %>% 
  uncount(Amount) %>% 
  group_by(Metric) %>% 
  mutate(cluster_i = row_number()) %>% 
  ungroup() %>% 
  right_join(trk2.pic ) %>% 
  mutate(Metric = fct_relevel(Metric, "Volume", "Calories"))

f19 %>% 
  ggplot(aes(x=x, y=y, fill = `Meat Type`)) +
  geom_raster() +
  scale_fill_manual(values = c(trk.col.red[1], trk.col.gold[1])) +
  coord_fixed() +
  geom_text(data = tibble(Metric = c("Volume", "Calories"), 
                          Label = paste(c("70%", "67%"), "\nwhite meat"),
                          `Meat Type` = NA),
              aes(label = Label),
            x = 170, y = 150, size = 6, fontface="bold", color = trk.col.blue) +
  facet_grid(.~Metric) +
  guides(fill = guide_legend(nrow=2))+
  labs(title = "Whole turkey composition, by meat type",
       subtitle = wrapper(fact, 90),
       caption = chart_footer) + 
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("Output/Turkey/3_CompositionByMeat.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact stuffing ----
f99 <- tribble(
  ~Recipe, ~Turkey, ~Stuffing, ~URL,
  "Alton Brown", 11, 4, "https://www.foodnetwork.com/recipes/alton-brown/turkey-with-stuffing-recipe-3381360",
  "Taste of Home", 15, 12, "https://www.tasteofhome.com/recipes/classic-stuffed-turkey/",
  "Farmhouse Herbed", 12, 9, "https://www.epicurious.com/recipes/food/views/farmhouse-herbed-stuffing-240446",
  "Pillsburty", 16, 10, "https://www.pillsbury.com/recipes/roast-turkey-with-stuffing/3dd04532-f8a3-4338-a523-080fe7b30314",
  "Martha Stewart", 20, 12, "https://www.marthastewart.com/317812/turkey-with-stuffing",
  "Betty Crocker", 16, 9, "https://www.bettycrocker.com/recipes/easy-turkey-stuffing/380d2532-b381-4336-8f18-f74469f17cd0",
  "Real Simple", 11, 6, "https://www.myrecipes.com/recipe/roast-turkey-with-sage-stuffing-gravy",
  "Food & Wine", 19, 20, "https://www.foodandwine.com/recipes/roasted-turkey-with-italian-sausage-stuffing",
  "BBC", 11, 1.5, "https://www.bbc.com/food/recipes/perfect_roast_turkey_72482",
  "Kraft", 10, 2, "https://www.kraftrecipes.com/recipe/054214/roast-turkey-sausage-stuffing",
  "Bon Appetit", 18, 8, "https://www.bonappetit.com/recipe/roast-turkey-with-cornbread-sausage-stuffing",
  "Andrew Zimmern", 14, 9, "https://andrewzimmern.com/2012/11/16/perfect-thanksgiving-turkey-stuffing-gravy/",
  "Cooking Channel", 15, 7, "https://www.cookingchanneltv.com/recipes/turkey-and-stuffing-2012777",
  "NYTimes", 15, 8, "https://cooking.nytimes.com/recipes/231-roast-turkey-with-bread-stuffing",
  "Craig Claiborne", 18.5, 6, "https://cooking.nytimes.com/recipes/185-roast-stuffed-turkey"
)
  

f99 %>% 
  ggplot(aes(x=Turkey, y=Stuffing))+
  geom_point(size = 4, color = trk.col.ltblue, shape = 18)+
  geom_smooth(size = 2, color = trk.col.blue, fill = trk.col.gold[1], 
              method = "lm", formula = y ~ x + 0) +
  geom_text(label = "Online recipes", y =8.5, x= 13, angle = 15, size = 6, color = trk.col.blue) +
  geom_label(label = "0.56 cups stuffing /\nlb turkey", y =3, x= 17, size = 5, 
             fill = trk.col.blue, color = "white") +
  geom_label(label = "0.75 cups stuffing /\nlb turkey", y =18, x= 12, size = 5, 
             fill = trk.col.red[1], color = "white") +
  geom_text(label = "Conventional Wisdom", y =11, x= 13, angle = 17, size = 6, color = trk.col.red[1]) +
  geom_segment(x=10, y=7.5, xend=20, yend=15,
              size = 2, color = trk.col.red[1]) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  labs(title = "Stuffing required by Turkey weight,\nconventional knowledge vs online recipes",
       subtitle = "Conventional wisdom suggests 3/4 cups per pound of turkey,\nwhile 15 online recipes call for an average of a little more than half a cup.",
       caption = paste0("Data collected from various online recipes\n", chart_footer_bonus),
       x = "Turkey Weight (lbs)",
       y = "Stuffing amount (cups)") + 
  theme_minimal() +
  trk_chart_theme +
  theme(
    axis.text.y =  element_text(size = 12, face = 'bold', color = "black"),
    axis.title =  element_text(size = 12, face = 'bold', color = "black"),
    axis.text.x = element_text(size = 12, face = 'bold', color = "black", 
                               angle = 90, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid = element_line(color = "#cccccc"),
    plot.margin = margin(t = 2, r = 50, b = 2, l = 20, unit = "pt")
  )
  
ggsave("Output/Turkey/4_StuffingBySize.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact 63 feathers ----
fact.index <- 63
fact <- trk.fact[fact.index]
"Turkeys will have 3,500 feathers at maturity."

library(emoGG)
emoji_search("turkey")

f63 <- trk2.pic %>% 
  mutate(cluster2 = kmeans(.[,1:2], 700)$cluster) %>% 
  group_by(cluster2) %>% 
  sample_n(1) %>% 
  ungroup()

f63 %>% 
  ggplot(aes(x=x, y=y)) +
  geom_emoji(emoji="1f983") +
  # geom_point( color = trk.col.red[1], size = 3, alpha = 0.5, shape = 18) +
  coord_fixed() +
  # geom_text(aes(paste0("Fact #", fact.index)),
  #           x = 220, y = 20, size = 5, color = trk.col.blue) +
  labs(title = wrapper(fact, 90),
       subtitle = "Each turkey emoji is 50 feathers",
       caption = chart_footer) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("Output/Turkey/5_TurkeyFeathers.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact 48 Speed ----
fact.index <- 48
fact <- paste(trk.fact[fact.index], trk.fact[45])
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
       subtitle = wrapper(fact, 76),
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

ggsave("Output/Turkey/6_TurkeyMoblity.png", device = "png",
       width = 6, height = 5.6, units = "in")

# Fact Poop ----
fact <- "You can identify a turkey's gender by the shape of its droppings." 

emoji_search("turkey") #1f983
emoji_search("poop") #1f4a9

fpp <- read_csv("data/Turkey/TurkeyPoop.csv") %>% 
  group_by(Sex) %>% 
  mutate(y = n()-row_number()) %>% 
  ungroup() %>% 
  gather(x, value, `1`:`10`) %>% 
  mutate(x = as.numeric(x)) %>% 
  mutate(emoji = case_when(
    value == 1 & (x+y)%%2 == 0 ~ "1f4a9",
    value == 1 & (x+y)%%2 == 1 ~ "1f983"
  )) %>% 
  filter(!is.na(emoji)) %>% 
  mutate(Sex = ifelse(Sex == "Male", "Toms (male)", "Hens (female)"))

fpp %>% 
  filter(emoji == "1f4a9") %>% 
  ggplot(aes(x=x, y=y)) +
  geom_emoji(emoji="1f4a9", size = .08) +
  geom_emoji(data = fpp %>% filter(emoji == "1f983"), emoji="1f983", size = .08) +
  coord_fixed() +
  facet_grid(.~Sex) +
  labs(title = wrapper(fact, 90),
       subtitle = "Female turkey poop has a 'J' shape, while male droppings are spiral.",
       caption = chart_footer_bonus) + 
  guides(fill = guide_legend(nrow=2))+
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("Output/Turkey/7_TurkeyPoop.png", device = "png",
       width = 6, height = 5.6, units = "in")

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
              aes(x=x+450, y=y), fill = trk.col.red[1]) +
  geom_raster(data = readPNG("data/Archaeopteryx.png") %>% process_png(6),
              aes(x=x+1450, y=y), fill = trk.col.orange[1]) +
  geom_text(aes(label = "The earliest birds"), 
            x = -1500, y = 150,  
            size = 5, color = trk.col.orange[1]) +
  geom_text(aes(label = "T. rex"), 
            x = -680, y = 250,  
            size = 5, color = trk.col.red[1]) +
  geom_text(aes(label = "Turkeys\nevolved"), 
            x = -120, y = 150,  
            size = 5, color = trk.col.gold[1]) +
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

ggsave("Output/Turkey/8_Evolution.png", device = "png",
       width = 8, height = 3.5, units = "in")
  

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
                          and they plucked off the feathers.", 85),
       caption = paste0("Images: phylopic.org\n", chart_footer_bonus)) + 
  theme_minimal() + 
  trk_chart_theme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 14, face = "bold")
  )
ggsave("Output/Turkey/9_JurassicPark.png", device = "png",
       width = 6, height = 4, units = "in")

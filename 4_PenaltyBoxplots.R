library(tidyverse)
library(rvest)

nhl_cols <- read_csv("data/NHLTeamColors.csv")

url <- "https://www.hockey-reference.com/leagues/NHL_2019_skaters.html"

nhl_stats <- read_html(url) %>% 
  html_nodes(xpath = '//*[@id="stats"]') %>% 
  html_table()

nhl_stats_df <- nhl_stats[[1]]

names(nhl_stats_df) <- trimws(paste(names(nhl_stats_df), as.character(nhl_stats_df[1, ])))

nhl_stats_df <- nhl_stats_df[-1, ]

names(nhl_stats_df)

pbx <- nhl_stats_df %>% 
  filter(!(Tm %in% c("TOT", "Tm"))) %>% 
  left_join(nhl_cols %>% 
              select(Tm = Team, Conference, Division, Col1, Col2) %>% 
              group_by(Conference) %>% 
              mutate(team_index = row_number()) %>% 
              ungroup()) %>% 
  mutate(Tm = factor(Tm)) %>% 
  select(Player, Tm, GP = `GP`, Points = `Scoring PTS`, PIM = `PIM`, TOI = `Ice Time TOI`,
         Conference, Division, Col1, Col2, team_index) %>% 
  mutate_at(vars(GP, Points, PIM, TOI), as.numeric) %>% 
  mutate(PIM_TOI = PIM / TOI)


#Draw my own!

pbx_plot <- pbx %>% 
  group_by(Tm, Division, Col1, Col2) %>% 
  nest() %>% 
  mutate(PIM_qt = map(data, ~quantile(.x$PIM, probs =  c(0, 0.1, 0.25, .5, 0.75, 0.9, 1)))) %>% 
  unnest(PIM_qt) %>% 
  group_by(Tm) %>% 
  mutate(Qnt = paste0("Q", row_number())) %>% 
  ungroup() %>% 
  spread(Qnt, PIM_qt) %>% 
  group_by(Division) %>% 
  mutate( x_base = row_number()) %>% 
  ungroup() %>% 
  #Num of 0 minute PIM
  left_join(
    pbx %>% 
      group_by(Tm) %>% 
      summarize(PIM_0 = sum(PIM == 0),
                PIM_2sd = sum(PIM >= (mean(PIM) + 1.5*sd(PIM))))
  ) %>% 
  mutate(Division = fct_relevel(Division, "Metropolitan", "Atlantic", "Pacific", "Central"))

pbx_legend <- pbx_plot %>% 
  filter(Tm == "WPG") %>% 
  gather(metric, yy, starts_with("Q")) %>% 
  mutate(
    label = case_when(
      metric == "Q4" ~ "Median PIM",
      metric == "Q3" ~ "40%",
      metric == "Q5" ~ "60%",
      metric == "Q2" ~ "25% PIM",
      metric == "Q6" ~ "75% PIM"
    ),
    col = case_when(
      metric == "Q4" ~ "#FF0000",
      metric %in% c("Q3", "Q5") ~ "#0000FF",
      TRUE ~ "black"
    ),
    x_jit = case_when(
      metric == "Q4" ~ -1/3,
      metric %in% c("Q3", "Q5") ~ -1/3,
      TRUE ~ 1/4
    ),
    x_jit = x_jit + x_base
  ) %>% 
  drop_na(label)

pbx_plot %>% 
  ggplot(aes(x= x_base, y = Q4, group = Tm)) +
  #Rink
  geom_rect(aes(xmin = x_base-1/3, xmax = x_base+1/3, ymin = Q2, ymax = Q6),
            fill = "white", color = "black") +
  #Median
  geom_segment(aes(x = x_base-1/3, xend = x_base+1/3, y = Q4, yend = Q4), 
               size = 1.5, color = "#FF0000") +
  #25/75
  geom_segment(aes(x = x_base-1/3, xend = x_base+1/3, y = Q3, yend = Q3), 
               size = 1.2, color = "#0000FF") +
  geom_segment(aes(x = x_base-1/3, xend = x_base+1/3, y = Q5, yend = Q5), 
               size = 1.2, color = "#0000FF") +
  #Logo
  geom_point(aes(color = Col2, size = PIM_2sd*2)) +
  geom_point(aes(color = Col1, size = PIM_2sd)) +
  scale_size_continuous(range = c(3, 8), guide = FALSE) +
  # geom_point(aes(color = Col2), size = 10) +
  # geom_point(aes(color = Col1), size = 7) +
  scale_color_identity() +
  #Team name
  geom_label(aes(x = x_base, y = Q6, label = Tm, fill = Col2), color = "white",
             hjust = 0.5, vjust = 0.5, nudge_y = 8, fontface = "bold", size = 3) +
  scale_fill_identity() +
  #Fake legend
  geom_segment(
    data = pbx_legend,
    aes(x = x_base + 1/3 + 0.1, xend = x_jit + .9, y = yy, yend = yy, color = col),
    size = 1
  ) +
  geom_label(
    data = pbx_legend,
    aes(x = x_jit+1, y = yy, label = label, color = col),
    hjust = 0,size = 3
  ) +
  #Rest of plot
  coord_cartesian(ylim = c(-5, max(pbx_plot$Q6+10)), xlim = c(2/3, 8.9)) +
  facet_grid(Division ~ ., scales = "free_x") +
  labs(
    title = "Penalty Boxplots - NHL player time in penalty box",
    subtitle = paste("Player penalty minutes by team, 2018-2019 season through", format.Date(Sys.Date(), format = "%B %d, %Y")),
    caption = "Size of middle circle proportional to # of players with PIM >1.5 st dev from mean.
    Data: https://www.hockey-reference.com
    @ Ryan Timpe .com",
    x = NULL,
    y = "Penalty Minutes"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "#fcedcc"),
    panel.grid.major.y = element_line(color = "#ebdcbb"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("Output/PenaltyBoxPlots.png", device = "png",
       width = 6.6, height = 5.6, units = "in")

library(tidyverse)
library(ggtext)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

glimpse(hike_data)

hikes <- hike_data %>% 
  mutate(across(.cols = c(gain, highpoint, rating), parse_number)) %>% 
  separate(length, into = c("distance", "type"), sep = ",") %>% 
  mutate(distance = str_extract(distance, "^\\d+\\.*\\d*"),
         distance = as.numeric(distance),
         type = str_trim(type),
         dist_oneway = ifelse(type == "roundtrip", distance/2, distance),
         gain = gain/5280) %>% 
  select(-distance)

glimpse(hikes)

hikes %>% ggplot(aes(x = dist_oneway, y = gain, color = type)) +
  geom_segment(data = filter(hikes, type == "one-way"),
               aes(x = 0, y = 0, xend = dist_oneway, yend = gain),
               size = 1) +
  geom_curve(data = filter(hikes, type == "roundtrip"),
             aes(x = 0, y = 0, xend = dist_oneway, yend = gain),
             angle = 150,
             size = 1) +
  geom_curve(data = filter(hikes, type == "roundtrip"),
             aes(x = dist_oneway, y = gain, xend = 0, yend = 0),
             angle = 30,
             size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Distance one-way (miles)") +
  scale_y_continuous(name = "Gain (miles)") +
  scale_color_discrete(type = c("#d2848d", "#6e7cb9")) +
  labs(title = "<span style='color:#6e7cb9;'>Roundtrip</span> 
          and  <span style='color:#d2848d;'>one-way</span> 
          hikes in Washington State") +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        plot.background = element_rect(fill = "#f5db99"),
        panel.background = element_blank())

# ggsave("trails.png", path = "2020_11_24_trails")

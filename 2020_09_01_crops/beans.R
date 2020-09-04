library(tidyverse)
library(ggimage)
library(magick)

crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
glimpse(crops)

crops <- crops %>% select(Entity, Code, Year,
                          Beans = `Beans (tonnes per hectare)`)

beans <- image_read("C:/Users/chris/Downloads/blackbeans.png")
beans <- image_transparent(beans, color = "white")
image_write(beans, "2020_09_01_crops/beans.png", "png")

crops$image <- "2020_09_01_crops/beans.png"

crops %>% filter(Entity == "World") %>% 
  ggplot(aes(x = Year, y = Beans)) +
  geom_image(image = "2020_09_01_crops/beans.png",
             aes(x = 1961, y = 0.493),
             size = 0.4) +
  geom_image(image = "2020_09_01_crops/beans.png",
             aes(x = 1970, y = 0.71),
             size = 0.4) +
  geom_image(image = "2020_09_01_crops/beans.png",
             aes(x = 2018, y = 0.9),
             size = 0.4) +
  geom_image(aes(image = image), size = 0.4) +
  geom_image(image = "2020_09_01_crops/beans.png",
             aes(x = 1979, y = 0.85),
             size = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 3.5,
              color = "black") +
  geom_smooth(method = "lm", se = FALSE, size = 2,
              color = "#f9b247") +
  geom_line(color = "black", size = 3.5) +
  geom_line(color = "#01cdfe", size = 2) +
  geom_curve(aes(x = 1980, y = 0.84, xend = 1986.5, yend = 0.68),
             color = "#ea9327",
             arrow = arrow(),
             size = 1, curvature = 0.3) +
  annotate("text", label = "B E A N S",
           x = 1980, y = 0.88, size = 16,
           family = "mono",
           fontface = "bold",
           color = "#01cdfe") +
  geom_curve(aes(x = 2000, y = 0.51, xend = 1993, yend = 0.62),
             color = "#01cdfe",
             arrow = arrow(),
             size = 1, curvature = 0.4) +
  annotate("text", label = "B E A N S !",
           x = 1995, y = 0.48, size = 16,
           family = "mono",
           fontface = "bold",
           color = "#ea9327") +
  scale_x_continuous(expand = c(0, 0), limits = c(1960, 2020)) +
  theme(axis.text = element_text(family = "mono", size = 10, color = "gray50"),
        axis.title = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 25, b = 10, l = 11),
        plot.background = element_rect(fill = "#fffb96", color = NA))

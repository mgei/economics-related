library(comtradr)
library(tidyverse)

q <- ct_search(reporters = "USA", 
               partners = c("Germany", "France", "Japan", "Mexico"), 
               trade_direction = "imports")

q %>% glimpse()

q %>% 
  as_tibble() %>% 
  filter(reporter == "USA", partner == "Mexico") %>% 
  select(trade_value_usd, reporter, partner, everything()) %>% 
  ggplot(aes(x = year, y = trade_value_usd/1000000000)) +
  geom_col(fill = "red", alpha = 0.6) +
  scale_y_continuous(labels = scales::number) +
  scale_x_continuous(breaks = seq(1990, 2018, 2)) +
  labs(title = "US imports from Mexico", y = "billion USD") +
  theme_bw()

?ct_search

####

top_partners <- us %>% 
  filter(partner_iso != "WLD", !is.na(partner_iso)) %>% 
  group_by(partner_iso) %>% 
  tally(trade_value_usd, sort = T) %>% 
  top_n(1) %>% 
  pull(partner_iso)

others <- us %>% 
  filter(year == "2018", !(partner_iso %in% top_partners)) %>% 
  group_by(trade_flow) %>% 
  summarise(trade_value_usd = sum(trade_value_usd)) %>% 
  mutate(partner_iso = "Others")

others

plotdata <- us %>% 
  filter(year == "2018", partner_iso %in% top_partners) %>% 
  bind_rows(others) %>% 
  mutate(partner_iso_fac = factor(partner_iso, levels = c(top_partners, "Others")))

plotdata %>% 
  ggplot(aes(y = trade_value_usd, x = trade_flow)) +
  # a1 +
  geom_col() +
  facet_wrap(~partner_iso_fac, dir = "h", nrow = 4, ncol = 4) -> p


for (ctry in c("MEX", "FRA")) {
  p <- p + annotation_custom2(rasterGrob(img, interpolate=TRUE), 
    ymax = max(plotdata$trade_value_usd), ymin = max(plotdata$trade_value_usd)*0.5, xmin = -0.45, partner_iso_fac = ctry)
}
p  

annotation_custom2(rasterGrob(img, interpolate=TRUE), 
                     ymax = max(plotdata$trade_value_usd), ymin = max(plotdata$trade_value_usd)*0.5,
                     xmin = -0.45, partner_iso_fac = "FRA") +
  annotation_custom2(rasterGrob(img, interpolate=TRUE), 
                     ymax = max(plotdata$trade_value_usd), ymin = max(plotdata$trade_value_usd)*0.5,
                     xmin = -0.45, partner_iso_fac = "MEX")

img <- readPNG("./flags/Mexico.png")

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, 
                                ymin = -Inf, ymax = Inf, 
                                data,
                                partner_iso_fac = "MEX", trade_flow = "Import", trade_value_usd = 1) { 
  layer(data = tibble(partner_iso_fac = partner_iso_fac, trade_flow = trade_flow, trade_value_usd = trade_value_usd), 
        stat = StatIdentity, 
        position = PositionIdentity,
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

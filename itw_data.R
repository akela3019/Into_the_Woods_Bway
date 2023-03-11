library(tidyverse)
library(ggthemr)
library(ggtext)
library(cowplot)
library(extrafont)
library(RColorBrewer)
library(ggpattern)
library(grid)
library(gridExtra)


# Plot theme ----------------------------
grape_theme <- ggthemr('grape', set_theme = FALSE)
camouflage_theme <- ggthemr('camouflage', set_theme = FALSE)
pal <- c("#353525", "saddlebrown", "#BF812D", "#DFC27D",
         "#f3fadc", "#adcc5a", "#6d9a58", "#283314")
swatch <- as.vector(camouflage_theme$palette$swatch)
swatch2 <- as.vector(grape_theme$palette$swatch)
custom <- define_palette(
  swatch = swatch,
  gradient = c(lower = "#F4F4A9", upper = "#6d9a58"),
  background = grape_theme$palette$background,
  text = c("#4f4f4e", "#4f4f4e"),
  line = c("#353525", "#353525"),
  gridline = "#bbb8ab"
)

ggthemr(custom, type = "outer")

theme_custom <- theme(
  text = element_text(family = "Franklin Gothic Medium Cond"),
  plot.background = element_blank(),
  plot.title = element_text(size = 12, family = "Dubai"),
  plot.subtitle = element_text(size = 10, margin = margin(-5, 5, 5, 5), 
                               face = "plain", color = "#757574"),
  plot.title.position = "plot",
  axis.text = element_text(color = "#757574", size = 9),
  panel.grid.major.x = element_line(linetype = "12", linewidth = 0.4),
  panel.grid.major.y = element_blank(),
  panel.background = element_blank(),
  legend.background = element_blank(),
  legend.text = element_text(size = 9.5)
  
)

week_label <- function(x) {
  labels <- paste("WEEK", x)
  labels[x == 1] <- "OPENING" 
  labels}


geom_production_label <- function() {
  list(
    geom_label(data = itw_data %>% group_by(Production) %>% slice(1),
               aes(x = as.Date(Inf), y = Inf, label = Production, 
                   fill = Production, color = Production), 
               family = "Franklin Gothic Medium Cond",
               size = 3, hjust = 1, vjust = 1, inherit.aes = FALSE,
               label.padding = unit(0.12, "lines"), 
               label.r = unit(0.1, "lines")),
    ggforce::facet_col(Production ~ ., scales = "free", space = "free"),
    scale_color_manual(values = c("#e8ece5", "#353525", "#e8ece5"))
  )
}

geom_curve_arrow <- function() {
  x <- labor_day$Week[1] + 1.5
  y <- labor_day$Avg_Gross_Adjusted[1] - 2E3
  xend <- 60; yend <- 5E4
  list(
    geom_curve(data = NULL, x = x, y = y, xend = xend, yend = yend, linetype = "11",
               color = "#9b3950", curvature = 0.15, linewidth = 0.4),
    geom_curve(data = NULL, x = x, y = y, 
               xend = x + 1, yend = y - 1.5E3, 
               color = "#9b3950", curvature = 0.15, linewidth = 0.4,
               arrow = arrow(angle = 25, length = unit(0.05, "inches"), ends = "first", type = "closed")))
}

scale_x_week <- function(reverse = FALSE) {
  if(reverse) {
    list(scale_x_reverse(labels = week_label,
                         breaks = c(1, seq(30, 90, 30)),
                         expand = expansion(add = c(1, 1))))}
  else {
    list(scale_x_continuous(labels = week_label,
                            breaks = c(1, seq(30, 90, 30)),
                            expand = expansion(add = c(1, 1))))}
}


scale_x_month <- function() {
  list(
    scale_x_date(labels = function(x) {
      format <- ifelse(month(x) == 1, "%b<br/><span style='color:#353525'>%Y</span>", "%b")
      strftime(x, paste0("<span>", format, "</span>"))}, 
      expand = expansion(add = 0),
      breaks = as.Date(paste0(c(sapply(c(1988:1989, 2002:2003, 2022:2023), rep, 3)), 
                              rep(c("-01-01", "-05-01", "-09-01"), 3)))),
    theme(axis.text.x = element_markdown(),
          axis.line = element_blank(),
          panel.spacing.y = unit(0, "mm"),
          plot.title = element_text(margin = margin(0, 5, 4, 5)))
  )
}

# Load & clean data -------------------------
CPI_U <- read.csv("data/CPI_U.csv") %>%
  mutate(month = as.numeric(str_remove(Period, "M"))) %>%
  select(year = Year, month, CPI = Value)
CPI_subtitle <- "INFLATION-ADJUSTED, JAN 2023 CPI-U=100"

itw_data_raw <- read.delim("data/Into_the_Woods.txt", sep = "\t", check.names = FALSE) %>%
  rename("WeekEnding" = "Week Ending", "Capacity" = "% Capacity",
         "Previews" = "# Previews", "Perf" = "# Perf.") %>%
  mutate(Gross = as.numeric(str_remove_all(Gross, "\\$|,"))) %>%
  mutate(Attendance = as.numeric(str_remove_all(Attendance, ","))) %>%
  mutate(Capacity = as.numeric(str_remove(Capacity, "%"))) %>%
  mutate(WeekEnding = as.Date(WeekEnding, format = "%b %d, %Y"))  %>%
  arrange(WeekEnding)%>%
  mutate(year = year(WeekEnding),
         month = month(WeekEnding)) %>%
  mutate(Gross = ifelse(Perf == 16, Gross/2, Gross),
         Attendance = ifelse(Perf == 16, Attendance/2, Attendance),
         Perf = ifelse(Perf == 16, Perf/2, Perf)) %>%
  mutate(Production = case_when(
    WeekEnding < as.Date("2000-01-01") ~ "ITW '87",
    WeekEnding < as.Date("2020-01-01") ~ "ITW '02",
    TRUE ~ "ITW '22")) %>%
  mutate(Production = factor(Production, paste0("ITW '", c("87", "02", "22"))))

write_tsv(itw_data_raw, file = "data/Into_the_Woods_cleaned.tsv")

itw_data <- itw_data_raw %>%
  group_by(Production) %>%
  mutate(Week = row_number() - min(which(Perf > 0)) + 1) %>%
  mutate(Avg_Price = Gross/Attendance) %>%
  mutate(Gross_Diff = (Gross - lag(Gross))/lag(Gross) * 100) %>%
  ungroup() %>%
  left_join(CPI_U, by = c("year", "month")) %>%
  mutate(Gross_Adjusted = Gross * tail(CPI_U, 1)$CPI / CPI)%>%
  mutate(Avg_Gross_Adjusted = Gross_Adjusted/(Perf + Previews)) %>%
  mutate(Avg_Price_Adjusted = Avg_Price * tail(CPI_U, 1)$CPI / CPI) 

theatres <- data.frame(
  Production = sort(unique(itw_data$Production)),
  theatre = c("MARTIN BECK", "BROADHURST", "ST. JAMES"),
  n_seats = c(1424, 1218, 1710))


production_labels <- paste0(
  "**INTO THE WOODS '", c("87", "02", "22"),
  "**<br/><span style='color: grey50; font-size:10pt; font-family:\"Franklin Gothic Medium Cond\"'>",
  theatres$theatre, " <span style='color:#8a8a80'>", theatres$n_seats, " SEATS</span></span>")

## Christmas/New Year
xmas_newyear <- itw_data %>%
  filter((month == 12 & WeekEnding >= as.Date(paste0(year, "-12-28"))) |
         (month == 1 & WeekEnding <= as.Date(paste0(year,  "-01-07"))))

## The week after labor day weekend
labor_day <- itw_data %>%
  filter(month == 9) %>%
  group_by(year) %>%
  slice(2) %>% ungroup()

## Labels
label_df <- rbind(
  cbind(xmas_newyear, shape = "Xmas/New Year", label = "\U1F384"),
  cbind(labor_day,    shape = "Labor Day",     label = "\U2692"),
  cbind(subset(itw_data, WeekEnding == as.Date("1989-05-21")),
        shape = "PBS Filming", label = "\U1F3A5"),
  cbind(subset(itw_data, WeekEnding == as.Date("1988-04-17")),
        shape = "Cast Change (Witch:\nJoslyn \u2192 Rashad)", label = "\U21c4")) %>%
  mutate(shape = factor(shape, c("Xmas/New Year", "Labor Day", "PBS Filming", 
                                 "Cast Change (Witch:\nJoslyn \u2192 Rashad)"))) %>%
  mutate(x_pos = WeekEnding - ifelse(str_detect(shape, "Cast"), 4, 1))

dummy <- data.frame(
  WeekEnding = as.Date(c("1987-10-04", "1989-09-03",
                         "2001-10-04", "2003-09-03",
                         "2021-10-04", "2023-09-03")),
  Production = paste0("ITW '", c("87", "87", "02", "02", "22", "22"))) %>%
  mutate(Production = factor(Production, levels(itw_data$Production)))

# Visualization ---------------------
## 1. Gross Difference -----------------------
p_gross_change <- itw_data %>%
  group_by(Production) %>%
  filter(lag(Previews) + lag(Perf) > 5) %>% 
  filter(!is.na(Gross_Diff)) %>%
  ggplot(aes(x = WeekEnding)) +
  geom_hline(yintercept = 0, color = "#4f4f4e", linewidth = 0.4, lty = "12")+
  geom_col(aes(y = Gross_Diff, fill = Production)) +
  geom_blank(data = dummy) +
  scale_x_month() +
  geom_production_label() +
  scale_y_continuous(breaks = seq(-50, 100, 50), expand = expansion(add = c(6, 0)),
                     labels = function(x) {paste0(x, "%")}) +
  labs(x = NULL, y = NULL, title = "% GROSS DIFF. FROM WEEK PRIOR") +
  theme_custom +
  theme(strip.text.x = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(linetype = "12", linewidth = 0.4))

## 2. Attendance ------------------
p_attendance <- itw_data %>%
  left_join(theatres, by = "Production") %>% 
  mutate(max_attenance = Attendance/Capacity * 100 #(Perf + Previews) * n_seats
         ) %>% 
  ggplot(aes(x = WeekEnding, color = Production, fill = Production)) +
  geom_col(aes(y = Attendance), color = "#e8ece5", linewidth = 0.05) +
  geom_line(aes(y = max_attenance), color = "#9b3950", linewidth = 0.5, linetype = "11") +
  geom_blank(data = dummy) +
  geom_production_label() +
  scale_x_month() +
  scale_y_continuous(breaks = seq(0, 12, 6) * 1E3, expand = expansion(mult = c(0.01, 0.01)),
                     labels = function(x) {paste0(x/1000, "K")}, limits = c(0, 15) * 1E3) +
  labs(x = NULL, y = NULL, title = toupper("Attendance")) +
  theme_custom +
  theme(strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 5, 5, 5),
        panel.grid.major.y = element_line(linetype = "12", linewidth = 0.4)) 

## 3. Capacity ----------------
guide_squarekey <- function(...) {
  # Constructor just prepends a different class
  x <- guide_legend(...)
  class(x) <- c("squarekey", class(x))
  x
}

guide_gengrob.squarekey <- function(guide, theme) {
  # Make default legend
  legend <- NextMethod()
  
  # Find the key grobs
  is_key <- startsWith(legend$layout$name, "key-")
  is_key <- is_key & !endsWith(legend$layout$name, "-bg")
  
  # Extract the width of the key column
  key_col <- unique(legend$layout$l[is_key])
  keywidth <- convertUnit(legend$widths[2], "mm", valueOnly = TRUE)
  
  # Set the height of every key to the key width
  legend$grobs[is_key] <- lapply(legend$grobs[is_key], function(key) {
    key$height <- unit(keywidth - 0.5, "mm") # I think 0.5mm is default offset
    key
  })
  legend
}


p_capacity <- itw_data %>%
  ggplot(aes(x = WeekEnding, y = Production)) +
  geom_tile(aes(fill = Capacity), color = "#e8ece5", linewidth = 0.3) +
  geom_vline(data = subset(itw_data, Week == 1), 
             aes(xintercept = WeekEnding - 3), 
             color = "#353525", linewidth = 0.4, lty = "22")+
  geom_blank(data = dummy) +
  geom_point(data = subset(label_df, shape != "PBS Filming"), 
             aes(x = x_pos, shape = shape), size = 2, color = "black") +
  facet_wrap(Production ~ ., scales = "free", ncol = 1) +
  coord_cartesian(expand = FALSE) +
  scale_x_month() +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "RdYlBu")), 
                    right = FALSE, name = NULL, 
                    breaks = seq(30, 110, 10), limits = c(30, 110),
                    labels = function(x) {paste0(x, "%")}) +
  scale_shape_manual(values = c("\U1F384", "\U1F6E0", "\U21c4", "T"), name = NULL, guide = "squarekey") +
  guides(fill = guide_colorbar(barwidth = 13, barheight = 0.3, ticks = FALSE, order = 1),
         shape = guide_squarekey(order = 2, keywidth = 0.5, keyheight = 0.1, nrow = 2, override.aes = list(size = 3)))+
  labs(x = NULL, y = NULL, title = toupper("% Capacity")) +
  theme_custom +
  theme(strip.background = element_blank(),
        strip.text = element_blank(), 
        axis.text.y = element_text(color = "#353525"),
        legend.text = element_text(lineheight = 0.9, vjust = 0.6, size = 9),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "bottom",
        legend.box.spacing = unit(1.2, "mm"),
        legend.justification = "left",
        legend.box.margin = margin(l = -5))


## 4. Average Grosses --------------------------------

p_avg_gross <- itw_data %>%
  group_by(Production) %>% 
  ggplot(aes(x = Week, y = Avg_Gross_Adjusted, color = Production, shape = Production)) +
  geom_vline(xintercept = 1, color = "#353525", linewidth = 0.4, lty = "12")+
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.3) +
  geom_point(data = labor_day, shape = 1, size = 2.5, color = "#9b3950", stroke = 0.8)+
  geom_text(data = data.frame(x = 61, y = 4.2E4, 
                              label = toupper("The Week After\nLabor Day Weekend")), 
            aes(x = x, y = y, label = label), color = "#9b3950", 
            family = "Franklin Gothic Medium Cond", size = 3,
            inherit.aes = FALSE, lineheight = 0.9, hjust = 0) +
  geom_curve_arrow() +
  scale_x_week() +
  scale_y_continuous(labels = function(x) {paste0("$", x/1E6, "M")}) +
  scale_color_discrete(labels = production_labels) +
  scale_shape_discrete(labels = production_labels) +
  guides(color = guide_legend(label.vjust = 0),
         shape = guide_legend(label.vjust = 0)) +
  labs(x = NULL, y = NULL, title = "AVERAGE GROSS PER PERFORMANCE",
       subtitle = CPI_subtitle) +
  theme_custom +
  theme(panel.grid.major.y = element_line(linetype = "12", linewidth = 0.4),
        axis.line.y = element_blank(), 
        legend.position = c(0.72, 0.85),
        legend.title = element_blank(),
        legend.text = element_markdown(lineheight = 1, family = "Dubai", face = 1, 
                                       size = 10.5, hjust = 1, margin = margin(l = -3)),
        legend.key.height = unit(8, "mm"))


## 5. Average Ticket Price ------------------

p_price_adjusted <- itw_data %>%
  ggplot(aes(x = Week + 0.5, y = Production, fill = Avg_Price_Adjusted)) +
  geom_tile(color = "#e8ece5") +
  geom_vline(xintercept = 1, color = "#353525", linewidth = 0.4, linetype = "22")+
  geom_text(data = subset(itw_data, WeekEnding %in% as.Date(c("1988-06-05", "2002-06-02"))),
            aes(x = Week + 1, label = "T"), color = "#353525", size = 2.5, family = "Dubai", fontface = 2) +
  geom_text(data = data.frame(x = 61, y = 1, label = "T: TONY AWARDS"), 
            aes(x = x, y = y, label = label),  hjust = 0,
            family = "Franklin Gothic Medium Cond", 
            color = "#9b3950", size = 3, inherit.aes = FALSE) +
  scale_x_week() +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "Spectral")),
                    breaks = seq(70, 160, 10), name = NULL,
                    labels = function(x) {paste0("$", x)},
                    limits = c(70, 160), right = FALSE) +
  guides(fill = guide_colorbar(barwidth = 17.5, barheight = 0.3, ticks = FALSE)) +
  labs(x = NULL, y = NULL, title = "AVERAGE TICKET PRICE", 
       subtitle = CPI_subtitle) +
  theme_custom +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = "#353525"),
        legend.position = "bottom",
        legend.box.just = "left",
        legend.box.margin = margin())

## 6. Performances -------------------

p_perf <- itw_data %>%
  group_by(Production) %>%
  summarise(Previews = -sum(Previews),
            `Regular Performances` = sum(Perf)) %>%
  pivot_longer(2:3, names_to = "type", values_to = "n") %>%
  mutate(text_pos = ifelse(Production == "ITW '02" & type == "Previews", n - 50, n)) %>%
  mutate(text_col = ifelse(Production == "ITW '02", "#353525", "#e8ece5")) %>%
  ggplot(aes(x = n, y = Production, fill = Production)) +
  geom_col_pattern(aes(pattern = type), width = 0.7, color = NA, linewidth = 0.6,
                   pattern_spacing = 0.15, pattern_colour = NA, pattern_fill = "#4f4f4e",
                   pattern_angle = 45, pattern_size = 0.05) +
  geom_text(aes(x = text_pos, label = abs(n), color = text_col), 
            fontface = "bold", position = position_stack(vjust = 0.5),
            family = "Dubai", hjust = 0.5, vjust = 0.4, size = 3) +
  facet_grid(~ toupper(type), scales = "free_x", space = "free_x") +
  labs(x = NULL, y = NULL, title = "TOTAL NO. OF PERFORMANCES") +
  scale_x_continuous(expand = expansion(add = c(0, 0))) +
  scale_y_discrete(expand = expansion(add = c(0.3, 0.3)),
                   limits = rev) +
  scale_color_identity() +
  scale_pattern_manual(values = c("stripe", "none")) +
  theme_custom +
  theme(panel.spacing.x = unit(0.2, "mm"),
        strip.text = element_text(size = 10, margin = margin(b = 5)),
        strip.clip = "off",
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "#353525"),
        axis.line = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank())

## 7. Poster -------------------------
itw_poster <- png::readPNG("itw_poster_rm_bg_2.png")
itw_poster <- matrix(rgb(itw_poster[,,1], itw_poster[,,2], 
                         itw_poster[,,3], itw_poster[,,4] * 0.7), 
                     nrow = dim(itw_poster)[1])
itw_poster <- rasterGrob(itw_poster, interpolate = TRUE, 
                         x = 1, y = 0, hjust = 1, vjust = 0) 


# Arrange panels ----------------------------
left_col <- plot_grid(
  p_gross_change, p_attendance, p_capacity, 
  ncol = 1, align = "v", rel_heights = c(3.4, 3.1, 3.1))

right_col_top <- plot_grid(
  p_avg_gross, p_price_adjusted + theme(plot.margin = margin(15, 5, 15, 5)), 
  ncol = 1, align = "v", rel_heights = c(2, 1.2))

right_col_bottom <- plot_grid(
  p_perf + theme(plot.margin = margin(0, -150, 15, 5)), itw_poster, 
  nrow = 1, rel_widths = c(1, 1)) +
  theme(plot.margin = margin(5, 5, -15, 5))

right_col <- plot_grid(
  right_col_top, right_col_bottom, 
  ncol = 1, align = "v", rel_heights = c(7, 2.1))


title <- ggdraw() + 
  draw_label("INTO THE WOODS ...", x = 0, hjust = 0, size = 20,
             fontfamily = "Broadway", color = "#353525") +
  draw_label("@akela@mstdn.social", x = Inf, hjust = 1, vjust = 0, size = 11,
             fontfamily = "Dubai", color = "#4f4f4e") +
  theme(plot.margin = margin(5, 0, 10, 7))

caption <- ggdraw() + 
  draw_label("Source: IBDB; Consumer Price Index (CPI) Databases, U.S. Bureau of Labor Statistics", 
             x = 0, hjust = 0, size = 11, color = "#4f4f4e", 
             fontfamily = "Franklin Gothic Medium Cond") +
  theme(plot.margin = margin(5, 0, 10, 7))


g <- plot_grid(left_col, right_col, nrow = 1, rel_widths = c(5, 4)) %>%
  plot_grid(title, ., caption, ncol = 1, rel_heights = c(0.035, 1, 0.03)) +
  theme(plot.background = element_rect(color = NA, fill = "#e8ece5"),
        plot.margin = margin(r = 5, t = 5))

ggsave("itw_data.png", g, width = 10, height = 7.5, dpi = 600)

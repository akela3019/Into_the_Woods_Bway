library(tidyverse)
library(ggthemr)
library(ggtext)
library(cowplot)
library(extrafont)
library(RColorBrewer)
library(ggpattern)

grape_theme <- ggthemr('grape', set_theme = FALSE)
camouflage_theme <- ggthemr('camouflage', set_theme = FALSE)
custom <- define_palette(
  swatch = c(camouflage_theme$palette$swatch),
  gradient = c(lower = "#F4F4A9", upper = "#6d9a58"),
  background = grape_theme$palette$background,
  text = c("#4f4f4e", "#4f4f4e"),
  line = c("#353525", "#353525"),
  gridline = "#bbb8ab"
)

ggthemr(custom, type = "outer")

# Load & clean data -------------------------
CPI_U <- read.csv("CPI_U.csv") %>%
  mutate(month = as.numeric(str_remove(Period, "M"))) %>%
  select(year = Year, month, CPI = Value)

itw_data <- read.delim("Into_the_Woods.txt", sep = "\t", check.names = FALSE) %>%
  rename("WeekEnding" = "Week Ending", "Capacity" = "% Capacity",
         "Previews" = "# Previews", "Perf" = "# Perf.") %>%
  mutate(WeekEnding = as.Date(WeekEnding, format = "%b %d, %Y"))  %>%
  arrange(WeekEnding)%>%
  mutate(year = year(WeekEnding),
         month = month(WeekEnding)) %>%
  mutate(Capacity = as.numeric(str_remove(Capacity, "%"))) %>%
  mutate(Production = case_when(
    WeekEnding < as.Date("2000-01-01") ~ "ITW '87",
    WeekEnding < as.Date("2020-01-01") ~ "ITW '02",
    TRUE ~ "ITW '22")) %>%
  mutate(Production = factor(Production, paste0("ITW '", c("87", "02", "22")))) %>%
  group_by(Production) %>%
  mutate(Week = row_number() - min(which(Perf > 0)) + 1) %>%
  mutate(Gross = as.numeric(str_remove_all(Gross, "\\$|,"))) %>%
  mutate(Attendance = as.numeric(str_remove_all(Attendance, ","))) %>%
  mutate(Avg_Price = Gross/Attendance) %>%
  mutate(Gross_Diff = (Gross - lag(Gross))/lag(Gross) * 100) %>%
  ungroup() %>%
  left_join(CPI_U, by = c("year", "month")) %>%
  mutate(Gross_Adjusted = Gross * tail(CPI_U, 1)$CPI / CPI)%>%
  mutate(Avg_Price_Adjusted = Avg_Price * tail(CPI_U, 1)$CPI / CPI) 


xmas_newyear <- itw_data %>%
  filter((month == 12 & WeekEnding >= as.Date(paste0(year, "-12-28"))) |
           (month == 1 & WeekEnding <= as.Date(paste0(year, "-01-07"))))


# Capacity ----------------

ggplot(itw_data, aes(x = Week, y = Capacity, color = Production)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.4)+
  scale_x_continuous(labels = function(x) {
    labels <- paste("Week", x)
    labels[x == 1] <- "Opening" 
    labels},
    breaks = c(1, seq(20, 100, 20)),
    expand = expansion(add = c(2, 2))) +
  scale_y_continuous(labels = function(x) {paste0(x, "%")},
                     breaks = seq(40, 100, 20)) +
  geom_point(data = label_df, aes(x = Week + 0.2, shape = shape), size = 2.5) +
  scale_shape_manual(values = c("\U1F384", "\U2692", "\U1F3A5", "\U1F504"), name = NULL) +
  labs(x = NULL, y = NULL, title = "% CAPACITY") +
  theme(text = element_text(family = "Dubai", size = 13),
        legend.background = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14))
ggsave("figures/a.png", width = 9, height = 5)


labor_day <- itw_data %>%
  filter(month == 9) %>%
  group_by(year) %>%
  slice(2) %>% ungroup()

"Bernadette Peters: Sep 29, 1987 - Mar 27, 1988
 Betsy Joslyn:      Mar 29, 1988 - Apr 13, 1988
 Phylicia Rashad:   Apr 14, 1988 - Jul 03, 1988
 Betsy Joslyn:      Jul 05, 1988 - Dec 11, 1988
 Ellen Foley:       Aug 01, 1989 - Sep 03, 1989
 "


label_df <- rbind(
  cbind(xmas_newyear, shape = "Xmas/New Year"),
  cbind(labor_day, shape = "Labor Day"),
  cbind(subset(itw_data, WeekEnding == as.Date("1989-05-21")), 
        shape = "PBS Filming"),
  cbind(subset(itw_data, WeekEnding == as.Date("1988-04-17")), 
        shape = "Witch: Joslyn\u2192Rashad")) %>%
  mutate(shape = factor(shape, c("Xmas/New Year", "Labor Day", "PBS Filming", "Witch: Joslyn\u2192Rashad")))
  
itw_data %>%
  ggplot(aes(x = Week + 0.5, y = Production, fill = Capacity)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_point(data = label_df, aes(x = Week + 0.2, shape = shape), size = 2.5) +
  scale_x_continuous(labels = function(x) {
    labels <- paste("Week", x)
    labels[x == 1] <- "Opening" 
    labels},
    breaks = c(1, seq(20, 100, 20))) +
  geom_vline(xintercept = 1, color = "black", linewidth = 0.4, lty = "32")+
  scale_y_discrete(limits = rev) +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "RdYlBu")), 
                    breaks = seq(30, 110, 10), name = NULL, limits = c(30, 110),
                    labels = function(x) {paste0(x, "%")}) +
  scale_shape_manual(values = c("\U1F384", "\U2692", "\U1F3A5", "\U1F504"), name = NULL) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 0.5, ticks = FALSE))+
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "% Capacity") +
  theme(text = element_text(family = "Corbel", size = 13),
        plot.title = element_text(hjust = 0.5, size = 14),
        panel.grid.major.x = element_line(linetype = "32", linewidth = 0.4),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical")
  
ggsave("figures/Capacity.png", width = 7, height = 2.8)

# Performances -------------------
itw_data %>%
  group_by(Production) %>%
  summarise(Previews = -sum(Previews),
            `Regular Performances` = sum(Perf)) %>%
  pivot_longer(2:3, names_to = "type", values_to = "n") %>%
  mutate(text_pos = ifelse(n > 0, n - 25, n + 20)) %>%
  mutate(text_pos = ifelse(Production == "ITW '02" & type == "Previews", n - 10, text_pos)) %>%
  mutate(text_col = ifelse(Production == "ITW '02", "#353525", "#e8ece5")) %>%
  ggplot(aes(x = n, y = Production, fill = Production)) +
  geom_col_pattern(aes(pattern = type), width = 0.7, color = "#e8ece5", linewidth = 0.6,
                   pattern_spacing = 0.15, pattern_colour = NA, pattern_fill = "#353525",
                   pattern_angle = 45, pattern_size = 0.05) +
  geom_text(aes(x = text_pos, label = abs(n), color = text_col), fontface = "bold",
            family = "Corbel", hjust = 0.5, vjust = 0.4, size = 3.5) +
  facet_grid(~ type, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = NULL, title = "NO. OF PERFORMANCES") +
  scale_x_continuous(expand = expansion(add = c(0, 0))) +
  scale_y_discrete(expand = expansion(add = c(0.3, 0.3)),
                   limits = rev) +
  scale_color_identity() +
  scale_pattern_manual(values = c("stripe", "none")) +
  theme(text = element_text(family = "Corbel"),
        panel.grid = element_blank(),
        panel.spacing.x = unit(0, "mm"),
        strip.text = element_text(face = "bold", size = 10, margin = margin(b = 5)),
        strip.clip = "off",
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold"),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 12, hjust = -0.12, face = "bold"))
ggsave("figures/performances.png", width = 7, height = 2.5)

# Average Ticket Price ------------------
## Unadjusted ----------------------------
itw_data %>%
  ggplot(aes(x = Week, y = Production, fill = Avg_Price)) +
  geom_tile(color = "#e8ece5") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(labels = function(x) {
    labels <- paste("Week", x)
    labels[x == 1] <- "Opening" 
    labels},
    breaks = c(1, seq(20, 100, 20))) +
  scale_y_discrete(limits = rev) +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "Spectral")), 
                    breaks = seq(30, 150, 10), name = NULL, limits = range(itw_data$Avg_Price)) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 12, ticks = FALSE)) +
  labs(x = NULL, y = NULL, title = "UNADJUSTED ($)") +
  theme(text = element_text(family = "Corbel"),
        plot.title = element_text(family = "Dubai", size = 12),
        axis.line.y = element_blank())

itw_data %>%
  ggplot(aes(x = Week, y = Production, fill = Avg_Price_Adjusted)) +
  geom_tile(color = "#e8ece5") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(labels = function(x) {
    labels <- paste("Week", x)
    labels[x == 1] <- "Opening" 
    labels},
    breaks = c(1, seq(20, 100, 20))) +
  scale_y_discrete(limits = rev) +
  scale_fill_stepsn(colors = rev(brewer.pal(11, "Spectral")), 
                    breaks = seq(30, 150, 10), name = NULL,
                    limits = range(itw_data$Avg_Price)) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 12, ticks = FALSE)) +
  labs(x = NULL, y = NULL, title = "INFLATION-ADJUSTED, JAN 2023 CPI-U=100 ($)") +
  theme(text = element_text(family = "Corbel"),
        plot.title = element_text(family = "Franklin Gothic Medium"),
        axis.line.y = element_blank())

# Grosses --------------------------------
itw_data %>%
  group_by(Production) %>%
  mutate(Week = row_number() - 1) %>%
  mutate(Gross_cumsum = cumsum(Gross)) %>% ungroup %>% 
  ggplot(aes(x = Week, y = Gross, fill = Production, color = Production)) +
  geom_line() +
  geom_area(position = "identity", alpha = 0.4)


# Gross Difference -----------------------
itw_data %>%
  group_by(Production) %>%
  filter(lag(Previews) + lag(Perf) > 5) %>% 
  filter(!is.na(Gross_Diff)) %>%
  ggplot(aes(x = Week, y = Gross_Diff, fill = Production)) +
  geom_col(aes(color = Production), width = 1, show.legend = FALSE, linewidth = 0.3) +
  geom_point(data = label_df, aes(shape = shape), size = 2.5) +
  scale_x_reverse(labels = function(x) {
    labels <- paste("Week", x)
    labels[x == 1] <- "Opening" 
    labels},
    breaks = c(1, seq(20, 100, 20)),
    expand = expansion(add = c(2, 2))) +
  scale_y_continuous(breaks = seq(-100, 100, 50),
                     labels = function(x) {paste0(x, "%")},
                     expand = expansion(mult = c(0.06, 0.06))) +
  scale_color_manual(values = c("black", "black", "white")) +
  scale_shape_manual(values = c("\U1F384", "\U2692", "\U1F3A5", "\U1F504"), name = NULL) +
  geom_label(data = itw_data %>% group_by(Production) %>% slice(1),
            aes(x = -Inf, y = Inf, label = Production), hjust = 1,  vjust = 1,
            fill = "#353525", family = "Corbel", color = "#e8ece5", size = 3,
            label.padding = unit(0.15, "lines"),
            label.r = unit(0.02, "lines")) +
  facet_grid(Production ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Corbel"),
        legend.position = "bottom",
        strip.text = element_blank(),
        panel.background = element_rect(color = "#353525"),
        panel.spacing.y = unit(0, "mm")) +
  coord_flip()
ggsave("figures/gross_change.png", width = 6, height = 7)

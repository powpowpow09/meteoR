library(meteoR)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(nasapower)


dir <- "f:/Geek/R/meteo"
# file_lst <- netatmo_file_to_load(file.path(dir, "csv"), pattern = list("Interieur_salon", "Interieur_bureau", "Interieur_chambre", "Exterieur", "Pluviometre", "Anemometre"))
file_lst <- netatmo_file_to_load(file.path(dir, "csv"), pattern = list("Exterieur", "Pluviometre"))
db_lst <- load_csv(file_lst)
listviewer::jsonedit(db_lst)
csv_to_rds(rds_directory = file.path(dir, "rds"), file_lst = file_lst)

db_indoor <- netatmo_indoor$new(db_lst$db_netatmo_indoor)
db_indoor$summary()

db_outdoor <- netatmo_outdoor$new(db_lst$db_netatmo_outdoor)
db_outdoor$summary()
db_outdoor$summary(n = 99)


tmp <- db_outdoor$data %>%
  mutate(id2 = paste(year, month, sep = "_")) %>%
  group_by(date, id2) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  group_by(id2) %>%
  summarise(min = min(min_temp, na.rm = TRUE), max = max(max_temp, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -id2)
gr <- ggplot(tmp, aes(x = id2, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point()
ggplotly(gr)


tmp <- db_outdoor$data %>%
  group_by(date, month_abbr) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -date, -month_abbr)
gr <- ggplot(tmp, aes(x = date, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  geom_point(alpha = 0.6, aes(color = tmp$month_abbr)) +
  scale_color_viridis_d()
ggplotly(gr)


tmp <- db_outdoor$data %>%
  select(year, month_abbr, temperature) %>%
  mutate(year = as.factor(year),
         month_abbr = factor(month_abbr, ordered = TRUE, levels = c("janv", "févr", "mars", "avr", "mai", "juin", "juil", "août", "sept", "oct", "nov", "déc"))) %>%
  filter(month_abbr == "juin")
gr <- ggplot(tmp, aes(x = month_abbr, y = temperature, fill = year)) +
  theme_bw() +
  xlab("") +
  geom_boxplot()
gr
ggplotly(gr)


# comparaison avec data nasapower
db_nasapower <- get_power(
  community = "AG",
  lonlat = c(2.4945, 44.3445),
  pars = c("RH2M", "T2M", "T2M_MIN", "T2M_MAX", "T10M", "T10M_MIN", "T10M_MAX", "WS2M", "WS2M_MIN", "WS2M_MAX", "WS10M", "WS10M_MIN", "WS10M_MAX", "PRECTOT"),
  dates = c(min(db_outdoor$data$date), max(db_outdoor$data$date)),
  site_elevation = 528,
  temporal_average = "DAILY"
)

db_study <- db_outdoor$data %>%
  select(date, temperature, humidity, rain, wind_strength, gust_strength) %>%
  group_by(date) %>%
  summarise(
    RH2M = mean(humidity, na.rm = TRUE),
    PRECTOT = sum(rain, na.rm = TRUE),
    TM = mean(temperature, na.rm = TRUE),
    TMIN = min(temperature, na.rm = TRUE),
    TMAX = max(temperature, na.rm = TRUE),
    WM = mean(wind_strength, na.rm = TRUE),
    WMIN = min(wind_strength, na.rm = TRUE),
    WMAX = max(wind_strength, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(date >= "2017-03-01") %>%
  inner_join(db_nasapower %>%
               transmute(date = YYYYMMDD,
                         RH2M_nasa = RH2M,
                         PRECTOT_nasa = PRECTOT,
                         T2M_nasa = T2M,
                         T2MIN_nasa = T2M_MIN,
                         T2MAX_nasa = T2M_MAX,
                         T10M_nasa = T10M,
                         T10MIN_nasa = T10M_MIN,
                         T10MAX_nasa = T10M_MAX,
                         WS2M_nasa = WS2M,
                         WS2MIN_nasa = WS2M_MIN,
                         WS2MAX_nasa = WS2M_MAX,
                         WS10M_nasa = WS10M,
                         WS10MIN_nasa = WS10M_MIN,
                         WS10MAX_nasa = WS10M_MAX
               ))

heatmap(cor(na.omit(db_study[, -1])))

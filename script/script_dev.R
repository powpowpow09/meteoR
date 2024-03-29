library(meteoR)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(nasapower)


dir <- "f:/Geek/R/meteo"
file_lst <- netatmo_file_to_load(file.path(dir, "csv"), pattern = list("Interieur_salon", "Interieur_bureau", "Interieur_chambre", "Exterieur", "Pluviometre", "Anemometre"))
db_lst <- load_csv(file_lst)
listviewer::jsonedit(db_lst)
csv_to_rds(rds_directory = file.path(dir, "rds"), file_lst = file_lst)

db_indoor <- netatmo_indoor$new(db_lst$db_netatmo_indoor)
db_indoor$summary()

db_outdoor <- netatmo_outdoor$new(db_lst$db_netatmo_outdoor)
db_outdoor$summary()


boo <- db_outdoor$data %>%
  mutate(id2 = paste(year, month, sep = "_")) %>%
  group_by(date, id2) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  group_by(id2) %>%
  summarise(min = min(min_temp, na.rm = TRUE), max = max(max_temp, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -id2)
gr <- ggplot(boo, aes(x = id2, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point()
ggplotly(gr)


boo <- db_outdoor$data %>%
  group_by(date, month_abbr) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -date, -month_abbr)
gr <- ggplot(boo, aes(x = date, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  geom_point(alpha = 0.6, aes(color = boo$month_abbr)) +
  scale_color_viridis_d()
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

pairsCorr(db_study %>% select(starts_with("T")))
pairsCorr(db_study %>% select(starts_with("P")))
pairsCorr(db_study %>% select(starts_with("R")))
pairsCorr(db_study %>% select(starts_with("W")))

tmp <- db_study %>% select(starts_with("P"))
plot_lm(tmp$PRECTOT, tmp$PRECTOT_nasa)
tmp <- db_study %>% select(starts_with("T"))
plot_lm(tmp$TM, tmp$T2M_nasa)
plot_lm(tmp$TM, tmp$T10M_nasa)
plot_lm(tmp$TMIN, tmp$T2MIN_nasa)
plot_lm(tmp$TMAX, tmp$T2MAX_nasa)

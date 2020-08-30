library(Rnetatmo)
library(ggplot2)
library(dplyr)
library(tidyr)


dir <- "f:/Geek/R/netatmo"
file_lst <- netatmo_file_to_load(file.path(dir, "csv"), pattern = list("Interieur_salon", "Interieur_bureau", "Interieur_chambre", "Exterieur", "Pluviometre", "Anemometre"))
db_lst <- load_csv(file_lst)
listviewer::jsonedit(db_lst)
csv_to_rds(rds_directory = file.path(dir, "rds"), file_lst = file_lst)

db_indoor <- netatmo_indoor$new(db_lst$db_netatmo_indoor)
db_indoor$summary()

db_outdoor <- netatmo_outdoor$new(db_lst$db_netatmo_outdoor)
db_outdoor$summary()


as_tibble(boo)
boo2 <- db_outdoor$data %>%
  mutate(id2 = paste(year, month, sep = "_")) %>%
  group_by(date, id2) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  group_by(id2) %>%
  summarise(min = min(min_temp, na.rm = TRUE), max = max(max_temp, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -id2)
gr <- ggplot(boo2, aes(x = id2, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point()
ggplotly(gr)


boo3 <- db_outdoor$data %>%
  group_by(date, month_abbr) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), max_temp = max(temperature, na.rm = TRUE)) %>%
  gather(temp_idc, value = temp, -date, -month_abbr)
gr <- ggplot(boo3, aes(x = date, y = temp, group = temp_idc, color = temp_idc)) +
  theme_bw() +
  xlab("") +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  geom_point(alpha = 0.6, aes(color = boo3$month_abbr)) +
  scale_color_viridis_d()
ggplotly(gr)


db_outdoor$data %>%
  mutate(id = paste(year, month, sep = "_")) %>%
  ggplot(aes(x = id, y = temperature)) + geom_point()

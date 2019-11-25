library(nycflights13)
library(dplyr)
library(ggplot2)

flights
str(flights)

summary(flights)

flights %>% 
  distinct(carrier)

flights_tbl <- flights %>% 
  left_join(airlines, by = "carrier")

not_cancel <- flights_tbl %>% 
  filter(!is.na(dep_delay))

qplot(x = name, data = not_cancel) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

ggplot(data = not_cancel, mapping = aes(x = reorder(name, name, function(x)length(x)))) +
  geom_bar() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))

# mengurutkan barplot berdasarkan value
# # 1. Buat dulu tabel frekuensi dari kategori yang diinginkan
# # 2. gunakan fungsi reorder() pada aestethic x dan n sbg y
# # 3. gunakan stat = "identity" pada geom_bar()

not_cancel <- not_cancel %>% 
  count(name) 

ggplot(data = not_cancel, mapping = aes(x = reorder(name, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

not_cancel <- not_cancel %>% 
  mutate(pct = n/sum(n))

ggplot(data = not_cancel, mapping = aes(x = reorder(name, -pct), y = pct)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

canceled <- flights_tbl %>% 
  filter(is.na(dep_delay)) %>% 
  count(name) %>% 
  mutate(pct = n/sum(n))

g <- ggplot(data = canceled, mapping = aes(x = reorder(name, -pct), y = pct)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

g

g +
  labs(title = "Persentase Maskapai Yang Sering Melakukan Cancel",
       x = "Maskapai",
       y = "Persentase") +
  theme(axis.title.y = element_text(angle = 0, vjust = 1))

ggplot(data = canceled, mapping = aes(x = reorder(name, -pct), y = pct, fill = name)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(angle = 0, vjust = 1),
        legend.position = "none") +
  labs(title = "Persentase Maskapai Yang Sering Melakukan Cancel",
       x = "Maskapai",
       y = "Persentase")

ggplot(data = canceled, mapping = aes(x = reorder(name, -pct), y = pct*100, fill = name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(pct*100, 2), "%")),
            vjust = -0.25) +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(angle = 0, vjust = 1),
        legend.position = "none") +
  labs(title = "Persentase Maskapai Yang Sering Melakukan Cancel",
       x = "Maskapai",
       y = "Persentase")

ggplot(data = canceled, mapping = aes(x = reorder(name, pct), y = pct*100, fill = name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(pct*100, 2), "%")), 
            position = position_dodge(0.9), 
            hjust = -0.01) +
  theme(legend.position = "none") +
  labs(title = "Persentase Maskapai Yang Sering Melakukan Cancel",
       x = "Maskapai",
       y = "Persentase") +
  coord_flip()


# Histogram & Density

g <- ggplot(data = flights, mapping = aes(x = air_time))

g +
  geom_histogram()

g +
  geom_histogram(bins = 50, fill = "skyblue")

g +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue")

g +
  geom_density(fill = "skyblue", alpha = 0.8)

# Boxplot

ggplot(data = flights, mapping = aes(x = name, y = dep_delay)) +
  geom_boxplot()

ggplot(data = flights, mapping = aes(x = "Statistics", y = dep_delay)) +
  geom_boxplot()

ggplot(data = flights, mapping = aes(x = name, y = dep_delay)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = flights, mapping = aes(x = name, y = dep_delay)) +
  geom_boxplot(color = "skyblue") +
  coord_flip()

# https://ggplot2.tidyverse.org/reference/geom_boxplot.html


# Scatter plot

flights_aug <- flights %>% 
  filter(!is.na(dep_delay) & month == 8) %>% 
  mutate(tgl = as.Date(paste(year, month, day, sep = "-"))) %>% 
  group_by(tgl) %>% 
  summarise(n = n()) 

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_point()

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_point(size = 2) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")


# Line plot/time series plot

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_line()

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_line(size = 1, color = "skyblue")

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  scale_x_date(breaks = "days") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  scale_x_date(breaks = "days", date_labels = "%b %d")

ggplot(data = flights_aug, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  scale_x_date(breaks = "days", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")


# Facet

flights_aug2 <- flights_tbl %>% 
  inner_join(airports, by = c("origin" = "faa"), suffix = c("_carrier", "_originairports")) %>% 
  filter(!is.na(dep_delay) & month == 8) %>% 
  mutate(tgl = as.Date(paste(year, month, day, sep = "-"))) %>% 
  group_by(name_originairports, tgl) %>% 
  summarise(n = n()) 


ggplot(data = flights_aug2, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  facet_grid(rows = vars(name_originairports)) +
  scale_x_date(breaks = "days", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")

ggplot(data = flights_aug2, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  facet_grid(name_originairports ~ . ) +
  scale_x_date(breaks = "days", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")

ggplot(data = flights_aug2, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  facet_grid(cols = vars(name_originairports)) +
  # scale_x_date(breaks = "days", date_labels = "%d") +
  # theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")

ggplot(data = flights_aug2, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  facet_grid( . ~ name_originairports) +
  # scale_x_date(breaks = "days", date_labels = "%d") +
  # theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")

ggplot(data = flights_aug2, mapping = aes(x = tgl, y = n)) + 
  geom_line(color = "skyblue") +
  geom_point() +
  facet_grid(rows = vars(), cols = vars(name_originairports)) +
  # scale_x_date(breaks = "days", date_labels = "%d") +
  # theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Jumlah Penerbangan Harian Bulan Agustus 2013",
       x = "Tanggal",
       y = "Jumlah")

ggsave("plotR.jpg")

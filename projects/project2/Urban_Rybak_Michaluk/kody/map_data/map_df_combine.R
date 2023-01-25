library(dplyr)

df_combine <- function(name1, name2) {
  raw1 <- read.csv(name1, encoding = 'UTF-8')
  raw2 <- read.csv(name2, encoding = 'UTF-8')
  df1 <- raw1 %>%
    select(placeVisit_location_name, placeVisit_location_address,
           placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp,
           placeVisit_location_latitudeE7, placeVisit_location_longitudeE7
           )
  df2 <- raw2 %>%
    select(placeVisit_location_name, placeVisit_location_address,
           placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp,
           placeVisit_location_latitudeE7, placeVisit_location_longitudeE7
    )
  df <- rbind(df1, df2)
  return(df)
}

df_raw_t <- df_combine("raw_data/december_data_t", "raw_data/january_data_t")

first_row_with_widok_19 <- df_raw_t %>% filter(grepl("Widok 19", placeVisit_location_address)) %>% slice(1)
first_row_with_golkow <- df_raw_t %>% filter(grepl("Gołków", placeVisit_location_address)) %>% slice(1)
first_row_with_sphinx <- df_raw_t %>% filter(grepl("Sphinx", placeVisit_location_name)) %>% slice(1)
first_row_with_wydzial <- df_raw_t %>% filter(grepl("Wydział", placeVisit_location_name)) %>% slice(1)

df_raw_t <- df_raw_t %>%
  mutate(placeVisit_location_address = if_else(grepl("Widok 19", placeVisit_location_address),
                                               first_row_with_widok_19$placeVisit_location_address, placeVisit_location_address),
         placeVisit_location_latitudeE7 = if_else(grepl("Widok 19", placeVisit_location_address),
                                                  first_row_with_widok_19$placeVisit_location_latitudeE7, placeVisit_location_latitudeE7),
         placeVisit_location_longitudeE7 = if_else(grepl("Widok 19", placeVisit_location_address),
                                                   first_row_with_widok_19$placeVisit_location_longitudeE7, placeVisit_location_longitudeE7)) %>%
  mutate(placeVisit_location_address = if_else(grepl("Gołków", placeVisit_location_address),
                                               first_row_with_golkow$placeVisit_location_address, placeVisit_location_address),
         placeVisit_location_latitudeE7 = if_else(grepl("Gołków", placeVisit_location_address),
                                                  first_row_with_golkow$placeVisit_location_latitudeE7, placeVisit_location_latitudeE7),
         placeVisit_location_longitudeE7 = if_else(grepl("Gołków", placeVisit_location_address),
                                                   first_row_with_golkow$placeVisit_location_longitudeE7, placeVisit_location_longitudeE7)) %>%
  mutate(placeVisit_location_address = if_else(grepl("Sphinx", placeVisit_location_name),
                                               first_row_with_sphinx$placeVisit_location_name, placeVisit_location_name),
         placeVisit_location_latitudeE7 = if_else(grepl("Sphinx", placeVisit_location_address),
                                                  first_row_with_sphinx$placeVisit_location_latitudeE7, placeVisit_location_latitudeE7),
         placeVisit_location_longitudeE7 = if_else(grepl("Sphinx", placeVisit_location_name),
                                                   first_row_with_sphinx$placeVisit_location_longitudeE7, placeVisit_location_longitudeE7)) %>%
  mutate(placeVisit_location_address = if_else(grepl("Wydział", placeVisit_location_name),
                                               first_row_with_wydzial$placeVisit_location_address, placeVisit_location_address),
         placeVisit_location_latitudeE7 = if_else(grepl("Wydział", placeVisit_location_name),
                                                  first_row_with_wydzial$placeVisit_location_latitudeE7, placeVisit_location_latitudeE7),
         placeVisit_location_longitudeE7 = if_else(grepl("Wydział", placeVisit_location_name),
                                                   first_row_with_wydzial$placeVisit_location_longitudeE7, placeVisit_location_longitudeE7))
df_raw_w <- df_combine("raw_data/december_data_w", "raw_data/january_data_w")

df_raw_w <- df_raw_w %>%
  mutate(placeVisit_location_name = case_when(
    grepl("Zielona 8", placeVisit_location_address) ~ "Łęgowo",
    TRUE ~ placeVisit_location_name
  ))

df_raw_c <- df_combine("raw_data/december_data_c", "raw_data/january_data_c")

df_raw_c <- df_raw_c %>%
  mutate(placeVisit_location_name = case_when(
    grepl("Morelowa 9", placeVisit_location_address) ~ "Home",
    TRUE ~ placeVisit_location_name
  ))

# df_raw_t %>%
#   group_by(placeVisit_location_name, placeVisit_location_address,
#            placeVisit_location_latitudeE7, placeVisit_location_longitudeE7) %>%
#   summarise(n = n()) %>%
#   print(n = 65)

df_t <- df_raw_t %>%
  mutate(person = "Tymek") %>%
  mutate(color = '#F4B400')

df_w <- df_raw_w %>%
  mutate(person = "Wojtek") %>%
  mutate(color = "#0F9D58")


df_c <- df_raw_c %>%
  mutate(person = "Czarek") %>%
  mutate(color = "#4285F4")

df_raw = rbind(df_t, df_w, df_c)

map_df <- df_raw %>%
  filter(placeVisit_location_address != "") %>%
  mutate(date = substr(placeVisit_duration_startTimestamp, 1, 10)) %>%
  mutate(lat = placeVisit_location_latitudeE7 / 10000000) %>%
  mutate(lng = placeVisit_location_longitudeE7 / 10000000)

write.csv(map_df, file = "map_data/map_df.csv", row.names = FALSE)

dat.pop.sex <- readxl::read_excel(paste0("data_raw/pop_total_age.xlsx")) %>%
  filter(Year==1880 | Year == 1900) %>%
  group_by(Year, Bezirk, sex, MapName) %>%
  summarise(Pop=sum(population)) %>%
  ungroup() %>%
  mutate(Year = as.factor(Year))


load("data/data_age90.RData")
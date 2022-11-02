dat.pop <- readxl::read_excel(paste0("data_raw/Population_Age1888.xlsx")) %>%
  filter(!Agegroups=="Total") %>%
  rename(Bezirk = Name) %>%
  rename(age_group = Agegroups) %>%
  mutate(age_group = replace(age_group, age_group=="0-4","0_79"),
                  age_group = replace(age_group, age_group=="5-9","0_79"),
                  age_group = replace(age_group, age_group=="10-14","0_79"),
                  age_group = replace(age_group, age_group=="15-19","0_79"),
                  age_group = replace(age_group, age_group=="20-24","0_79"),
                  age_group = replace(age_group, age_group=="25-29","0_79"),
                  age_group = replace(age_group, age_group=="20-29","0_79"),
                  age_group = replace(age_group, age_group=="30-34","0_79"),
                  age_group = replace(age_group, age_group=="35-39","0_79"),
                  age_group = replace(age_group, age_group=="40-44","0_79"),
                  age_group = replace(age_group, age_group=="45-49","0_79"),
                  age_group = replace(age_group, age_group=="50-54","0_79"),
                  age_group = replace(age_group, age_group=="55-59","0_79"),
                  age_group = replace(age_group, age_group=="60-64","0_79"),
                  age_group = replace(age_group, age_group=="65-69","0_79"),
                  age_group = replace(age_group, age_group=="70-74","0_79"),
                  age_group = replace(age_group, age_group=="75-79","0_79"),
                  age_group = replace(age_group, age_group=="80-84",">79"),
                  age_group = replace(age_group, age_group=="85-89",">79"),
                  age_group = replace(age_group, age_group=="90-95",">79"),
                  age_group = replace(age_group, age_group==">=80",">79"),
                  age_group = replace(age_group, age_group==">=95",">79"),
                  age_group = replace(age_group, age_group=="30-39","0_79"),
                  age_group = replace(age_group, age_group=="40-49","0_79"),
                  age_group = replace(age_group, age_group=="50-59","0_79"),
                  age_group = replace(age_group, age_group=="60-69","0_79"),
                  age_group = replace(age_group, age_group=="70-79","0_79")) %>%
  group_by(Bezirk,age_group) %>%
  mutate(Pop=sum(Total)) %>%
  ungroup() %>%
  distinct(Bezirk, age_group, .keep_all = TRUE)

dat.pop.80 <- dat.pop  %>%
  group_by(Bezirk) %>%
  summarise(Pop_total=sum(Pop)) %>%
  ungroup() %>%
  full_join(dat.pop) %>%
  mutate(Prop80 = (Pop/Pop_total)*100) %>%
  filter(age_group == ">79")

save(dat.pop.80 ,file=paste0("data/dat.pop.80.RData"))
write.xlsx(dat.pop.80,file=paste0("data/dat.pop.80.xlsx"),rowNames=FALSE, overwrite = TRUE)

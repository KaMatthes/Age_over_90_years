data.cofactors.1888 <- read_excel("data_raw/Cofactors_1888.xlsx") %>%
  mutate(Prop80 = as.numeric(Prop80))

data.cofactors <-  read_excel("data_raw/Cofactors_1900.xlsx") %>%
  mutate(height  = as.numeric(height),
         TB = as.numeric(TB),
         TB_1888 = as.numeric(TB_1888)) %>%
  full_join(data.cofactors.1888 ) %>%
      mutate(U90_inc=  U90/population*10000,  
             Mortality=death/population*10000,
             hosp_group = ifelse( hospitals>0, ">=1",  hospitals),
             hosp_group = factor( hosp_group, levels = c("0", ">=1"))) %>%
  select(Year, U90_inc, Language, Mortality, BIP, height, note_1=note1, Prop_80=Prop80, TB=TB_1888) %>%
  gather(., condition, measurement, Mortality:TB) %>%
  mutate(condition = recode(condition,
                            "BIP" = "GDP per capita",
                            "height" = "Height",
                            "note_1" = "Educational test",
                            "Prop_80" = "Prop.80+ years",
                            "TB" = "Tb"),
         condition = factor(condition, levels = c("GDP per capita","Height","Mortality",
                                                   "Educational test", "Prop.80+ years","Tb")))


Supplement_Figure6 <- ggplot(data=data.cofactors) +
  geom_point(aes(y=U90_inc, x=measurement, col=Language), lwd=2) +
  geom_smooth(aes(y=U90_inc, x=measurement),  method='rlm',se=TRUE, col="grey30") +
  facet_grid(Year~condition, scales="free_x") +
  xlab("")+
  ylab(">= 90-years-old per 10'000 inhabitants")+
  scale_color_manual("Language: ",
                    breaks=c("Deutsch", "Französisch", "Italienisch"),
                    labels=c("German", "French", "Italien"),
                    values =  c(cbp1[1],cbp1[2], cbp1[3]))  +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title =element_text(size=20),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/Supplement_Figure6.pdf", Supplement_Figure6 ,base_height=10,base_width=25)
  


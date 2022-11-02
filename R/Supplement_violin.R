function_violin <- function() {
  
data.cofactors.1888 <- read_excel("../data_raw/Cofactors_1888.xlsx") %>%
  mutate(Prop80 = as.numeric(Prop80))

data.cofactors <-  read_excel("../data_raw/Cofactors_1900.xlsx") %>%
  mutate(height  = as.numeric(height),
         TB = as.numeric(TB),
         TB_1888 = as.numeric(TB_1888)) %>%
  full_join(data.cofactors.1888 ) %>%
      mutate(U90_inc=  U90/population*10000,  
             Mortality=death/population*10000,
             hosp_group = ifelse( hospitals>0, ">=1",  hospitals),
             hosp_group = factor( hosp_group, levels = c("0", ">=1")),
             Language = as.factor(Language)) %>%
  select(Year, U90_inc, hosp_group, Language) %>%
  mutate(tempx = 1,
         hosp_group = factor(hosp_group, levels=c("0",">=1")))
  
  
Figure_hosp <- ggplot(data=data.cofactors,aes(y=U90_inc,x=tempx, fill = hosp_group)) +
  # geom_split_violin() +
  geom_violin(draw_quantiles = c(0.5)) +
  facet_grid(~Year) +
  # stat_summary(fun = median,
  #              width = 0.25,
  #              position = position_dodge(width = .25),
  #              # aes(shape="median"), 
  #              colour = "black",
  #              geom = "crossbar",
  #              show.legend=FALSE) +
  xlab("")+
  ylab(">= 90-years-old per 10'000 inhabitants")+
    scale_fill_manual("Hospitals: ",values =  c(cbp1[1],cbp1[2]))  +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text=element_text(size=18),
        axis.text=element_text(color="black",size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=15),
        legend.position = "right")


Figure_language <- ggplot(data=data.cofactors,aes(y=U90_inc,x=tempx, fill = Language)) +
  geom_violin(draw_quantiles = c(0.5)) +
  facet_grid(~Year) +
  xlab("")+
  ylab(">= 90-years-old per 10'000 inhabitants")+
  scale_fill_manual("Language: ",
                    breaks=c("Deutsch", "Französisch", "Italienisch"),
                    labels=c("German", "French", "Italien"),
                    values =  c(cbp1[1],cbp1[2], cbp1[3]))  +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text=element_text(size=18),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=15),
        legend.position = "right")



Figure_violin <- plot_grid(Figure_hosp, Figure_language, nrow = 2)

Figure_violin
# cowplot::save_plot("output/Figure_violin.pdf", Figure_violin ,base_height=15,base_width=15)
  

}
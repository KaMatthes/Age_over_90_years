function_dist_age <- function() {
data_age <- readxl::read_excel("../data_raw/Age_distribution.xlsx")

data_per <- data_age %>%
  mutate(sex= as.factor(sex),
         age90 = ifelse(age <90, 1, 2)) %>%
  group_by(age90, sex,year) %>%
  summarise(N=sum(number))

data_t <- data_age %>%
  mutate(sex= as.factor(sex),
         age90 = ifelse(age <90, 1, 2)) %>%
  group_by(sex,year) %>%
  summarise(N_t=sum(number)) %>%
  full_join(data_per) %>%
  mutate(perc90 = N/N_t*100)

dat_text_female <- data.frame(
  label = c("Female: 0.016%", "Female: 0.022%"),
   year   = c(1888, 1900)
)

dat_text_male <- data.frame(
  label = c("Male: 0.016%", "Male: 0.016%"),
  year   = c(1888, 1900)
)


Figure2 <- ggplot(data=data_age) +
  # geom_line(aes(x=age, y=number, col=sex), lwd=1.5) +
  geom_bar(aes(x=age, y=number,fill=sex),stat="identity") +
  annotate("rect",xmin=90,xmax=100,ymin=0,ymax=Inf, alpha=0.2,fill="wheat4") +
  annotate("text",x=67,y=65000, label="Percentage of >= 90-years-old:", size=6) +
  facet_grid(~year) +
  geom_text(data = dat_text_female,mapping = aes(x = 56.2, y = 61500, label = label),size=6)+
  geom_text(data = dat_text_male,mapping = aes(x =54.5, y = 58000, label = label), size=6)+
  scale_fill_manual("Sex: ",values =  c(cbp1[2],cbp1[1])) +
  # ggtitle("")+
  ylab("Number of inhabitants")+
  xlab("Age in years")+
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=15),
        legend.position = "bottom")


Figure2

# cowplot::save_plot("output/Figure2.pdf", Figure2,base_height=10,base_width=20)

}

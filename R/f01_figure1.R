rm(list=ls())
source("R/00_setup.R")


data_age <- readxl::read_excel("data/Age_distribution.xlsx")

data_per <- data_age %>%
  mutate(
    sex= as.factor(sex),
    age90 = ifelse(age <90, 1, 2)
    ) %>%
  group_by(age90, sex,year) %>%
  summarise(N=sum(number))

data_t <- data_age %>%
  mutate(
    sex= as.factor(sex),
    age90 = ifelse(age <90, 1, 2)
    ) %>%
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


ggplot(data=data_age) +
  # geom_line(aes(x=age, y=number, col=sex), lwd=1.5) +
  geom_bar(aes(x=age, y=number,fill=sex),stat="identity") +
  annotate("rect",xmin=90,xmax=100,ymin=0,ymax=Inf, alpha=0.2,fill=cbp1[5]) +
  annotate("text",x=59,y=65000, label="Percentage of >= 90-years-old:", size=7) +
  facet_grid(~year) +
  geom_text(data = dat_text_female,mapping = aes(x = 54.2, y = 59500, label = label),size=7)+
  geom_text(data = dat_text_male,mapping = aes(x =52.2, y = 55000, label = label), size=7)+
  scale_fill_manual("Sex ",values =  c(cbp1[2],cbp1[1])) +
  # ggtitle("")+
  ylab("Population")+
  xlab("Age in years")+
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text = element_text(size=size_striptext-5),
        axis.text=element_text(size=size_legend-8),
        axis.title=element_text(size=size_legend-5),
        legend.text=element_text(size=size_legend-5),
        legend.title =element_text(size=size_legend-5),
        plot.title = element_text(size=size_legend-5),
        legend.position = "bottom")



ggsave("output/Figure1.pdf",height=10, width=18)

ggsave("output/Figure1.png",height=10, width=18,  dpi = 1000)


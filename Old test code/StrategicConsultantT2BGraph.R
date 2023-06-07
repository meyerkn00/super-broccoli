##INITALIZE

# load the below libraries before you start

pacman::p_load("tidyverse","dplyr","readr","showtext")

#Add apercu font

font_add(family = "apercu", regular = "Apercu-Regular.otf")
showtext_auto()

#Load in data
testsub <- read_csv("OpsLeadsGuitarTestColwise.csv")
testsub$subq <- as.factor(testsub$subq)
testsub$subqtext <- as.factor(testsub$subqtext)

#Test out of 5 instead of percentage. In the real data the mean will be out of 5 instead of out of 100%
testsub$val1<-testsub$val*5


##GRAPH

plot5<-ggplot(testsub,aes(x=subq,y=val1)) +
  geom_point(aes(color=region),size=5) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  #scale_shape_manual(values=c("West"=21, "Midwest"=22,"Northeast"=23,
       #                       "Atlantic"=24,"Southwest"=25,"Southeast"=19))
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    #legend.text=element_text(size=18),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    legend.key=element_blank(),
    legend.title=element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black"),
    panel.ontop = FALSE)+
  coord_flip()

plot5

ggsave(filename="SCT2B_NEW.png",plot=plot5,width=8,height=4)

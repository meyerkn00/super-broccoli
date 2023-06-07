##INITALIZE

# load the below libraries before you start

pacman::p_load("tidyverse","dplyr","readr","showtext")

#Add apercu font

font_add(family = "apercu", regular = "Apercu-Regular.otf")
showtext_auto()

#Load in data
superq <- read_csv("Raw Data/SuperQuestionData11.17.22.csv")
#superq$subq <- as.factor(superq$subq)
superq$qtext <- as.factor(superq$qtext)

#Turn qtext into factor and sort by level of importance from drivers (note: done manually by qorder, starting with least importance)

qorder<-c("Liason","Area Advisor","Restaurant Consultant","Strategic Consultant","Trusted Advisor")

superq$qtext.r<-factor(superq$qtext, levels=qorder)
levels(superq$qtext.r)

#Filter data by year, only use rows where include = 1 (not total, STC, or no region)
superq2022<- filter(superq, Year==2022, include==1)
superq2021<- filter(superq, Year==2021, include==1)


###TEST Code that adds labels for easier checking
testplot <-ggplot(superq2022,aes(x=qtext.r,y=val)) +
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(.3,.73)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  theme( legend.position="bottom") +
  coord_flip()
testplot
###

##GRAPHS - 2022

sqplot2022 <-ggplot(superq2022,aes(x=qtext.r,y=val)) +
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(.3,.73)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    legend.position="none", #set "bottom" for bottom, default is right
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

sqplot2022

ggsave(filename=paste("Images/RolePlot/SQPLOT22.",format(Sys.Date(),"%m.%d" ),".png",sep=""),plot=sqplot2022,width=8,height=4)

  #2021

sqplot2021 <-ggplot(superq2021,aes(x=qtext.r,y=val)) +
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(.3,.73)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    legend.position="none",
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

sqplot2021

ggsave(filename=paste("Images/RolePlot/SQPLOT21.",format(Sys.time(),"%m.%d" ),".png",sep=""),plot=sqplot2021,width=8,height=4)

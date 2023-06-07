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
#levels(superq$qtext.r)

#Filter data by year, only use rows where include = 1 (not total, STC, or region=none)
superq2022<- filter(superq, Year==2022, include==1)
superq2021<- filter(superq, Year==2021, include==1)


###TEST Code that adds labels for easier checking
#testplot <-ggplot(superq2022,aes(x=qtext.r,y=val)) +
#  geom_point(aes(color=region),size=5) +
#  scale_y_continuous(limits=c(.3,.73)) +
#  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
#                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
#  theme( legend.position="bottom") +
#  coord_flip()
#testplot
###

##GRAPHS - 2022
min(superq2022$val)
minval<-.3
max(superq2022$val)
maxval<-.65


sqplot2022 <-ggplot(superq2022,aes(x=qtext.r,y=val)) +
  geom_dotplot(aes(fill=region,color=region),binwidth=.04,stackdir="center",binaxis="y",
               dotsize=.2,stackgroups = TRUE,
               binpositions="all"
  ) +
  scale_y_continuous(limits=c(minval,maxval),breaks=c(minval,maxval),
                     labels = c(minval=paste(minval*100,"%",sep=""),
                                maxval=paste(maxval*100,"%",sep=""))) +
  scale_fill_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                             "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=40,family="apercu"),
    legend.text=element_text(size=18),
    legend.position="none",
    #axis.text.x=element_blank(),
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

ggsave(filename=paste("Images/RolePlot/SQPLOT22.",format(Sys.time(), format="%m.%d.%y+%H.%M"), ".png",sep="")
       ,plot=sqplot2022,width=8,height=4)

  #2021

min(superq2021$val)
minval1<-.3
max(superq2021$val)
maxval1<-.75


sqplot2021 <-ggplot(superq2021,aes(x=qtext.r,y=val)) +
  geom_dotplot(aes(fill=region,color=region),binwidth=.05,stackdir="center",binaxis="y",
               dotsize=.2,stackgroups = TRUE,
               binpositions="all"
  ) +
  scale_y_continuous(limits=c(minval1,maxval1),breaks=c(minval1,maxval1),
                     labels = c(minval1=paste(minval1*100,"%",sep=""),
                                maxval1=paste(maxval1*100,"%",sep=""))) +
  scale_fill_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                             "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=40,family="apercu"),
    legend.text=element_text(size=18),
    legend.position="none",
    #axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    legend.key=element_blank(),
    legend.title=element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black"),
    panel.ontop = FALSE)+
  coord_flip()

#sqplot2021

ggsave(filename=paste("Images/RolePlot/SQPLOT21.",format(Sys.time(), format="%m.%d.%y+%H.%M"), ".png",sep="")
       ,plot=sqplot2021,width=8,height=4)

##INITALIZE

# load the below libraries before you start

pacman::p_load("tidyverse","dplyr","readr","showtext")

#Add apercu font

font_add(family = "apercu", regular = "Apercu-Regular.otf")
showtext_auto()

#Load in data
q78 <- read_csv("Raw Data/Q7Q8Data11.4.22.csv")
#q78$subq <- as.factor(q78$subq)
#q78$subqtext <- as.factor(q78$subqtext)
#str(q78)

# Create tables for each year and question

q72022<- filter(q78, qnumber==7 & include==1 & Year==2022)
q72021<- filter(q78, qnumber==7 & include==1 & Year==2021)
q82022<- filter(q78, qnumber==8 & include==1 & Year==2022)
q82021<- filter(q78, qnumber==8 & include==1 & Year==2021)

##GRAPH Q72022

q72022plot <-ggplot(q72022,aes(x=qnumber,y=val)) +
  geom_vline(aes(xintercept=7),color="black")+
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  scale_x_discrete(breaks=c(7))+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    #legend.text=element_text(size=18),
    legend.position="none",
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


ggsave(filename=paste("Images/Q7/q72022plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q72022plot,width=8,height=4)

#q72021plot

q72021plot <-ggplot(q72021,aes(x=qnumber,y=val)) +
  geom_vline(aes(xintercept=7),color="black")+
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  scale_x_discrete(breaks=c(8))+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    #legend.text=element_text(size=18),
    legend.position="none",
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

ggsave(filename=paste("Images/Q7/q72021plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q72021plot,width=8,height=4)

#Question 7

q82022plot <-ggplot(q82022,aes(x=qnumber,y=val)) +
  geom_vline(aes(xintercept=8),color="black")+
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  scale_x_discrete(breaks=c(8))+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    #legend.text=element_text(size=18),
    legend.position="none",
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

ggsave(filename=paste("Images/Q8/q82022plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q82022plot,width=8,height=4)

q82021plot <-ggplot(q82021,aes(x=qnumber,y=val)) +
  geom_vline(aes(xintercept=8),color="black")+
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  scale_x_discrete(breaks=c(8))+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    #legend.text=element_text(size=18),
    legend.position="none",
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

ggsave(filename=paste("Images/Q8/q82021plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q82021plot,width=8,height=4)

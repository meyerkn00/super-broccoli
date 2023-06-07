##INITALIZE

# load the below libraries before you start

pacman::p_load("tidyverse","dplyr","readr","showtext")

#Add apercu font

font_add(family = "apercu", regular = "Apercu-Regular.otf")
showtext_auto()

#Load in data
q78.t <- read_csv("Raw Data/Q7Q8Data11.12.22.csv")
#q78$subq <- as.factor(q78$subq)
#q78$subqtext <- as.factor(q78$subqtext)
#str(q78)

# Create tables for each year and question

q82021.t<- filter(q78.t, qnumber==8 & include==1 & Year==2021)
q8all.t<-filter(q78.t, qnumber==8 & include==1)

q8all.t$qnumber<-as.factor(q8all.t$qnumber)
q82021.t$qnumber<-as.factor(q82021.t$qnumber)
str(q82021.t)
#Q72022
head(q82021.t$qnumber)

q82021plot.t <-ggplot(q82021.t,aes(x=qnumber,y=val)) +
  #geom_vline(aes(xintercept=8),color="black")+
  geom_point(aes(color=region),size=5) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
  #scale_x_discrete(breaks=c(8))+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    legend.text=element_text(size=18),
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

q82021plot.t

ggsave(filename=paste("Images/Test/q72022plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q72022plot,width=8,height=4)

#Test
###Test plot 2 works as a dotplot
testplot2 <-ggplot(q82021.t,aes(x=qnumber,y=val)) +
  geom_dotplot(aes(fill=region,color=region),binwidth=.05,stackdir="center",binaxis="y",
               dotsize=.2,stackgroups = TRUE#,
               #binpositions="all"
               ) +
  scale_y_continuous(limits=c(0.3,0.65)) +
  scale_fill_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                             "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40"))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=58/.pt,family="apercu"),
    legend.text=element_text(size=18),
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
  #Note, I do not want to set binpositions=all. 
  #Currently, it bins every .05 in x as a bucket. So .4 and .45 would be binned together for example
  #If binpositions=all, then any two points that fall within .05 would be binned. So .39 and .42 for example.
  #This is visually confusing, so I have left it as the default
                     
testplot2

###

#Testing displaying multiple years via facet

p1 <-ggplot(q8all.t,aes(x=qnumber,y=val)) +
  geom_dotplot(aes(fill=region,color=region),binwidth=.05,stackdir="center",binaxis="y",
               dotsize=.2,stackgroups = TRUE#,
               #binpositions="all"
  ) +
  scale_y_continuous(limits=c(0.3,0.65),breaks=c(0.3,0.65),labels = c("0.3"="30%","0.65"="65%")) +
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

p2 <- p1 +
  facet_grid(Year ~ .,
             scales="free",space="free")+
  theme(strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.spacing = unit(0, "pt"))

p2


#ggsave("testmultiplot.png",p2,)
ggsave(filename=paste("Images/RolePlot/testmulti.",format(Sys.Date(),"%m.%d" ),".png",sep=""),plot=p2,width=8,height=4)




#Note: this one does not work, it is better to have y=continous and then call coord_flip

dotplot.t1<-ggplot(q82021.t,aes(x=val,y=qnumber)) +
  geom_dotplot(aes(fill=region, color=region),  binaxis="x", stackdir = "center", stackgroups=TRUE, binpositions="all", binwidth=.01) +
  scale_x_continuous(limits=c(0.3,0.65)) +
  scale_fill_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                          "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  scale_color_manual(values=c("Atlantic"="#1C62A6", "Midwest"="#D90134","Northeast"="#F6BB00",
                             "Southeast"="#7030A0","Southwest"="#6C5200","West"="#008E40")) +
  labs(x=NULL, y=NULL)+
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
    panel.ontop = FALSE)

dotplot.t1

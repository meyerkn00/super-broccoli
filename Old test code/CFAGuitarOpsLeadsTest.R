# load the below libraries before you start

pacman::p_load("tidyverse","dplyr","readr","showtext")

#Add apercu font

font_add(family = "apercu", regular = "Apercu-Regular.otf")
showtext_auto()

#Load in data
testsub <- read_csv("OpsLeadsGuitarTestColwise.csv")
testsub$subq <- as.factor(testsub$subq)


#------- Phoriz is how I made the proof of concept
plot<-ggplot(testsub, aes(x=subq,y=val)) +
  geom_dotplot(binaxis='y',stackdir='center', dotsize=0.6)

phoriz <- plot + coord_flip()

phoriz

#--- same thing with color
plot1<-ggplot(testsub, aes(x=subq,y=val,color=region)) +
  geom_dotplot(binaxis='y',stackdir='center', dotsize=0.6) +
  guides(color = guide_legend(override.aes = list(size = 3) ) )

phoriz1 <- plot1 + coord_flip()

phoriz1

#--- same thing with geompoint, here is how I got ProofofConcept2
plot2<-ggplot(testsub, aes(x=subq,y=val)) +
  geom_point(aes(color=region, shape=region),size=4)

phoriz2 <- plot2 + coord_flip()

phoriz2

# Note, below overrides normal size call and replaces with different size, in case we need that
# guides(color = guide_legend(override.aes = list(size = 4) ) )

#--- Now I want to specify color and shape, instead of having it generated randomly
#How I made proof of concept 3
plot3<-ggplot(testsub,aes(x=subq,y=val, color=region, shape=region)) +
  geom_point(aes(color=region, shape=region),size=4) +
  scale_color_manual(values=c("West"="#008E40", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Atlantic"="#1C62A6","Southwest"="#6C5200","Southeast"="#7030A0")) +
  scale_shape_manual(values=c("West"=21, "Midwest"=22,"Northeast"=23,
                              "Atlantic"=24,"Southwest"=25,"Southeast"=19))

phoriz3 <- plot3 + coord_flip()

phoriz3

phoriz3.1 <- phoriz3 + 
  theme(
  text=element_text(size=14,family="apercu"),
  legend.text=element_text(size=9),
  panel.background = element_rect(fill = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(colour = "black"),
  panel.ontop = FALSE
  
)

phoriz3.1
#Plot 4

plot4<-ggplot(testsub,aes(x=subq,y=val)) +
  geom_point(aes(color=region, shape=region),size=4) +
  scale_color_manual(values=c("West"="#008E40", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Atlantic"="#1C62A6","Southwest"="#6C5200","Southeast"="#7030A0")) +
  #scale_fill_manual(values=c("West"="#008E40", "Midwest"="#D90134","Northeast"="#F6BB00",
                           #  "Atlantic"="#1C62A6","Southwest"="#6C5200","Southeast"="#7030A0")) +
  scale_shape_manual(values=c("West"=0, "Midwest"=1,"Northeast"=2,
                              "Atlantic"=5,"Southwest"=6,"Southeast"=19))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=14,family="apercu"),
    legend.text=element_text(size=9),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black"),
    panel.ontop = FALSE
    
  )

phoriz4 <- plot4 + coord_flip()

phoriz4

#Plot 4 - Original Stratcont2b plot in case something gets messed up

plot4<-ggplot(testsub,aes(x=subq,y=val)) +
  geom_point(aes(color=region, shape=region),size=4) +
  scale_color_manual(values=c("West"="#008E40", "Midwest"="#D90134","Northeast"="#F6BB00",
                              "Atlantic"="#1C62A6","Southwest"="#6C5200","Southeast"="#7030A0")) +
  scale_shape_manual(values=c("West"=15, "Midwest"=16,"Northeast"=17,
                              "Atlantic"=18,"Southwest"=19,"Southeast"=19))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=18/.pt,family="apercu"),
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

plot4

ggsave(filename="SCT2B.png",plot=plot4,width=8,height=4)

ggsave(filename="SCT2B_1.png",plot=plot4,width=8,height=4,units="in")

#Some Notes

#Remove gridlines

p1 + theme(
  
  panel.grid.major.y = element_blank(),
  
  panel.grid.minor.y = element_blank()
  
)

#Add grid over white background

p1 + theme(
  
  panel.grid.major.y = element_blank(),
  
  panel.grid.minor.y = element_blank()
  
)

#Add just y gridlines over white background

p1 + theme(
  
  panel.background = element_rect(fill = NA),
  
  panel.grid.major.y = element_line(colour = "grey50"),
  
  panel.ontop = TRUE
  
)

#—Custom Fonts (library showtext)




#Custom fonts are added by first,

#Finding the font you want to use (I’ve picked a few from 1001 Free Fonts and Font Space, but there are many more out there)
#Download the font .ttf() file and unzip if needed
#Use font_add() to register the font
#Run showtext_auto() to load the fonts
#Note: need .ttf file!

font_add(family = "docktrin", regular = "./fonts/docktrin/docktrin.ttf")
showtext_auto()

#Amatic SC can now be used by changing the font family to “amatic-sc”. For R to know how to properly render the text we first need to run showtext_auto() prior to displaying the plot. One downside is it currently does not display in Rstudio. Either open a new graphics window with windows() or save as an external file e.g. .png.

# Note, below overrides normal size call and replaces with different size, in case we need that
# guides(color = guide_legend(override.aes = list(size = 4) ) )

#below is a junkyard

restcon <- read_csv("OpsLeadsGuitarTest.csv")

column_to_rownames(restcon, var="Rownames")
restcon<-restcon[,2:7]

head(restcon)

ggplot(data=RestCon) + geom_boxplot(binaxis=, )

p<-ggplot(restcon, aes(x="Sub1",)) + 
  geom_dotplot(binaxis='y', stackdir='center')

p
plot(y=restcon[1])

#Test
data(diamonds)
ggplot(data = diamonds, aes(x = carat, y = price, color = cut) ) +
  geom_point(alpha = .25, size = 1)

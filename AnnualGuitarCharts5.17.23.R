# Initialize --------------------------------------------------------------

## load the below libraries before you start
pacman::p_load("tidyverse","dplyr","readr","showtext","forcats")

##Add apercu font

font_add(family = "apercu", regular = "Fonts/Apercu-Regular.otf")
showtext_auto()


# Load and Clean Data -----------------------------------------------------

##Load in data
rawdata <- read_csv("Raw Data/GCData5.19.csv")
rawdata$Category <- as.factor(rawdata$Category)

##Filter out unused ASTs

rawdata.r<-rawdata %>% select(-c(Canada, `Puerto Rico`, Unclassified))

##Turn data from "wide" to "long", I want one column with AST name, one with attribute name, and another with value
rawdata.pivot<-rawdata.r %>% pivot_longer(cols=!c(Tag,Category))

##Normalize values out of 100% to make up for different scales
#First I normalize all values for a 6 point scale, which is value-1/(n-1) where n is the number of scale points 
#(in this case 6)
rawdata.pivot$value.r<-(rawdata.pivot$value-1)/5

#Then I overwrite the Satisfaction normalized values to be out of 5 scale points (since only SAT was 5 point)
#Using the same function value-1/(n-1), where n is 5
rawdata.pivot$value.r[rawdata.pivot$Category=="Satisfaction"]<-(rawdata.pivot$value[rawdata.pivot$Category=="Satisfaction"]-1)/4
rawdata.pivot$value.r<-as.numeric(rawdata.pivot$value.r)

## Create subsets of data for each graph, INCLUDING TOTAL

chart1_data<- filter(rawdata.pivot, Category=="Satisfaction" | 
                     Category=="Engagement" | 
                     Category=="Care" | 
                     Category=="ROI" | 
                     Category=="Autonomy" |
                     Category=="Confidence to Grow" |
                     Category=="Support Needed to Grow" |
                     Category=="Total"
                     )
chart1_total<- filter(chart1_data, name=="Total")

#Chart 2 is defined as NOT the categories used in chart 1, except total is included
chart2_data<-filter(rawdata.pivot, Category!="Satisfaction" & 
                      Category!="Engagement" & 
                      Category!="Care" & 
                      Category!="ROI" &
                      Category!="Autonomy" &
                      Category!="Confidence to Grow" &
                      Category!="Support Needed to Grow"
)
chart2_total<- filter(chart2_data, name=="Total")

##Chart 2 data needs to be summarized, which is combining all of the factors that make up the larger categories
#I do this by grouping by Category and Name, so the data is Organized into subsets like "AT1, Clarity and Belonging"
#Then for each of these groups, I take the mean of value.r, which creates 1 number for the whole section (e.g. Clarity and Belonging)
#Note, this means that the data column for chart1 is value.r, while for chart2 is value (sloppy, I know. Sorry)

chart2_data.r<- chart2_data %>% 
  group_by(Category, name) %>% 
  summarize(value=mean(value.r))

# Summary Statistics ------------------------------------------------------


chart1summary<- chart1_data %>% 
  group_by(Category) %>%
  summarize(
    Mean = scales::percent(mean(value.r)),
    Median = scales::percent(median(value.r)),
    Min = scales::percent(min(value.r)),
    Max = scales::percent(max(value.r))
  )

#chart1summary

#write.csv(chart1summary, file="Chart1summary.csv")


chart2summary<- chart2_data.r %>% 
  group_by(Category) %>%
  summarize(
    Mean = scales::percent(mean(value)),
    Median = scales::percent(median(value)),
    Min = scales::percent(min(value)),
    Max = scales::percent(max(value))
  )

#chart2summary

#write.csv(chart2summary, file="Chart2summary.csv")

# Chart 1 -----------------------------------------------------------------

## Dot Plot Chart 1 ##

chart1plot <- chart1_data %>% 
  mutate(Category = fct_relevel(Category,
                                "Confidence to Grow",
                                "Support Needed to Grow",
                                "ROI",
                                "Care",
                                "Autonomy",
                                "Engagement",
                                "Satisfaction")) %>%
  ggplot(aes(x=Category,y=value.r)) +
  geom_dotplot(aes(fill=name,color=name),binwidth=.005,stackdir="center",binaxis="y",
               dotsize=1,stackgroups = TRUE,
               binpositions="all",
               stroke=2
  ) +
  scale_y_continuous(limits=c(0.54,.87)) +
  scale_fill_manual("Region",values=c(
                             "Total"="#FF0000",
                             "AT Team #1"="#A11622",
                             "AT Team #2"="#EE4B2B",
                             "AT Team #3"="#FE517A",
                             "MW Team #1"="#00008B",
                             "MW Team #2"="#1C62A6",
                             "NE Team #1"="#00B050",
                             "NE Team #2"="#92D050",
                             "SE Team #1"="#FFFF00",
                             "SE Team #2"="#FFC000",
                             "SE Team #3"="#ED7817",
                             "SW Team #1"="#7030A0",
                             "SW Team #2"="#A162D0",
                             "SW Team #3"="#CDACE6",
                             "West Team #1"="#282F34",
                             "West Team #2"="#7D8E99",
                             "West Team #3"="#A8B3BB"
  ))+
  scale_color_manual("Region",values=c(
                              "Total"="#000000",
                              "AT Team #1"="#A11622",
                              "AT Team #2"="#EE4B2B",
                              "AT Team #3"="#FE517A",
                              "MW Team #1"="#00008B",
                              "MW Team #2"="#1C62A6",
                              "NE Team #1"="#00B050",
                              "NE Team #2"="#92D050",
                              "SE Team #1"="#FFFF00",
                              "SE Team #2"="#FFC000",
                              "SE Team #3"="#ED7817",
                              "SW Team #1"="#7030A0",
                              "SW Team #2"="#A162D0",
                              "SW Team #3"="#CDACE6",
                              "West Team #1"="#282F34",
                              "West Team #2"="#7D8E99",
                              "West Team #3"="#A8B3BB"
  ))+
  #geom_point()
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=72/.pt,family="apercu"),
    legend.text=element_text(size=54/.pt),
    #legend.position="none",
    #axis.text=element_blank(),
    #legend.spacing.y = unit(.0005, "mm"),
    legend.key.width = unit(.55,"in"),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    legend.key=element_blank(),
    #legend.title=element_blank()
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#D5E7F8"),
    panel.ontop = FALSE)+
  coord_flip()
  #guides(colour = guide_legend(byrow=TRUE, override.aes = list(size=14)))
#chart1plot

ggsave(filename=paste("Images/Chart1plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),dpi=96,
       plot=chart1plot,width=24,height=9,units="in")

# Chart 2 -----------------------------------------------------------------
range(chart2_data.r$value)
range(chart1_data$value.r)
chart2plot <-ggplot(chart2_data.r,aes(x=Category,y=value)) +
  geom_dotplot(aes(fill=name,color=name),binwidth=.005,stackdir="center",binaxis="y",
               dotsize=1,stackgroups = TRUE,
               binpositions="all",
               stroke=2
  ) +
  scale_y_continuous(limits=c(0.54,.87)) +
  scale_fill_manual("Region",values=c(
                             "Total"="#FF0000",
                             "AT Team #1"="#A11622",
                             "AT Team #2"="#FF0000",
                             "AT Team #3"="#FE517A",
                             "MW Team #1"="#00008B",
                             "MW Team #2"="#1C62A6",
                             "NE Team #1"="#00B050",
                             "NE Team #2"="#92D050",
                             "SE Team #1"="#FFFF00",
                             "SE Team #2"="#FFC000",
                             "SE Team #3"="#ED7817",
                             "SW Team #1"="#7030A0",
                             "SW Team #2"="#A162D0",
                             "SW Team #3"="#CDACE6",
                             "West Team #1"="#282F34",
                             "West Team #2"="#7D8E99",
                             "West Team #3"="#A8B3BB"
  ))+
  scale_color_manual("Region",values=c(
                              "Total"="#000000",
                              "AT Team #1"="#A11622",
                              "AT Team #2"="#FF0000",
                              "AT Team #3"="#FE517A",
                              "MW Team #1"="#00008B",
                              "MW Team #2"="#1C62A6",
                              "NE Team #1"="#00B050",
                              "NE Team #2"="#92D050",
                              "SE Team #1"="#FFFF00",
                              "SE Team #2"="#FFC000",
                              "SE Team #3"="#ED7817",
                              "SW Team #1"="#7030A0",
                              "SW Team #2"="#A162D0",
                              "SW Team #3"="#CDACE6",
                              "West Team #1"="#282F34",
                              "West Team #2"="#7D8E99",
                              "West Team #3"="#A8B3BB"
  ))+
  labs(x=NULL, y=NULL)+
  theme(
    text=element_text(size=72/.pt,family="apercu"),
    legend.text=element_text(size=54/.pt),
    #legend.position="none",
    #axis.text=element_blank(),
    legend.key.width = unit(.55,"in"),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    legend.key=element_blank(),
    #legend.title=element_blank()
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#D5E7F8"),
    panel.ontop = FALSE
    )+
  coord_flip()
  #guides(colour = guide_legend(byrow=TRUE, override.aes = list(size=2/.pt)))
         #fill = guide_legend(override.aes = list(size=.05)))
   #      color = guide_legend(override.aes = list(size=14)))
  #coord_flip()


n=n+1
ggsave(filename=paste("Images/Chart2plot.",n,format(Sys.Date(),"%m.%d"),".png", sep=""),
       dpi=96,plot=chart2plot,width=24,height=9,units="in")

chart2plot

# Old Charts --------------------------------------------------------------

# chart1plot <-ggplot(chart1_data,aes(x=Category,y=value.r)) +
#   #geom_vline(aes(xintercept=8),color="black")+
#   geom_point(aes(color=name),size=5) +
#   scale_y_continuous(limits=c(0.5,1)) +
#   scale_color_manual(values=c("AT Team #1"="#A11622",
#                               "AT Team #2"="#FF0000",
#                               "AT Team #3"="#FE517A",
#                               "MW Team #1"="#00008B",
#                               "MW Team #2"="#1C62A6",
#                               "NE Team #1"="#00B050",
#                               "NE Team #2"="#92D050",
#                               "SE Team #1"="#FFFF00",
#                               "SE Team #2"="#FFC000",
#                               "SE Team #3"="#ED7817",
#                               "SW Team #1"="#7030A0",
#                               "SW Team #2"="#A162D0",
#                               "SW Team #3"="#CDACE6",
#                               "West Team #1"="#282F34",
#                               "West Team #2"="#7D8E99",
#                               "West Team #3"="#A8B3BB"
#                               ))+
#   labs(x=NULL, y=NULL)+
#   #scale_x_discrete(breaks=c(8))+
#   theme(
#     text=element_text(size=58/.pt,family="apercu"),
#     legend.text=element_text(size=18),
#     #legend.position="none",
#     #axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     legend.key=element_blank(),
#     legend.title=element_blank(),
#     panel.background = element_rect(fill = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(colour = "#D5E7F8"),
#     panel.ontop = FALSE)+
#   coord_flip()
# 
# chart1plot
# 
# 
# ggsave(filename=paste("Images/Test/q72022plot.",format(Sys.Date(),"%m.%d"),".png", sep=""),plot=q72022plot,width=8,height=4)
# 
# 
# chart1plot <-ggplot(chart1_data,aes(x=Category,y=value.r)) +
#   #geom_vline(aes(xintercept=8),color="black")+
#   geom_point(aes(color=name),size=5) +
#   scale_y_continuous(limits=c(0.5,1)) +
#   scale_color_manual(values=c("AT Team #1"="#A11622",
#                               "AT Team #2"="#FF0000",
#                               "AT Team #3"="#FE517A",
#                               "MW Team #1"="#00008B",
#                               "MW Team #2"="#1C62A6",
#                               "NE Team #1"="#00B050",
#                               "NE Team #2"="#92D050",
#                               "SE Team #1"="#FFFF00",
#                               "SE Team #2"="#FFC000",
#                               "SE Team #3"="#ED7817",
#                               "SW Team #1"="#7030A0",
#                               "SW Team #2"="#A162D0",
#                               "SW Team #3"="#CDACE6",
#                               "West Team #1"="#282F34",
#                               "West Team #2"="#7D8E99",
#                               "West Team #3"="#A8B3BB"
#   ))+
#   labs(x=NULL, y=NULL)+
#   #scale_x_discrete(breaks=c(8))+
#   theme(
#     text=element_text(size=58/.pt,family="apercu"),
#     legend.text=element_text(size=18),
#     #legend.position="none",
#     #axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     legend.key=element_blank(),
#     legend.title=element_blank(),
#     panel.background = element_rect(fill = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(colour = "#D5E7F8"),
#     panel.ontop = FALSE)+
#   coord_flip()
# 
# chart1plot
# 
# testplot2 <-ggplot(chart1_data,aes(x=Category,y=value.r)) +
#   geom_dotplot(aes(fill=name,color=NULL),binwidth=.01,stackdir="center",binaxis="y",
#                dotsize=.5,stackgroups = TRUE,
#                binpositions="all"
#   ) +
#   #scale_y_continuous(limits=c(0.5,1)) +
#   scale_fill_manual(values=c("AT Team #1"="#A11622",
#                              "AT Team #2"="#FF0000",
#                              "AT Team #3"="#FE517A",
#                              "MW Team #1"="#00008B",
#                              "MW Team #2"="#1C62A6",
#                              "NE Team #1"="#00B050",
#                              "NE Team #2"="#92D050",
#                              "SE Team #1"="#FFFF00",
#                              "SE Team #2"="#FFC000",
#                              "SE Team #3"="#ED7817",
#                              "SW Team #1"="#7030A0",
#                              "SW Team #2"="#A162D0",
#                              "SW Team #3"="#CDACE6",
#                              "West Team #1"="#282F34",
#                              "West Team #2"="#7D8E99",
#                              "West Team #3"="#A8B3BB"
#   ))+
#   labs(x=NULL, y=NULL)+
#   theme(
#     text=element_text(size=58/.pt,family="apercu"),
#     legend.text=element_text(size=18),
#     legend.position="none",
#     #axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     legend.key=element_blank(),
#     legend.title=element_blank(),
#     panel.background = element_rect(fill = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(colour = "#D5E7F8"),
#     panel.ontop = FALSE)+
#   coord_flip()
# testplot2

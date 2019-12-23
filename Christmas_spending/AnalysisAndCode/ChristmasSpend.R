library(readxl)
library(tidyverse)
library(ggrepel)

christmasspending<- readxl::read_xlsx("./Christmas_spending/Data/Christmas spending.xlsx")
christmasspending<- christmasspending %>% rename(SpendingAmount=`Amount in £`)
cmasNonTotal <- christmasspending %>% filter(Category != "Total")
## find any missing values
myplot<-ggplot(data = cmasNonTotal, 
               mapping = aes(x=Year,y=SpendingAmount,group=Category)) +
  geom_line(aes(colour=Category,alpha=.5),size=2) +
  geom_point(aes(colour=Category,alpha=.5),size=4) +
  geom_text_repel(data = cmasNonTotal%>%filter(Year=="2018")
                  ,color="white"
                  ,nudge_x = -.45
                  ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 size = 1)) +
  geom_text_repel(data = cmasNonTotal%>%filter(Year=="2019")
                  ,color='white'
                  ,nudge_x = .25
                  ,mapping = aes(label=paste0( "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 size = 1 
                                 
                  ))+
  
  
  scale_x_discrete(position = "top",limit=c("2018","2019")) +
  
  theme_bw() +
  
  theme(legend.position = "none") +
  theme(panel.border = element_blank()) +
  
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  facet_wrap(.~Region) +
  theme(strip.background = element_rect(
    color="white", fill="white"
  )) +
  
  theme(strip.text = element_text(face = "bold")) +
  
  #   #titles
  labs( title = "Comparision of Christmas spending between 2018 and 2019 across UK and Europe")

grb<-ggplotGrob(myplot)

grb$grobs[[2]]$children[[1]]$children[[1]]$gp$fill<-"#003399"
grb$grobs[[3]]$children[[1]]$children[[1]]$gp$fill<-"#00247D"
# # texts to match color
grb$grobs[[12]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col<-"#003399"
grb$grobs[[13]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col<-"#00247D"
plot(grb)

spendanalysis<-cmasNonTotal %>% pivot_wider(names_from = "Year",values_from = "SpendingAmount")
#spendanalysis<-spendanalysis %>% mutate(PercentChange= ((`2019`-`2018`)/`2018`)*100)
spendanalysis<-spendanalysis %>% 
  mutate(PercentChange= round(((`2019`-`2018`)/`2018`)*100,4)) %>%
  group_by(Region) %>%
  mutate(IsMax= if_else(PercentChange==max(PercentChange),1,0))
spendanalysis
                          
#round(((`2019`-`2018`)/`2018`)*100,4)
# spendanalysis<-spendanalysis %>% group_by(Region) %>% 
#   mutate("Total_2018"=sum(`2018`),"Total_2019"=sum(`2019`)) %>%
#   mutate("PropOfTotal2018"= (`2018`/Total_2018)*100,"PropOfTotal2019"= (`2019`/Total_2019)*100 )
  

ggplot(data = spendanalysis ,aes(x=Category,y=PercentChange,fill=factor(IsMax))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("1"="orange","0"="gray")) +
  geom_text(mapping = aes(label=paste0(round(PercentChange,2),"%")),nudge_y = -1) +  
  facet_wrap(~Region,ncol = 1) +
  theme(legend.position = "none") +
  theme_bw() +
  theme(legend.position = "none") +
  
  theme(panel.border = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(axis.text.x      = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  
    coord_flip()
  
library(readxl)
library(tidyverse)
library(ggrepel)
christmasspending<- readxl::read_xlsx("./Christmas_spending/Data/Christmas spending.xlsx")
christmasspending<- christmasspending %>% rename(SpendingAmount=`Amount in £`)

## find any missing values
missingvalues<-apply(christmasspending,2,function(x){sum(is.na(x))})
## no missing values.
##
christmasspending%>% group_by(Region)%>% distinct(Category)
## Let us focus on how the spending has changed by region and by category
# Splitting UK and Europe so that we can compare and also remove totals.
christmasspending$Year <- as.factor(christmasspending$Year)
cmasNonTotal <- christmasspending %>% filter(Category != "Total")
cmasUK <- christmasspending %>% filter((Region == "UK") & (Category != "Total"))
cmaEur <-christmasspending %>% filter((Region == "Europe") & (Category != "Total"))

## plot UK
ggplot(data = cmasUK, mapping = aes(x=Year,y=SpendingAmount,group=Category)) +
  geom_line(aes(colour=Category,alpha=1),size=1) +
  geom_point(aes(colour=Category,alpha=1),size=4) +
  geom_text_repel(data = cmasUK%>%filter(Year=="2018")
            ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                           hjust = "left", 
                           #fontface = "italics", 
                           size = 1, 
                           nudge_x = -.45, 
                           direction = "y")) +
  geom_text_repel(data = cmasUK%>%filter(Year=="2019")
            ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                           hjust = "left", 
                           #fontface = "bold", 
                           size = 1, 
                           nudge_x = -.45, 
                           direction = "y"))+
  
  
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


  labs(title = "Christmas spending in the UK")







## plot Europe
ggplot(data = cmaEur, mapping = aes(x=Year,y=SpendingAmount,group=Category)) +
  geom_line(aes(colour=Category,alpha=1),size=1) +
  geom_point(aes(colour=Category,alpha=1),size=4) +
  geom_text_repel(data = cmaEur%>%filter(Year=="2018")
                  ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 #fontface = "italics", 
                                 size = 1, 
                                 nudge_x = -.45, 
                                 direction = "y")) +
  geom_text_repel(data = cmaEur%>%filter(Year=="2019")
                  ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 #fontface = "bold", 
                                 size = 1, 
                                 nudge_x = -.45, 
                                 direction = "y"))+
  
  
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
  
  
  labs(title = "Christmas spending in Europe")


cmasNonTotal %>% pivot_wider(names_from = "Year",values_from = "SpendingAmount")

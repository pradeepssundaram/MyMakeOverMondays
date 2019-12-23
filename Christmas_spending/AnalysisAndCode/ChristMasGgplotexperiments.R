myplot<-ggplot(data = cmasNonTotal, 
       mapping = aes(x=Year,y=SpendingAmount,group=Category)) +
  geom_line(aes(colour=Category,alpha=.5),size=2) +
  geom_point(aes(colour=Category,alpha=.5),size=4) +
  scale_fill_hue(h = 360,l = 90,c=10) +
  #scale_fill_brewer(palette = "Dark2")+
  geom_text_repel(data = cmasNonTotal%>%filter(Year=="2018")
                  ,color="white"
                  ,nudge_x = -.45
                  ,mapping = aes(label=paste0(Category," ", "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 
                                 #fontface = "italics", 
                                 size = 1, 
                                 nudge_x = -.45, 
                                 direction = "y")) +
  geom_text_repel(data = cmasNonTotal%>%filter(Year=="2019")
                  ,color='white'
                  ,nudge_x = .25
                  ,mapping = aes(label=paste0( "£",as.character(SpendingAmount)),
                                 hjust = "left", 
                                 #fontface = "bold", 
                                 size = 1, 
                                 #nudge_x = 1, 
                                 #direction = "y"
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
  #geom_rect(aes(fill = Region),xmin = -Inf,xmax = Inf,
  #          ymin = -Inf,ymax = Inf,alpha = 0.3)+
  #scale_fill_manual(c("blue","green","red","yellow"))+
  facet_wrap(.~Region) +
 
  
  # playing around with colors of facets
  # theme(strip.text.x = element_text(size = 12, color = "blue", face = "bold.italic")) +
  # #theme(strip.text.x = element_text(size = 12, color = "blue", face = "bold.italic")) +
  # 
  theme(strip.background = element_rect(
    color="white", fill="white"
  )) +
  
  theme(strip.text = element_text(face = "bold")) +
  
  #theme(panel.background =  element_rect(fill = "grey")) +
  #   #titles
  labs( title = "Comparision of Christmas spending between 2018 and 2019 across UK and Europe")


# 
 grb<-ggplotGrob(myplot)



grb$grobs[[2]]$children[[1]]$children[[1]]$gp$fill<-"#808080"
grb$grobs[[3]]$children[[1]]$children[[1]]$gp$fill<-"#C0C0C0"
grb$grobs[[12]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col<-"#808080"
grb$grobs[[13]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col<-"#C0C0C0"

#808080","#C0C0C0"

plot(grb)
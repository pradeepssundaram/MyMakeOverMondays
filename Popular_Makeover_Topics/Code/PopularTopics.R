library(tidyverse)

poptops<-read_csv(file = "./Popular_Makeover_Topics/data/downloads_by_dataset.csv")
# 
# poptops %>% 
#   group_by(year,subject) %>% 
#   summarise(totaldownloads=sum(downloads)) %>% 
#   arrange(year,desc(totaldownloads),subject)

#most popular weeks 
View(
  poptops %>% 
       group_by(year,week) %>% 
       summarise(totaldownloads=sum(downloads)) %>% 
        arrange(year,desc(totaldownloads)) %>% 
    top_n(5) %>% inner_join(y = (poptops %>% 
                                   mutate(mnth=lubridate::month(date,label=TRUE,abbr=FALSE)) %>% 
                                   select(year,week,mnth)
    ) ,by = c("year","week"))
)
  

  
  poptops %>% 
    mutate(mnth=lubridate::month(date,label=TRUE,abbr=FALSE)) %>% 
    select(year,week,mnth)


    poptops %>% 
      inner_join(y=poptops %>% 
                   mutate(mnth=lubridate::month(date)) %>% 
                   select(year,week,mnth)
      ) %>%
      group_by(year,mnth) %>% 
      summarise(totaldownloads=sum(downloads)) %>% 
      arrange(year,desc(totaldownloads)) %>% 
    top_n(5) %>% 
    mutate(bb=month.name[mnth])
  
    
  poptopsnew <- poptops %>% select(date)
    
  
  poptops %>% 
    mutate(mnth=lubridate::month(date)) %>% 
    filter(year %in% c(2018,2019)) %>% 
    filter(mnth%in% c(9,10,11,12)) %>% 
    select(subject,downloads)
  
  
  poptops %>% mutate(mnth=lubridate::month(date)) %>% group_by(year,mnth) %>% summarise(totaldownloads=sum(downloads))
  
  ggplot(poptops %>% 
           mutate(mnth=lubridate::month(date)) %>% 
           group_by(year,mnth) %>% 
           summarise(totaldownloads=sum(downloads)) %>% filter(year %in% c(2018,2019)))  +
    #geom_line(mapping = aes(x = as.factor(mnth),y = totaldownloads, group=year,color=as.factor(year))) +
    geom_bar(mapping = aes(x=as.factor(mnth),y=totaldownloads,fill=as.factor(year)),stat = "identity") 
    

  
  popnew<-poptops %>% 
    filter(year %in% c(2018,2019)) %>% 
    mutate(monthn= lubridate::month(date)) %>% 
    group_by(year,monthn) %>% 
    summarise(totdown=sum(downloads)) %>% 
    pivot_wider(id_cols = monthn,names_from = year,values_from = totdown)
  
  
  
  
  ggplot(data = popnew,mapping = aes(x=as.factor(monthn),y=`2018`))  +
    #geom_line(mapping = aes(x = as.factor(mnth),y = totaldownloads, group=year,color=as.factor(year))) +
    geom_bar(stat="identity", width = 0.3,  fill="orange") +
    geom_bar(mapping = aes(x=as.factor(monthn),y=`2019`),stat = "identity", width = 0.1,color="green",fill="green") +
    coord_flip()
  
  
  
  
  
  poptops %>% filter(year %in% c(2018,2019)) %>% 
    mutate(monthn=lubridate::month(date)) %>% 
    select(year,monthn,subject,downloads) %>% 
    arrange(year,monthn,desc(downloads)) %>% 
    group_by(year,monthn)
  
  
  popbymonth<-top_n(x= poptops %>% filter(year %in% c(2018,2019)) %>% 
          mutate(monthn=lubridate::month(date)) %>% 
          select(year,monthn,subject,downloads) %>% 
          arrange(year,monthn,desc(downloads)) %>% 
          group_by(year,monthn) ,n = 1,wt = downloads)
    
  
  popbymonth %>% 
    pivot_wider(c(subject,monthn),year,values_from = subject)
  
  poptops %>% 
    filter(year %in% c(2018,2019)) %>% 
    pivot_wider(id_cols = c(subject,downloads),names_from = "year",values_from = downloads)
  
  
  poptops %>% tidytext::unnest_tokens(input = subject )
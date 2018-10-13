# make a map of Karelia and plot where evacuess were at different times - ?based on education or occupation
library(ggplot2)
library(ggmap)
library(dplyr)
locate <- readRDS("locate.rds")
# filter data for 1937, 1943 and 1947
locate_1937<- locate %>% filter (years==1930 & lat.x<62.2 & lon.x !=27.52 & lon.x!=28.01) %>% as.data.frame()
locate_1943<- locate %>% filter (years==1943 ) %>% as.data.frame()
locate_1945<- locate %>% filter (years==1945 & location2 !=301 ) %>% as.data.frame()
al1 = get_map(location = c(lon = 28 , lat = 61), zoom = 5, maptype = "terrain")
p37<- ggmap(al1)+geom_count(data=locate_1937, 
                          aes(x=lon.x, y=lat.x), col="darkred",fill="darkred",alpha=0.7)+ #,cex=input$opt.cex)+
  
  #scale_shape_identity() + 
  
  #geom_count(aes(size = ..prop.., group = 1))
  #guides(names(legend_choices[legend_choices == input$cat]))))+
  scale_x_continuous(name="", limits=c(18,35))+
  scale_y_continuous(name="Latitude", limits=c(58,66)) +
  #scale_fill_manual("Number of evacuees")
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"), 
        #legend.position = "none",
        legend.key.size=unit(0.5,"cm"),
        legend.text=element_text(size=rel(0.7)),
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))+
  guides(colour = guide_legend(override.aes = list(size=0.5, alpha = 1)))
  guides(col = guide_legend(override.aes = list(size=1)))
p37


library(ggpubr)
Fig_1<- ggarrange(p37, p43, p45,ncol=3,nrow=1,vjust=11.5,hjust=-2.0,common.legend = TRUE, legend="right",labels=c("1938", "1943", "1945"))
         
Fig_1



#  Posterior predictive check figure
Fig_1_final <-annotate_figure(Fig_1,
                            top = text_grob("Evacuee locations before during and after the war", color = "black",
                                            face = "bold", size = 14),
                            fig.lab = "Figure 1", fig.lab.face = "bold")

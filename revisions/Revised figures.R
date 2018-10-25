#### Figure 1 maps
# make a map of Karelia and plot where evacuess were at different times - ?based on education or occupation
# make a map of Karelia and plot where evacuess were at different times - ?based on education or occupation
library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyverse)

locate <- readRDS("C:/Users/robert/Dropbox/Github/data files/locate.rds")
# filter data for 1937, 1943 and 1947
locate_1937<- locate %>% filter (years==1930 & lat.x<62.2 & lon.x !=27.52 & lon.x!=28.01) %>% as.data.frame()
locate_1943<- locate %>% filter (years==1943 ) %>% as.data.frame()
locate_1945<- locate %>% filter (years==1945 & location2 !=301 ) %>% as.data.frame()


# this is the patch - make sure this installs properly!!!
devtools::install_github("dkahle/ggmap", ref = "tidyup")

library(ggmap)
# run this line to authienticate google srevices with my api code from google cloud platform - this
# is where I find my api key
register_google(key = "AIzaSyDHZK48zah4BizELuH_08kynwoNHHzk2Jc")


al1 = get_map(location = c(lon = 28 , lat = 61), zoom = 5, scale = 2, maptype = "terrain", source="google")


p37<- ggmap(al1)+geom_count(data=locate_1937, 
                            aes(x=lon.x, y=lat.x), col="darkred",fill="darkred",alpha=0.7)+ 
  
  
  scale_x_continuous(name="", limits=c(18,35))+
  scale_y_continuous(name="Latitude", limits=c(58,66)) +
  #scale_fill_manual("Number of evacuees")
  theme(axis.text.x = element_text(colour="black",size=5,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=5,angle=0,hjust=0,vjust=0,face="plain"), 
        #legend.position = "none",
        legend.key.size=unit(0.5,"cm"),
        legend.text=element_text(size=rel(0.7)),
        axis.title.x = element_text(colour="grey20",size=6,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides(colour = guide_legend(override.aes = list(size=0.5, alpha = 1)))
guides(col = guide_legend(override.aes = list(size=1)))
p37

p43<- ggmap(al1)+geom_count(data=locate_1943, 
                            aes(x=lon.x, y=lat.x), col="darkred",fill="darkred",alpha=0.7)+ #,cex=input$opt.cex)+
  
  
  scale_x_continuous(name="Longitude", limits=c(18,35))+
  scale_y_continuous(name="", limits=c(58,66)) +
  #scale_fill_manual("Number of evacuees")
  theme(axis.text.x = element_text(colour="black",size=5,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=5,angle=0,hjust=0,vjust=0,face="plain"), 
        #legend.position = "none",
        legend.key.size=unit(0.5,"cm"),
        legend.text=element_text(size=rel(0.7)),
        axis.title.x = element_text(colour="grey20",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
  guides(colour = guide_legend(override.aes = list(size=0.5, alpha = 1)))
guides(col = guide_legend(override.aes = list(size=1)))
p43


p45<- ggmap(al1)+geom_count(data=locate_1945, 
                            aes(x=lon.x, y=lat.x), col="darkred",fill="darkred",alpha=0.7)+ #,cex=input$opt.cex)+
  
  #scale_shape_identity() + 
  
  #geom_count(aes(size = ..prop.., group = 1))
  #guides(names(legend_choices[legend_choices == input$cat]))))+
  scale_x_continuous(name="", limits=c(18,35))+
  scale_y_continuous(name="", limits=c(58,66)) +
  #scale_fill_manual("Number of evacuees")
  theme(axis.text.x = element_text(colour="black",size=5,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=5,angle=0,hjust=0,vjust=0,face="plain"), 
        #legend.position = "none",
        legend.key.size=unit(0.5,"cm"),
        legend.text=element_text(size=rel(0.7)),
        axis.title.x = element_text(colour="grey20",size=6,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
  guides(colour = guide_legend(override.aes = list(size=0.5, alpha = 1)))
guides(col = guide_legend(override.aes = list(size=1)))
p45
library(ggpubr)
Fig_1<- ggarrange(p37, p43, p45,ncol=3,nrow=1,vjust=1,hjust=-4,common.legend = TRUE, 
                  font.label = list(size = 7, color = "black", face = "bold", family = NULL),
                  legend="right",labels=c("1938", "1943", "1945"))

Fig_1



#  Posterior predictive check figure
Fig_1_final <-annotate_figure(Fig_1,top = text_grob("Evacuee locations before during and after the war", 
                                                    color = "black",face = "bold", size = 9,vjust=0),
                              fig.lab = "")


Fig_1_final

ggsave(Fig_1_final, filename = "Figure 1a-c.png", width = 6, height = 2, device = "png", dpi = 600,units = "in")

#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)
library(colortools)
person_data <- readRDS("C:/Users/rofrly/Dropbox/Github/data files/person_data.rds")
m<- person_data
m <- m %>% filter(birthregion == "karelia" & birthyear<1926 & birthyear>1870)
m$hypergamy <- m$social_class-m$social_class_spouse
m$hypergamy <- ifelse(m$hypergamy<0,-1, ifelse(m$hypergamy>0, 1,0))

m$married_after <- ifelse(m$weddingyear<1945 | is.na(m$weddingyear), 0, 1)
m$log_pop <- log(m$birthpopulation)
m$log_pop <- m$log_pop-min(m$log_pop, na.rm=TRUE)
m$log_pop <- m$log_pop / max(m$log_pop, na.rm=TRUE)
m$fdf_log_pop <- log(m$fdf_population)
m$fdf_log_pop <- m$fdf_log_pop-min(m$fdf_log_pop, na.rm=TRUE)
m$fdf_log_pop <- m$fdf_log_pop/max(m$fdf_log_pop, na.rm=TRUE)
m$age <- 1944- m$birthyear
m$age_1940 <- 1940-m$birthyear
m$age <- m$age - min (m$age)
m$age <- m$age/ max(m$age)
m$census_1950 <- as.factor(m$'1950_census')
m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)




m<- m %>% dplyr::select(kids,hypergamy,outbred,returnedkarelia,sex, age,log_pop, education, agricult, technical,factory,service,
                        office,business,transport, birthplaceid,married_after,age_1940, birthyear)
m <- m[complete.cases(m),] # N=26,757

# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

remained_after <- after %>% filter(returnedkarelia==0) 
returned_after <- after %>% filter(returnedkarelia==1) 
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)


# load models
#model_before <- readRDS("C:/Users/rofrly/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
#model_after <- readRDS("C:/Users/rofrly/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")

outbred_all <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_30_outbred_all_w_all_interactions.rds")

Model_kids_all <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_kids_all_FULL_INTS.rds")




####################################################################
####################################################################
####################################################################
####################################################################
### Make the posterior predictive check figures for models 1 and 2
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)

# Load old data
## read in df that matches models  KEY PART
person_data_old <- readRDS("C:/Users/rofrly/Dropbox/Github/data files/person_data_old.rds")


m <- person_data_old
m <- m %>% filter(birthregion == "karelia" & birthyear<1926 & birthyear>1870)
m$hypergamy <- m$social_class-m$social_class_spouse
m$hypergamy <- ifelse(m$hypergamy<0,-1, ifelse(m$hypergamy>0, 1,0))

m$married_after <- ifelse(m$weddingyear<1945 | is.na(m$weddingyear), 0, 1)
m$log_pop <- log(m$birthpopulation)
m$log_pop <- m$log_pop-min(m$log_pop, na.rm=TRUE)
m$log_pop <- m$log_pop / max(m$log_pop, na.rm=TRUE)
m$fdf_log_pop <- log(m$fdf_population)
m$fdf_log_pop <- m$fdf_log_pop-min(m$fdf_log_pop, na.rm=TRUE)
m$fdf_log_pop <- m$fdf_log_pop/max(m$fdf_log_pop, na.rm=TRUE)
m$age <- 1944- m$birthyear
m$age <- m$age - min (m$age)
m$age <- m$age/ max(m$age)

m$census_1950 <- as.factor(m$'1950_census')
m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)

m<- m %>% dplyr::select(kids,hypergamy,outbred,returnedkarelia,sex, age,log_pop, education, agricult, technical,factory,service,
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,669

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

returned_after <- m %>% filter (married_after==1 & returnedkarelia==1)
remained_after <- m %>% filter (married_after==1 & returnedkarelia==0)

returned_before <- m %>% filter (married_after==0 & returnedkarelia==1)
remained_before <- m %>% filter (married_after==0 & returnedkarelia==0)
# put birthplace ids in format for STAN
m <- m %>% arrange(birthplaceid)
m$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(m$birthplaceid)) != 0))

returned <- returned %>% arrange(birthplaceid)
returned$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(returned$birthplaceid)) != 0))

remained <- remained %>% arrange(birthplaceid)
remained$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(remained$birthplaceid)) != 0))

before <- before %>% arrange(birthplaceid)
before$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(before$birthplaceid)) != 0))

after <- after %>% arrange(birthplaceid)
after$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(after$birthplaceid)) != 0))

returned_after <- returned_after %>% arrange(birthplaceid)
returned_after$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(returned_after$birthplaceid)) != 0))

remained_after <- remained_after %>% arrange(birthplaceid)
remained_after$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(remained_after$birthplaceid)) != 0))

returned_before <- returned_before %>% arrange(birthplaceid)
returned_before$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(returned_before$birthplaceid)) != 0))

remained_before <- remained_before %>% arrange(birthplaceid)
remained_before$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(remained_before$birthplaceid)) != 0))

# use dataframe m (26,757 cases) for dataframe that matches the models
# My pi values
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
#load models 
#Use models 30 and 31
model_outbred <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_30_outbred_all_w_all_interactions.rds")

model_kids <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_31_kids_all_w_all_interactions.rds")

model_kids_after <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_61_kids_after_w_intxs.rds")
model_kids_before <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_60_kids_before_w_intxs.rds")
model_outbred_before <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_70_outbred_before_w_intxs.rds")
model_outbred_after <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_38_outbred_after_w_all_interactions.rds")


sims_out<- sim(model_outbred)
sims_kids<- sim(model_kids)
sims_out_before<- sim(model_outbred_before)
sims_kids_before<- sim(model_kids_before)
sims_out_after<- sim(model_outbred_after)
sims_kids_after<- sim(model_kids_after)

mean_sims_ob <- apply(sims_out, 2, mean)
mean_sims_kids <- apply(sims_kids, 2, mean)
mean_sims_out_before <- apply(sims_out_before, 2, mean)
mean_sims_kids_before <- apply(sims_kids_before, 2, mean)
mean_sims_out_after <- apply(sims_out_after, 2, mean)
mean_sims_kids_after <- apply(sims_kids_after, 2, mean)




# make vectors for outcome variables from raw data
ob <- m$outbred
kids <- m$kids
ob_before <- before$outbred
ob_after <- after$outbred
kids_before <- before$kids
kids_after <- after$kids

# get number of times PPC accurately predicts the observed data
#Figure S3a
df3a <- cbind(sims_out[1,],ob)  %>% as.data.frame()
df3a$sum <- df3a$V1+df3a$ob
plyr::count(df3a,'sum')

#Figure S3b
df3b <- cbind(sims_out_before[1,],ob_before)  %>% as.data.frame()
df3b$sum <- df3b$V1+df3b$ob
plyr::count(df3b,'sum')

#Figure S3c
df3c <- cbind(sims_out_after[1,],ob_after)  %>% as.data.frame()
df3c$sum <- df3c$V1+df3c$ob
plyr::count(df3c,'sum')


# figure S4a
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
pi_kids <- apply(sims_kids, 2, my_PI) %>% as.data.frame()
pi_kids <- t(pi_kids)

pi_kids <- cbind(kids,pi_kids) %>% as.data.frame()
colnames(pi_kids) <- c("obs","low","high")
pi_kids$count <- ifelse(pi_kids$obs>=pi_kids$low & pi_kids$obs<=pi_kids$high,0,1)
sum(pi_kids$count)

# figure S4b
pi_kids_before <- apply(sims_kids_before, 2, my_PI) %>% as.data.frame()
pi_kids_before <- t(pi_kids_before)

pi_kids_before <- cbind(kids_before,pi_kids_before) %>% as.data.frame()
colnames(pi_kids_before) <- c("obs","low","high")
pi_kids_before$count <- ifelse(pi_kids_before$obs>=pi_kids_before$low & pi_kids_before$obs<=pi_kids_before$high,0,1)
sum(pi_kids_before$count)

# figure S4c
pi_kids_after <- apply(sims_kids_after, 2, my_PI) %>% as.data.frame()
pi_kids_after <- t(pi_kids_after)

pi_kids_after <- cbind(kids_after,pi_kids_after) %>% as.data.frame()
colnames(pi_kids_after) <- c("obs","low","high")
pi_kids_after$count <- ifelse(pi_kids_after$obs>=pi_kids_after$low & pi_kids_after$obs<=pi_kids_after$high,0,1)
sum(pi_kids_after$count)


# join predictions and raw data into df's
df1<- cbind(mean_sims_ob,ob) %>% as.data.frame()
df2<- cbind(mean_sims_kids,kids) %>% as.data.frame()
df3 <- cbind(mean_sims_out_before,ob_before) %>% as.data.frame()
df4 <- cbind(mean_sims_kids_before,kids_before) %>% as.data.frame()
df5 <- cbind(mean_sims_out_after,ob_after) %>% as.data.frame()
df6 <- cbind(mean_sims_kids_after,kids_after) %>% as.data.frame()

big_data <- as.data.frame(mapply(c,df1,df2,df3,df4,df5,df6))
big_data$condition <- c(rep(1,26669),rep(2,26669),
                        rep(3,21105),rep(4,21105),
                        rep(5,5564),rep(6,5564))
names(big_data)<- c("predicted","observed","model")


big_data1 <- big_data %>% filter (model==1)
big_data2 <- big_data %>% filter (model==2)
big_data3 <- big_data %>% filter (model==3)
big_data4 <- big_data %>% filter (model==4)
big_data5 <- big_data %>% filter (model==5)
big_data6 <- big_data %>% filter (model==6)

# make standard error formula
library(plotrix)

model_1 <- big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=c(-0.1,1.1), linetype="dotted") +
  geom_vline(xintercept=mean(big_data1$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*15), xend=mean(observed)+(std.error(observed)*15),y=1,yend=1,
  linetype='Observed data\n 99% CI'), 
  position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_discrete(name="",limits=c(-0.05,mean(big_data1$observed),1.1), labels=c("Married a\nKarelian","Mean\nobservation",
                                                                  "Married a\nresident Finn"))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("Factors affecting\n the probability\nof intermarriage"))+ 
  ggtitle("All marriages")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_1

#ggsave(model_1, filename = "Figure S3.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")
model_2 <- big_data2 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted,y=1), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=mean(big_data2$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*10), xend=mean(observed)+(std.error(observed)*10),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  #coord_fixed(ratio=3)+
  scale_x_continuous(name="Number of children",limits=c(0,7))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("Factors affecting\nreproductive outcomes"))+ 
  #coord_cartesian(ylim=c(0.99,1.01))+
  coord_cartesian(ylim = c(0.99, 1.01)) +
  ggtitle("All marriages")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  
#plot.margin=grid::unit(c(0,0,0,0), "mm"))
model_2 
#ggsave(model_2, filename = "Figure S4.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")
model_3 <- big_data3 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted,y=1), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=c(0.1,1.1), linetype="dotted") +
  geom_vline(xintercept=mean(big_data3$observed), linetype="F1") +
  geom_segment(aes(x=mean(observed)-(std.error(observed)*15), xend=mean(observed)+(std.error(observed)*15),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_discrete(name="",limits=c(-0.05,mean(big_data3$observed),1.1), 
                   labels=c("Married a\nKarelian","Mean\nobservation","Married a\nresident Finn"))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c(""))+ 
  coord_cartesian(ylim = c(0.99, 1.01)) +
  ggtitle("Marriages before the war")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_3
#ggsave(model_3, filename = "Figure S5.tiff", width = 9, height = 7, device = "tiff", dpi = 600,units = "in")
model_4 <- big_data4 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted,y=1), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=mean(big_data4$observed), linetype="F1") +
  
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*10), xend=mean(observed)+(std.error(observed)*10),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  #coord_fixed(ratio=3)+
  scale_x_continuous(name="Number of children",limits=c(0,7))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c(""))+ 
  #coord_cartesian(ylim=c(0.99,1.01))+
  coord_cartesian(ylim = c(0.99, 1.01)) +
  ggtitle("Marriages before the war")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  
#plot.margin=grid::unit(c(0,0,0,0), "mm"))
model_4
#ggsave(model_4, filename = "Figure S6.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")
model_5 <- big_data5 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted,y=1), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=c(-0.05,1.1), linetype="dotted") +
  geom_vline(xintercept=mean(big_data5$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*15), xend=mean(observed)+(std.error(observed)*15),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_discrete(name="",limits=c(-0.05,mean(big_data5$observed),1.1), 
                   labels=c("Married a\nKarelian","Mean\nobservation","Married a\nresident Finn"))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c(""))+ 
  coord_cartesian(ylim = c(0.99, 1.01)) +
  ggtitle("Marriages after the war")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_5
#ggsave(model_5, filename = "Figure S7.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")
model_6 <- big_data6 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted,y=1), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  geom_vline(xintercept=mean(big_data6$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*10), xend=mean(observed)+(std.error(observed)*10),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  #coord_fixed(ratio=3)+
  scale_x_continuous(name="Number of children",limits=c(0,7))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c(""))+ 
  #coord_cartesian(ylim=c(0.99,1.01))+
  coord_cartesian(ylim = c(0.99, 1.01)) +
  ggtitle("Marriages after the war")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  

model_6
### PPC plots

######### panel plots for supp figure S3a-c and S4a-c
# make panel plots for reproduction before and after war fig 2a 
panel_plot_s3 <- ggarrange(model_1,model_3,model_5, labels=c("", 
                                                             ""),
                           vjust=2.5, hjust= -2,ncol=3, nrow=1, common.legend=TRUE)
figureS3 <- annotate_figure(panel_plot_s3,
                            top = text_grob("Posterior predictive checks for models Factors affecting Reproductive outcomes", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
ggsave(figureS3, filename = "Figure S3a-c.png", width = 12, height = 4, device = "png", dpi = 600,units = "in")


panel_plot_s4 <- ggarrange(model_2,model_4,model_6, labels=c("", 
                                                             ""),
                           vjust=2.5, hjust= -2,ncol=3, nrow=1, common.legend=TRUE)
figureS4 <- annotate_figure(panel_plot_s4,
                            top = text_grob("Posterior predictive checks for models Factors affecting Reproductive outcomes", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
ggsave(figureS4, filename = "Figure S4a-c.png", width = 16, height = 4, device = "png", dpi = 600,units = "in")



library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)


#model_outbred <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_30_outbred_all_w_all_interactions.rds")
#model_kids <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_31_kids_all_w_all_interactions.rds")

#model_kids_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")
#model_kids_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
#model_outbred_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
#model_outbred_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")



# read in kids before and after with all intxs for figure S2
# 
# ##########################################################
kids_before_all_intxs <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_kids_before_FULL_INTS.rds")
kids_after_all_intxs <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_kids_after_FULL_INTS.rds")




post_model1 <- extract.samples(model_outbred) %>% as.data.frame()
post_model1 <- post_model1 %>% select (1:19)
post_model1 <- post_model1 [c(3,4,1,2,6,7,8,11,14,15,16,17,18,19)]
post_model1$bage <- post_model1$bage/54
post_model1$brkage <- post_model1$brkage/54
#rescale age variable
post_model1$bage <- post_model1$bage/54
post_model1$brkage <- post_model1$brkage/54
names(post_model1) <- c(labels=c("Male","Age","Hypergamy","Returned to Karelia","Educated",
                                 "Agriculture","Technical professions","Office workers","Married after war",
                                 "Returned to Karelia X\nMale","Returned to Karelia X\nAge",
                                "Married after war X\nReturned to Karelia","Married after war X\nMale",
                                "Married after war X\nHypergamy"))


# read in full kids model with all intxs for figure 2b
Kids_all_all_intxs <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_kids_all_FULL_INTS.rds")

post_model2 <- extract.samples(Kids_all_all_intxs) %>% as.data.frame()
post_model2 <- post_model2 %>% select (1:23)
post_model2 <- post_model2 [c(17,1,8,2,5,14,15,19,20,21,22,9,11,12)]
post_model2$bage <- post_model2$bage/54
post_model2$brkage <- post_model2$brkage/54
names(post_model2) <- c(labels=c("Age","Hypergamy","Intermarriage","Returned to Karelia","Agriculture",
                                 "Office workers","Business workers","Married after war",
                                 "Returned to Karelia X\nAge","Married after war X\nReturned to Karelia",
                                 "Married after war X\nMale","Intermarriage X\nReturned to Karelia","Intermarriage X\nHypergamy",
                                 "Intermarriage X\nMarried after war"))

### before and after the war (table S2 models)
post_model3 <- extract.samples(model_outbred_before) %>% as.data.frame()
post_model3 <- post_model3 %>% select (1:15)
post_model3 <- post_model3 [c(3,4,11,8,6,1,2,14,15)]
#post_model3 <- post_model3 [c(15,14,1,6,8,11,4,3)]
post_model3$bage <- post_model3$bage/54
post_model3$brkage <- post_model3$brkage/54
names(post_model3) <- c(labels=c("Male","Age","Office workers","Technical professions",
                                 "Educated","Hypergamy","Returned to Karelia","Returned to Karelia\nX Age",
                                 "Returned to Karelia X\nMale"))

### before and after the war (table S2 models)
post_model4 <- extract.samples(kids_before_all_intxs) %>% as.data.frame()
post_model4 <- post_model4 %>% select (1:19)
post_model4 <- post_model4 [c(1,2,5,13,14,18,9)]
names(post_model4) <- c(labels=c("Hypergamy","Intermarriage","Agriculture","Office workers","Business workers",
                                 "Returned to Karelia X\nAge", "Intermarriage X\nReturned to Karelia"))

### before and after the war (table S2 models)
post_model5 <- extract.samples(model_outbred_after) %>% as.data.frame()
post_model5 <- post_model5 %>% select (1:15)
post_model5 <- post_model5 [c(3,4,1,10,11,14)]
post_model5$bage <- post_model5$bage/54
names(post_model5) <- c(labels=c("Male","Age","Hypergamy","Service industry", "Office workers",
                                 "Returned to Karelia X\nMale"))

### before and after the war (table S2 models)
post_model6 <- extract.samples(kids_after_all_intxs) %>% as.data.frame()
post_model6 <- post_model6 %>% select (1:18)
post_model6 <- post_model6 [c(16,1,8,5,18,11)]
post_model6$bage <- post_model6$bage/54
post_model6$brkage <- post_model6$brkage/54
names(post_model6) <- c(labels=c("Age","Hypergamy", "Intermarriage","Agriculture","Returned to Karelia X\nAge",
                                 "Intermarriage X\nHypergamy"))

color_scheme_set("green")
p1 <- mcmc_intervals(post_model1, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_1<- p1 + 
  scale_x_continuous(name="Odds ratio",limits=c(-1.25,2.0), labels=c("0.3","0.5","1","1.5","2","3","6"),
                     breaks=c(-1.2,-0.69,0,0.41, 0.7,1.1,1.8)) +
  ggplot2::labs(
    title = "Factors affecting the probability\n of intermarriage"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        plot.title=element_text(face ="bold", size=12,hjust=0.4),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
#ggsave(plot_1, filename = "Figure S1.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

### This is figure 2b
color_scheme_set("green")
p2 <- mcmc_intervals(post_model2, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_2<- p2 + 
  scale_x_continuous(name="Odds ratio",limits=c(-0.45,0.45), labels=c("0.75","1","1.5"),
                     breaks=c(-0.30,0,0.41)) +
  ggplot2::labs(
    title = "Factors affecting\nreproductive outcomes"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        plot.title=element_text(face ="bold", size=12,hjust=0.4),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_2
ggsave(plot_2, filename = "Figure 2b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")



# main figure panel plots
color_scheme_set("purple")
p3 <- mcmc_intervals(post_model3, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_3<- p3 + 
scale_x_continuous(name="Odds ratio",limits=c(-0.75,1.0), labels=c("0.5","1","1.5","2"),
                     breaks=c(-0.69,0,0.41, 0.7)) +
  ggplot2::labs(
    title = "Before the war"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_3

#ggsave(plot_3, filename = "Figure 2a.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

color_scheme_set("blue")
p4 <- mcmc_intervals(post_model4, prob = 0.5, prob_outer = 0.9,
point_est = c("median"), rhat = numeric())
plot_4<- p4 + 
  scale_x_continuous(name="Odds ratio",limits=c(-0.4,0.5), labels=c("0.7","1","1.5"),
                     breaks=c(-0.35,0,0.41)) +
  ggplot2::labs(
    title = "Before the war"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_4

#ggsave(plot_4, filename = "Figure 2b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

color_scheme_set("purple")
p5 <- mcmc_intervals(post_model5, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_5<- p5 + 
  scale_x_continuous(name="Odds ratio",limits=c(-1.1,1.1), labels=c("0.5","1","1.5","2"),
                     breaks=c(-0.69,0,0.41, 0.7)) +
  ggplot2::labs(
    title = "After the war"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_5

#ggsave(plot_5, filename = "Figure 2c.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


color_scheme_set("blue")
p6 <- mcmc_intervals(post_model6, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_6<- p6 + 
  
  scale_x_continuous(name="Odds ratio",limits=c(-0.4,0.6), labels=c("0.7","1","1.5"),
                     breaks=c(-0.35,0,0.41)) +
  ggplot2::labs(
    title = "After the war"
  )+
  geom_vline(xintercept=0, linetype="solid") +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_6
#ggsave(plot_6, filename = "Figure 2d.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")





library(ggpubr)





###############################################################
# make panel plots for inermarriage before and after war fig S1 and S2
# ################################################################
panel_plot1 <- ggarrange(plot_3,plot_5, labels=c("", 
                                                 ""),
                         vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=FALSE)
figureS1 <-annotate_figure(panel_plot1,
                top = text_grob("Factors affecting Intermarriage", color = "black", face = "bold", size = 14),
                fig.lab = "", fig.lab.face = "bold"
)
ggsave(figure2a , filename = "Figure S1.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")

# make panel plots for reproduction and intermarriage posteriors before and after war fig 2a and 2b
panel_plot2 <- ggarrange(plot_4,plot_6, labels=c("", 
                                                 ""),
                         vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=FALSE)
figureS2 <- annotate_figure(panel_plot2,
                top = text_grob("Factors affecting Reproductive outcomes", color = "black", face = "bold", size = 14),
                fig.lab = "", fig.lab.face = "bold"
)
ggsave(figureS2, filename = "Figure S2.png", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


### save figures 2a and 2b
ggsave(plot_1, filename = "Figure 2a.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")

ggsave(plot_2, filename = "Figure 2b.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")



library(ggplot2)


## Figure S5 (age histogram)
## person_data <- readRDS("C:/Users/rofrly/Dropbox/Github/data files/person_data.rds")
m<- person_data
m <- m %>% filter(birthregion == "karelia" & birthyear<1926 & birthyear>1870)
m$hypergamy <- m$social_class-m$social_class_spouse
m$hypergamy <- ifelse(m$hypergamy<0,-1, ifelse(m$hypergamy>0, 1,0))

m$married_after <- ifelse(m$weddingyear<1945 | is.na(m$weddingyear), 0, 1)
m$log_pop <- log(m$birthpopulation)
m$log_pop <- m$log_pop-min(m$log_pop, na.rm=TRUE)
m$log_pop <- m$log_pop / max(m$log_pop, na.rm=TRUE)
m$fdf_log_pop <- log(m$fdf_population)
m$fdf_log_pop <- m$fdf_log_pop-min(m$fdf_log_pop, na.rm=TRUE)
m$fdf_log_pop <- m$fdf_log_pop/max(m$fdf_log_pop, na.rm=TRUE)
m$age <- 1944- m$birthyear
m$age_1940 <- 1940-m$birthyear
m$age <- m$age - min (m$age)
m$age <- m$age/ max(m$age)
m$census_1950 <- as.factor(m$'1950_census')
m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)




m<- m %>% dplyr::select(kids,hypergamy,outbred,returnedkarelia,sex, age,log_pop, education, agricult, technical,factory,service,
                        office,business,transport, birthplaceid,married_after,age_1940, birthyear)
m <- m[complete.cases(m),] # N=26,757
age_dist <-ggplot(data=m, aes(age_1940)) + 
  geom_histogram(breaks=seq(15, 69, by = 1), 
                 color="red", 
                 fill="black", 
                 alpha = 1) + 
  geom_vline(aes(xintercept=mean(age_1940)-0.5))+
  labs(title="Age distribution for evacuees at\ntime of first evacuation") +
  labs(x="Ages in 1940", y="Count") + 
  xlim(c(15,70)) + 
  ylim(c(0,1200))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  +
  guides(fill=FALSE)



age_dist

ggsave(age_dist, filename = "Figure S5.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")



##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##### Figures 3 and 4
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)
library(colortools)
library(ggpubr)
person_data <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/person_data.rds")
m<- person_data
m <- m %>% filter(birthregion == "karelia" & birthyear<1926 & birthyear>1870)
m$hypergamy <- m$social_class-m$social_class_spouse
m$hypergamy <- ifelse(m$hypergamy<0,-1, ifelse(m$hypergamy>0, 1,0))

m$married_after <- ifelse(m$weddingyear<1945 | is.na(m$weddingyear), 0, 1)
m$log_pop <- log(m$birthpopulation)
m$log_pop <- m$log_pop-min(m$log_pop, na.rm=TRUE)
m$log_pop <- m$log_pop / max(m$log_pop, na.rm=TRUE)
m$fdf_log_pop <- log(m$fdf_population)
m$fdf_log_pop <- m$fdf_log_pop-min(m$fdf_log_pop, na.rm=TRUE)
m$fdf_log_pop <- m$fdf_log_pop/max(m$fdf_log_pop, na.rm=TRUE)
m$age <- 1944- m$birthyear
m$age_1940 <- 1940-m$birthyear
m$age <- m$age - min (m$age)
m$age <- m$age/ max(m$age)
m$census_1950 <- as.factor(m$'1950_census')
m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)




m<- m %>% dplyr::select(kids,hypergamy,outbred,returnedkarelia,sex, age,log_pop, education, agricult, technical,factory,service,
                        office,business,transport, birthplaceid,married_after,age_1940)
m <- m[complete.cases(m),] # N=26,757

# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)


retb<- m%>% filter(returnedkarelia==1 & married_after==0)
remb <- m%>% filter(returnedkarelia==0 & married_after==0)

reta<- m%>% filter(returnedkarelia==1 & married_after==1)
rema <- m%>% filter(returnedkarelia==0 & married_after==1)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)




# load main models
KALL <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/Model_kids_all_FULL_INTS.rds")

attach(m)
Kretb <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  returnedkarelia=1L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =0L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_kbrt <- tidy_link(Kretb, KALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kbrt <- aggregate(kids ~ outbred + married_after, data=returned, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                            se_kids=std(x))) 

z <- unlist(rawdata_kbrt$kids)

rawdata_kbrt <- cbind(rawdata_kbrt,z)
rawdata_kbrt$kids <-  NULL



data <- df_kbrt %>% left_join (rawdata_kbrt, by =c("outbred"="outbred","married_after"="married_after"))


# Violin plots


library(HDInterval)
int_2<- hdi(data$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k1 <- ggplot(data = data, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = "dodge", fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position="dodge") +
  geom_errorbar(size=0.6,width=0.3,color="black",position="dodge",aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=1, aes(x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()


k1 <- k1 + 
  
  
  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                                                         "Married a \nResident Finn")) +
  
  scale_y_continuous(breaks=c(2.5,3,3.5),limits = c(2.5,3.75),
                     labels=c("2.5","3","3.5")) +
  
  
  
  xlab("") + ylab("Number of Children") + 
  ggtitle("Returned to Karelia") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k1


#  remained before kids
attach(m)
Kremb <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  returnedkarelia=0L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
  married_after =0L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_kbrm <- tidy_link(Kremb, KALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kbrm <- aggregate(kids ~ outbred + married_after, data=remained, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                            se_kids=std(x))) 

z <- unlist(rawdata_kbrm$kids)

rawdata_kbrm <- cbind(rawdata_kbrm,z)
rawdata_kbrm$kids <-  NULL



data2 <- df_kbrm %>% left_join (rawdata_kbrm, by =c("outbred"="outbred", "married_after"="married_after"))


# Violin plots


library(HDInterval)
int_2<- hdi(data2$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data2 <- data2[which(data2$lambda >min(int_2) & data2$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k2 <- ggplot(data = data2, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = "dodge", fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position = "dodge") +
  geom_errorbar(size=0.6,width=0.3,color="black",position="dodge",aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=1, aes(x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()


k2 <- k2 + 
  
  
  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                                                         "Married a \nResident Finn")) +
  
  scale_y_continuous(breaks=c(2.5,3,3.5),limits = c(2.5,3.75),
                     labels=c("","","")) +
  
  
  
  xlab("") + ylab("") + 
  ggtitle("Remained in western Finland") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k2


#  reeturned after kids
attach(m)
Kreta <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =1L,
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_kart <- tidy_link(Kreta, KALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kart <- aggregate(kids ~ outbred+married_after, data=returned, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                          se_kids=std(x))) 

z <- unlist(rawdata_kart$kids)

rawdata_kart <- cbind(rawdata_kart,z)
rawdata_kart$kids <-  NULL



data3 <- df_kart %>% left_join (rawdata_kart, by =c("outbred"="outbred","married_after"="married_after"))


# Violin plots


library(HDInterval)
int_2<- hdi(data3$lambda)
# yields a range of 2.79 to 3.13
# subset data
data3 <- data3[which(data3$lambda >min(int_2) & data3$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k3 <- ggplot(data = data3, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = "dodge", fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position = "dodge") +
  geom_errorbar(size=0.6,width=0.3,color="black",position="dodge",aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=1, aes(x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()


k3 <- k3 + 
  
  
  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                                                         "Married a \nResident Finn")) +
  
  scale_y_continuous(breaks=c(1.5,2,2.5),limits = c(1.5,3.0),
                     labels=c("1.5","2","2.5")) +
  
  
  
  xlab("") + ylab("Number of Children") + 
  ggtitle("Returned to Karelia") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k3


attach(m)
Krema <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_karm <- tidy_link(Krema, KALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_karm <- aggregate(kids ~ outbred+married_after, data=remained, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                          se_kids=std(x))) 

z <- unlist(rawdata_karm$kids)

rawdata_karm <- cbind(rawdata_karm,z)
rawdata_karm$kids <-  NULL



data4 <- df_karm %>% left_join (rawdata_karm, by =c("outbred"="outbred","married_after"="married_after"))
datatest <- data4 %>% filter (outbred==1) %>% 
  mutate(lambda= lambda)
datatest2 <- data4 %>% filter (outbred==0) %>% 
  mutate(lambda= lambda)

data4 <- rbind (datatest,datatest2)

# Violin plots


library(HDInterval)
int_2<- hdi(data4$lambda,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data4 <- data4[which(data4$lambda >min(int_2) & data4$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

#B47846


k4 <- ggplot(data = data4, aes(x = factor(outbred), y = newlambda)) +
  geom_violin(position = "dodge", fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position = "dodge") +
  geom_errorbar(size=0.6,width=0.3,color="black",position="dodge",aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=1, aes(x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()


k4 <- k4 + 
  
  
  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                                                         "Married a \nResident Finn")) +
  
  scale_y_continuous(breaks=c(1.5,2,2.5),limits = c(1.5,3.0),
                     labels=c("","","")) +
  
  
  
  xlab("") + ylab("") + 
  ggtitle("Remained in western Finland") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k4



### make figure 4a-d
figure4a <- ggarrange(k1,k2, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure4a <- annotate_figure(figure4a,
                            top = text_grob("MARRIED BEFORE THE WAR", color = "black", face = "bold", size = 8),
                            fig.lab = "", fig.lab.face = "bold"
)
figure4b <- ggarrange(k3,k4, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure4b <- annotate_figure(figure4b,
                            top = text_grob("MARRIED AFTER THE WAR", color = "black", face = "bold", size = 8),
                            fig.lab = "", fig.lab.face = "bold"
)
figure4 <- ggarrange(figure4a,figure4b, nrow=2)


ggsave(figure4, filename = "Figure 4a-4b_new.png", width = 4, height = 4, device = "png", dpi = 600,units = "in")

## Fig 3####################################

# full model
MALL <- readRDS("C:/Users/rofrly/Dropbox/Migrations paper/Models for NHB revision/model_30_outbred_all_w_all_interactions.rds")

attach(m)
obrt <- tidyr::crossing(
  returnedkarelia = 1L, # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =0L,
  hypergamy = c(-1L,1L),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rt_before <- tidy_link(obrt, MALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

before0 <- returned[ which(m$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rt_before <- aggregate(outbred ~ hypergamy+married_after, data=before0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                   se_out=std(x))) 

z <- unlist(rawdata_rt_before$outbred)

rawdata_rt_before <- cbind(rawdata_rt_before,z)
rawdata_rt_before$outbred <-  NULL



data5 <- df_rt_before%>% left_join (rawdata_rt_before, by =c("hypergamy"="hypergamy","married_after"="married_after"))



# Violin plots

library(HDInterval)
int_2<- hdi(data5$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data5 <- data5[which(data5$p >min(int_2) & data5$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k5 <- ggplot(data = data5, aes(x = factor(hypergamy), y = p, position="dodge")) +
  geom_violin( fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=0.6,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=1,aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k5 <- k5 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="Probability of intermarriage",breaks=c(0.1,0.2,0.3,0.4),
                     limits = c(0.05,0.55),
                     labels=c("0.1","0.2","0.3","0.4")) +
  
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Returned to Karelia") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k5


#########################
attach(m)
obrm <- tidyr::crossing(
  returnedkarelia = 0L, # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =0L,
  hypergamy = c(-1L,1L),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rm_before <- tidy_link(obrm, MALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

before0 <- remained[ which(remained$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rm_before <- aggregate(outbred ~ hypergamy+married_after, data=before0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                   se_out=std(x))) 

z <- unlist(rawdata_rm_before$outbred)

rawdata_rm_before <- cbind(rawdata_rm_before,z)
rawdata_rm_before$outbred <-  NULL



data6 <- df_rm_before%>% left_join (rawdata_rm_before, by =c("hypergamy"="hypergamy","married_after"="married_after"))



# Violin plots

library(HDInterval)
int_2<- hdi(data6$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data6 <- data6[which(data6$p >min(int_2) & data6$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k6 <- ggplot(data = data6, aes(x = factor(hypergamy), y = p, position="dodge")) +
  geom_violin( fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=0.6,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=1,aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k6 <- k6 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="",breaks=c(0.1,0.2,0.3,0.4),
                     limits = c(0.05,0.55),
                     labels=c("","","","")) +
  
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Remained in western Finland") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k6




######
attach(m)
oart <- tidyr::crossing(
  returnedkarelia = 1L, # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =1L,
  hypergamy = c(-1L,1L),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rt_after <- tidy_link(oart, MALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

after0 <- returned[ which(returned$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rt_after <- aggregate(outbred ~ hypergamy+married_after, data=after0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                 se_out=std(x))) 

z <- unlist(rawdata_rt_after$outbred)

rawdata_rt_after <- cbind(rawdata_rt_after,z)
rawdata_rt_after$outbred <-  NULL



data7 <- df_rt_after%>% left_join (rawdata_rt_after, by =c("hypergamy"="hypergamy","married_after"="married_after"))



# Violin plots

library(HDInterval)
int_2<- hdi(data7$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data7 <- data7[which(data7$p >min(int_2) & data7$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k7 <- ggplot(data = data7, aes(x = factor(hypergamy), y = p, position="dodge")) +
  geom_violin( fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=0.6,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=1, aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k7 <- k7 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="Probability of intermarriage",breaks=c(0.5,0.6,0.7,0.8,0.9),
                     limits = c(0.5,0.95),
                     labels=c("0.5","0.6","0.7","0.8","0.9")) +
  
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Returned to Karelia") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k7


#########################
attach(m)
oarm <- tidyr::crossing(
  returnedkarelia = 0L, # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =1L,
  hypergamy = c(-1L,1L),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rm_after <- tidy_link(oarm, MALL) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

after0 <- remained[ which(remained$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rm_after <- aggregate(outbred ~ hypergamy+married_after, data=after0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                 se_out=std(x))) 

z <- unlist(rawdata_rm_after$outbred)

rawdata_rm_after <- cbind(rawdata_rm_after,z)
rawdata_rm_after$outbred <-  NULL



data8 <- df_rm_after%>% left_join (rawdata_rm_after, by =c("hypergamy"="hypergamy","married_after"="married_after"))




# Violin plots

library(HDInterval)
int_2<- hdi(data8$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data8 <- data8[which(data8$p >min(int_2) & data8$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k8 <- ggplot(data = data8, aes(x = factor(hypergamy), y = newp, position="dodge")) +
  geom_violin( fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=0.6,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=1,aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k8 <- k8 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="",breaks=c(0.5,0.6,0.7,0.8,0.9),
                     limits = c(0.5,0.95),
                     labels=c("","","","","")) +
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Remained in western Finland") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k8


### make figure 4a-d
figure3a <- ggarrange(k5,k6, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure3a <- annotate_figure(figure3a,
                            top = text_grob("MARRIED BEFORE THE WAR", color = "black", face = "bold", size = 8),
                            fig.lab = "", fig.lab.face = "bold"
)
figure3b <- ggarrange(k7,k8, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure3b <- annotate_figure(figure3b,
                            top = text_grob("MARRIED AFTER THE WAR", color = "black", face = "bold", size = 8),
                            fig.lab = "", fig.lab.face = "bold"
)
figure3 <- ggarrange(figure3a,figure3b, nrow=2)


ggsave(figure3, filename = "Figure 3a-b.png", width = 4, height = 4, device = "png", dpi = 600,units = "in")



##### Model predictions
# Intermarriage full model
attach(m)
oarm <- tidyr::crossing(
  returnedkarelia = mean(returnedkarelia), # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=1L,
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rm_after <- tidy_link(oarm, MALL) %>% as.data.frame()

mean(df_rm_after$p)
std <- function(x) sd(x)/sqrt(length(x))
hdi(df_rm_after$p)

### get predictions for reproduction model main
attach(m)
Krema <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = 0L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_karm <- tidy_link(Krema, KALL) %>% as.data.frame()
mean(df_karm$lambda)
std <- function(x) sd(x)/sqrt(length(x))
hdi(df_karm$lambda)


##### Model predictions
# Intermarriage full model
attach(m)
oarm <- tidyr::crossing(
  returnedkarelia = mean(returnedkarelia), # the "L" makes the value an integer, avoiding possible errors
  #outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=1L,
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_rm_after <- tidy_link(oarm, MALL) %>% as.data.frame()

mean(df_rm_after$p)
std <- function(x) sd(x)/sqrt(length(x))
hdi(df_rm_after$p)

### get predictions for reproduction model main
attach(m)
Krema <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = 0L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=mean(education),
  agriculture = mean(agricult),
  technical = mean(technical),
  transport = mean(transport),
  factory = mean(factory),
  office = mean(office),
  service = mean(service),
  business = mean(business)) %>%
  as.data.frame()
detach(m)
library(tidybayes.rethinking)

df_karm <- tidy_link(Krema, KALL) %>% as.data.frame()
mean(df_karm$lambda)
std <- function(x) sd(x)/sqrt(length(x))
hdi(df_karm$lambda)






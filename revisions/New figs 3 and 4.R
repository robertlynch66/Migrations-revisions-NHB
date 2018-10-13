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


# load models from table s4
OBRT <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_51_outbred_before_returned_no_intxs.rds")
OBRM <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_43_outbred_before_remained_no_intxs.rds")
OART <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_12_outbred_returned_after.rds")
OARM <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_11_outbred_remained_after.rds")
KBRT <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_50_kids_before_returned_no_intxs.rds")
KBRM <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_42_kids_before_remained_no_intxs.rds")
KART <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_10_kids_returned_after.rds")
KARM <-readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_9_kids_remained_after.rds")

## before and after models- table s2
KA <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")
KB <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")


attach(before)
Kretb <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  returnedkarelia=1L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  #married_after =0L,
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
detach(before)
library(tidybayes.rethinking)

df_kbrt <- tidy_link(Kretb, KB) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kbrt <- aggregate(kids ~ outbred, data=retb, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                              se_kids=std(x))) 

z <- unlist(rawdata_kbrt$kids)

rawdata_kbrt <- cbind(rawdata_kbrt,z)
rawdata_kbrt$kids <-  NULL



data <- df_kbrt %>% left_join (rawdata_kbrt, by =c("outbred"="outbred"))


# Violin plots


library(HDInterval)
int_2<- hdi(data$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k1 <- ggplot(data = data, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = dodge, fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,color="black",position=dodge,aes(x=factor(outbred),
                                                    #fill=factor(returnedkarelia),
                                                    
                                                    ymin=(mean_kids-se_kids),
                                                    ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, aes(x = factor(outbred), y = mean_kids)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k1


#  remaine before kids
attach(before)
Kremb <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  returnedkarelia=0L,
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
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
detach(before)
library(tidybayes.rethinking)

df_kbrm <- tidy_link(Kremb, KB) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kbrm <- aggregate(kids ~ outbred, data=remb, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                        se_kids=std(x))) 

z <- unlist(rawdata_kbrm$kids)

rawdata_kbrm <- cbind(rawdata_kbrm,z)
rawdata_kbrm$kids <-  NULL



data2 <- df_kbrm %>% left_join (rawdata_kbrm, by =c("outbred"="outbred"))


# Violin plots


library(HDInterval)
int_2<- hdi(data2$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data2 <- data2[which(data2$lambda >min(int_2) & data2$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k2 <- ggplot(data = data2, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = dodge, fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,color="black",position=dodge,aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, aes(x = factor(outbred), y = mean_kids)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k2


#  reeturned after kids
attach(after)
Kreta <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =1L,
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
detach(after)
library(tidybayes.rethinking)

df_kart <- tidy_link(Kreta, KA) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_kart <- aggregate(kids ~ outbred, data=reta, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                        se_kids=std(x))) 

z <- unlist(rawdata_kart$kids)

rawdata_kart <- cbind(rawdata_kart,z)
rawdata_kart$kids <-  NULL



data3 <- df_kart %>% left_join (rawdata_kart, by =c("outbred"="outbred"))


# Violin plots


library(HDInterval)
int_2<- hdi(data3$lambda)
# yields a range of 2.79 to 3.13
# subset data
data3 <- data3[which(data3$lambda >min(int_2) & data3$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred




k3 <- ggplot(data = data3, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = dodge, fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,color="black",position=dodge,aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, aes(x = factor(outbred), y = mean_kids)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k3


attach(after)
Krema <- tidyr::crossing(
  # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  returnedkarelia =0L,
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
detach(after)
library(tidybayes.rethinking)

df_karm <- tidy_link(Krema, KA) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_karm <- aggregate(kids ~ outbred, data=rema, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                        se_kids=std(x))) 

z <- unlist(rawdata_karm$kids)

rawdata_karm <- cbind(rawdata_karm,z)
rawdata_karm$kids <-  NULL



data4 <- df_karm %>% left_join (rawdata_karm, by =c("outbred"="outbred"))
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


k4 <- ggplot(data = data4, aes(x = factor(outbred), y = lambda)) +
  geom_violin(position = dodge, fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,color="black",position=dodge,aes(x=factor(outbred),
                                                                  #fill=factor(returnedkarelia),
                                                                  
                                                                  ymin=(mean_kids-se_kids),
                                                                  ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, aes(x = factor(outbred), y = mean_kids)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k4



### make figure 4a-d
figure4a <- ggarrange(k1,k2, labels=c("", 
                                       ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure4a <- annotate_figure(figure4a,
                           top = text_grob("MARRIED BEFORE THE WAR", color = "black", face = "bold", size = 14),
                           fig.lab = "", fig.lab.face = "bold"
)
figure4b <- ggarrange(k3,k4, labels=c("", 
                                     ""),
                     vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure4b <- annotate_figure(figure4b,
                           top = text_grob("MARRIED AFTER THE WAR", color = "black", face = "bold", size = 14),
                           fig.lab = "", fig.lab.face = "bold"
)
figure4 <- ggarrange(figure4a,figure4b, nrow=2)


ggsave(figure4, filename = "Figure 4a-4b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

## Fig 3####################################

#Read in models
OB <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
OA <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")

attach(before)
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
detach(before)
library(tidybayes.rethinking)

df_rt_before <- tidy_link(obrt, OB) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

before0 <- retb[ which(before$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rt_before <- aggregate(outbred ~ hypergamy, data=before0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                    se_out=std(x))) 

z <- unlist(rawdata_rt_before$outbred)

rawdata_rt_before <- cbind(rawdata_rt_before,z)
rawdata_rt_before$outbred <-  NULL



data5 <- df_rt_before%>% left_join (rawdata_rt_before, by =c("hypergamy"="hypergamy"))



# Violin plots

library(HDInterval)
int_2<- hdi(data5$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data5 <- data5[which(data5$p >min(int_2) & data5$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k5 <- ggplot(data = data5, aes(x = factor(hypergamy), y = p)) +
  geom_violin( fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,aes(x=factor(hypergamy),
                                                    ymin=(mean_out-se_out),
                                                    ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(x = factor(hypergamy), y = mean_out)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k5


#########################
attach(before)
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
detach(before)
library(tidybayes.rethinking)

df_rm_before <- tidy_link(obrm, OB) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

before0 <- remb[ which(before$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rm_before <- aggregate(outbred ~ hypergamy, data=before0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                     se_out=std(x))) 

z <- unlist(rawdata_rm_before$outbred)

rawdata_rm_before <- cbind(rawdata_rm_before,z)
rawdata_rm_before$outbred <-  NULL



data6 <- df_rm_before%>% left_join (rawdata_rm_before, by =c("hypergamy"="hypergamy"))



# Violin plots

library(HDInterval)
int_2<- hdi(data6$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data6 <- data6[which(data6$p >min(int_2) & data6$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k6 <- ggplot(data = data6, aes(x = factor(hypergamy), y = p)) +
  geom_violin( fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(x = factor(hypergamy), y = mean_out)) +
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
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k6




######
attach(after)
oart <- tidyr::crossing(
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
detach(after)
library(tidybayes.rethinking)

df_rt_after <- tidy_link(oart, OA) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

after0 <- reta[ which(after$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rt_after <- aggregate(outbred ~ hypergamy, data=after0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                     se_out=std(x))) 

z <- unlist(rawdata_rt_after$outbred)

rawdata_rt_after <- cbind(rawdata_rt_after,z)
rawdata_rt_after$outbred <-  NULL



data7 <- df_rt_after%>% left_join (rawdata_rt_after, by =c("hypergamy"="hypergamy"))



# Violin plots

library(HDInterval)
int_2<- hdi(data7$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data7 <- data7[which(data7$p >min(int_2) & data7$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k7 <- ggplot(data = data7, aes(x = factor(hypergamy), y = p)) +
  geom_violin( fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k7 <- k7 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="Probability of intermarriage",breaks=c(0.7,0.8,0.9),
                     limits = c(0.65,0.95),
                     labels=c("0.7","0.8","0.9")) +
  
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Returned to Karelia") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k7


#########################
attach(after)
oarm <- tidyr::crossing(
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
detach(after)
library(tidybayes.rethinking)

df_rm_after <- tidy_link(oarm, OA) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

after0 <- rema[ which(after$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_rm_after <- aggregate(outbred ~ hypergamy, data=after0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                     se_out=std(x))) 

z <- unlist(rawdata_rm_after$outbred)

rawdata_rm_after <- cbind(rawdata_rm_after,z)
rawdata_rm_after$outbred <-  NULL



data8 <- df_rm_after%>% left_join (rawdata_rm_after, by =c("hypergamy"="hypergamy"))


datatest <- data8 %>% filter (hypergamy==-1) %>% 
  mutate(p= p)
datatest2 <- data8 %>% filter (hypergamy==1) %>% 
  mutate(p= p)

data8 <- rbind (datatest,datatest2)

# Violin plots

library(HDInterval)
int_2<- hdi(data8$p,credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data8 <- data8[which(data8$p >min(int_2) & data8$p < max(int_2)), ]

data8 <- data8
#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





k8 <- ggplot(data = data8, aes(x = factor(hypergamy), y = p)) +
  geom_violin( fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=1,width=0.3,aes(x=factor(hypergamy),
                                     ymin=(mean_out-se_out),
                                     ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


k8 <- k8 + scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                                    "Married up")) +
  scale_y_continuous(name="",breaks=c(0.7,0.8,0.9),
                     limits = c(0.65,0.95),
                     labels=c("","","")) +
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Remained in western Finland") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



k8


### make figure 4a-d
figure3a <- ggarrange(k5,k6, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure3a <- annotate_figure(figure3a,
                            top = text_grob("MARRIED BEFORE THE WAR", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
figure3b <- ggarrange(k7,k8, labels=c("", 
                                      ""),
                      vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure3b <- annotate_figure(figure3b,
                            top = text_grob("MARRIED AFTER THE WAR", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
figure3 <- ggarrange(figure3a,figure3b, nrow=2)


ggsave(figure3, filename = "Figure 3a-b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


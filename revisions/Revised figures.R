
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
model_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
model_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")

outbred_all <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_30_outbred_all_w_all_interactions.rds")
kids_all <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_31_kids_all_w_all_interactions.rds")

kids_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
kids_after <-readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")
  
inter_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
inter_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")
  
inter_after_return <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_12_outbred_returned_after.rds")
inter_after_remain <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_11_outbred_remained_after.rds")

kids_after_remain <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_9_kids_remained_after.rds")
kids_after_return <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_10_kids_returned_after.rds")
attach(m)
in_rt <- tidyr::crossing(
  returnedkarelia = 0L, # the "L" makes the value an integer, avoiding possible errors
  outbred = mean(outbred),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
  married_after =1L,
  hypergamy = mean(hypergamy),
  population = mean(log_pop),
  education=0L,
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

df_kids_after<- tidy_link(in_rt, kids_all) %>% as.data.frame()
mean(df_kids_after$lambda)
hdi(df_kids_after$lambda)


library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_before <- aggregate(kids ~ outbred + returnedkarelia, data=before, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                              se_kids=std(x))) 

 z <- unlist(rawdata_before$kids)

rawdata_before <- cbind(rawdata_before,z)
rawdata_before$kids <-  NULL



data <- df_before %>% left_join (rawdata_before, by =c("returnedkarelia"="returnedkarelia","outbred"="outbred"))

    
# Violin plots


library(HDInterval)
int_2<- hdi(data$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred



dodge <- position_dodge(width = 1.7)

b1 <- ggplot(data = data, aes(x = factor(outbred), y = lambda, fill = factor(returnedkarelia))) +
  geom_violin(position = dodge, alpha=0.46, aes(colour = factor(returnedkarelia)))+
  geom_boxplot(width=.1, outlier.colour=NA,color="black", position = dodge) +
  geom_errorbar(size=1,width=0.3,position=dodge,aes(group=factor(returnedkarelia),
                                                    x=factor(outbred),
                                                    #fill=factor(returnedkarelia),
                                                    colour=factor(returnedkarelia),
                                                    ymin=(mean_kids-se_kids),
                                                    ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, position=dodge,aes(group=factor(returnedkarelia), colour = factor(returnedkarelia),
                                               x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()





b1 <- b1 + scale_fill_manual(name="",labels=c("Posterior for evacuees who\n remained in western Finland\n (95% HDI)",
                                              "Posterior for evacuees who\n returned to Karelia\n (95% HDI)"), values=c("#4682B4","#B47846")) + 
  scale_color_manual(name="",labels=c("Remained observed (+/- SE)","Returned observed (+/- SE)"),
                     values=c("#4682B4","#B47846")) +
  
  


  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                              "Married a \nResident Finn")) +
  
  scale_y_continuous(breaks=c(1.5,2,2.5,3,3.5),limits = c(1.5,3.75),
                     labels=c("1.5","2","2.5","3","3.5")) +
  

  
  xlab("Intermarriage") + ylab("Number of Children") + 
  ggtitle("Married before the war") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



b1

#ggsave(b1, filename = "Figure 2a.tiff", width = 9, height = 7, device = "tiff", dpi = 600,units = "in")

#############################################
############################################################
############################################################
# make after war plot
attach(after)
in_rt <- tidyr::crossing(
  returnedkarelia = c(0L,1L), # the "L" makes the value an integer, avoiding possible errors
  outbred = c(0L,1L),
  birthplace_id_seq = 1,
  sex = mean(sex),
  age = mean(age),
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
detach(after)
library(tidybayes.rethinking)

df_after <- tidy_link(in_rt, model_after) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for kids
# # make standard error function
std <- function(x) sd(x)/sqrt(length(x))
rawdata_after <- aggregate(kids ~ outbred + returnedkarelia, data=after, FUN= function(x) c(mean_kids=mean(x),sd_kids=sd(x),
                                                                                              se_kids=std(x))) 

z <- unlist(rawdata_after$kids)

rawdata_after <- cbind(rawdata_after,z)
rawdata_after$kids <-  NULL



data2 <- df_after %>% left_join (rawdata_after, by =c("returnedkarelia"="returnedkarelia","outbred"="outbred"))




# Violin plots
# get 89 percetn interval


library(HDInterval)
int_2<- hdi(data2$lambda, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data2 <- data2[which(data2$lambda >min(int_2) & data2$lambda < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred



dodge <- position_dodge(width = 1.7)

b2 <- ggplot(data = data2, aes(x = factor(outbred), y = lambda, fill = factor(returnedkarelia))) +
  geom_violin(position = dodge, alpha=0.46, aes(colour = factor(returnedkarelia)))+
  geom_boxplot(width=.1, outlier.colour=NA,color="black", position = dodge) +
  geom_errorbar(size=1,width=0.3,position=dodge,aes(group=factor(returnedkarelia),
                                                    x=factor(outbred),
                                                    #fill=factor(returnedkarelia),
                                                    colour=factor(returnedkarelia),
                                                    ymin=(mean_kids-se_kids),
                                                    ymax=(mean_kids+se_kids))) +
  geom_point(alpha=1,size=2, position=dodge,aes(group=factor(returnedkarelia), colour = factor(returnedkarelia),
                                                x = factor(outbred), y = mean_kids)) +
  scale_shape_identity()





b2 <- b2 + scale_fill_manual(name="",labels=c("Posterior for evacuees who\n remained in western Finland\n (95% HDI)",
                                              "Posterior for evacuees who\n returned to Karelia\n (95% HDI)"), values=c("#4682B4","#B47846")) + 
  scale_color_manual(name="",labels=c("Remained observed (+/- SE)","Returned observed (+/- SE)"),
                     values=c("#4682B4","#B47846")) +
  
  
  
  
  scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Married a\n Karelian",
                                                                         "Married a \nResident Finn")) +
  
  
  scale_y_continuous(breaks=c(1.5,2,2.5,3,3.5),limits = c(1.5,3.75),
                     labels=c("","","","","")) +
  xlab("Intermarriage") + ylab("") + 
  ggtitle("Married after the war") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



b2

#ggsave(b2, filename = "Figure 2b.tiff", width = 9, height = 7, device = "tiff", dpi = 600,units = "in")
## Figure 1a and 1b
## Models from table 3 again
## Read in models
model_before_out <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
model_after_out <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")

## Fig 1a
attach(before)
in_rt <- tidyr::crossing(
  returnedkarelia = c(0L,1L), # the "L" makes the value an integer, avoiding possible errors
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

df_before <- tidy_link(in_rt, model_before_out) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

before0 <- before[ which(before$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_before <- aggregate(outbred ~ hypergamy + returnedkarelia, data=before0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                  se_out=std(x))) 

z <- unlist(rawdata_before$outbred)

rawdata_before <- cbind(rawdata_before,z)
rawdata_before$outbred <-  NULL



data3 <- df_before%>% left_join (rawdata_before, by =c("returnedkarelia"="returnedkarelia","hypergamy"="hypergamy"))



# Violin plots

library(HDInterval)
int_2<- hdi(data3$p, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data3 <- data3[which(data3$p >min(int_2) & data3$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)



library(scales)
trans <- function(x) {
  ifelse(x > 0.7, x - 0.2, ifelse(x < 0.5, x + 0.2, x/4))
}
inv <- function(x) {
  ifelse(x > 0.5, x + 0.2, ifelse(x < 0.7, x - 0.2, x*4))
}
my_trans <- trans_new("my_trans", trans, inv)

dodge <- position_dodge(width = 1.7)

b3 <- ggplot(data = data3, aes(x = factor(hypergamy), y = p, fill = factor(returnedkarelia))) +
  geom_violin(position = dodge, alpha=0.46, aes(colour = factor(returnedkarelia)))+
  geom_boxplot(width=.1, outlier.colour=NA,color="black", position = dodge) +
  geom_errorbar(size=1,width=0.3,position=dodge,aes(group=factor(returnedkarelia),
                                                    x=factor(hypergamy),
                                                    #fill=factor(returnedkarelia),
                                                    colour=factor(returnedkarelia),
                                                    ymin=(mean_out-se_out),
                                                    ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(group=factor(returnedkarelia), colour = factor(returnedkarelia),
                                                x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()





b3 <- b3 + scale_fill_manual(name="",labels=c("Posterior for evacuees who\n remained in western Finland\n (95% HDI)",
                                              "Posterior for evacuees who\n returned to Karelia\n (95% HDI)"), values=c("#4682B4","#B47846")) + 
  scale_color_manual(name="",labels=c("Remained observed (+/- SE)","Returned observed (+/- SE)"),
                     values=c("#4682B4","#B47846")) +
  
  
  
  
  scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                         "Married up")) +
  scale_y_continuous(name="Probability of intermarriage",breaks=c(0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9),
                      limits = c(0.0,1.0),
                     labels=c("0.1","0.2","0.3","0.4","0.5","0.7", "0.8","0.9")) +
  
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="Probability of intermarriage",labels=scaleFUN)+
  ggtitle("Married before the war") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



b3

#ggsave(b3, filename = "Figure 1a.tiff", width = 9, height = 7, device = "tiff", dpi = 600,units = "in")


# Fig 1b
attach(after)
in_rt <- tidyr::crossing(
  returnedkarelia = c(0L,1L), # the "L" makes the value an integer, avoiding possible errors
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

df_after <- tidy_link(in_rt, model_after_out) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for intermarriage
# # make standard error function

after0 <- after[ which(after$hypergamy!=0), ]
std <- function(x) sd(x)/sqrt(length(x))
rawdata_after <- aggregate(outbred ~ hypergamy + returnedkarelia, data=after0, FUN= function(x) c(mean_out=mean(x),sd_out=sd(x),
                                                                                                 se_out=std(x))) 

z <- unlist(rawdata_after$outbred)

rawdata_after <- cbind(rawdata_after,z)
rawdata_after$outbred <-  NULL



data4 <- df_after%>% left_join (rawdata_after, by =c("returnedkarelia"="returnedkarelia","hypergamy"="hypergamy"))



# Violin plots

library(HDInterval)
int_2<- hdi(data4$p, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data4 <- data4[which(data4$p >min(int_2) & data4$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)

dodge <- position_dodge(width = 1.7)

b4 <- ggplot(data = data4, aes(x = factor(hypergamy), y = p, fill = factor(returnedkarelia))) +
  geom_violin(position = dodge, alpha=0.46, aes(colour = factor(returnedkarelia)))+
  geom_boxplot(width=.1, outlier.colour=NA,color="black", position = dodge) +
  geom_errorbar(size=1,width=0.5,position=dodge,aes(group=factor(returnedkarelia),
                                                    x=factor(hypergamy),
                                                    #fill=factor(returnedkarelia),
                                                    colour=factor(returnedkarelia),
                                                    ymin=(mean_out-se_out),
                                                    ymax=(mean_out+se_out))) +
  geom_point(alpha=1,size=2, position=dodge,aes(group=factor(returnedkarelia), colour = factor(returnedkarelia),
                                                x = factor(hypergamy), y = mean_out)) +
  scale_shape_identity()


b4 <- b4 + scale_fill_manual(name="",labels=c("Posterior for evacuees who\n remained in western Finland\n (95% HDI)",
                                              "Posterior for evacuees who\n returned to Karelia\n (95% HDI)"), values=c("#4682B4","#B47846")) + 
  scale_color_manual(name="",labels=c("Remained observed (+/- SE)","Returned observed (+/- SE)"),
                     values=c("#4682B4","#B47846")) +
  
  
  
  
  scale_x_discrete(breaks=c("-1","1"),limits = c("-1","skip","1"),labels=c("Married down",
                                                                           "Married up")) +
  
  scale_y_continuous(name="",breaks=c(0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9),
                     limits = c(0.0,1.0),
                     labels=c("","","","","","", "","")) +
  
  
  xlab("Social class of spouse") + 
  #scale_y_continuous(name="",labels=scaleFUN)+
ggtitle("Married after the war") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))  



b4

#ggsave(b4, filename = "Figure 1b.tiff", width = 9, height = 7, device = "tiff", dpi = 600,units = "in")

### Make the posterior predictive check figures for models 1 and 2



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
model_outbred <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_30_outbred_all_w_all_interactions.rds")
model_kids <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_31_kids_all_w_all_interactions.rds")

model_kids_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")
model_kids_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
model_outbred_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
model_outbred_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")


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


# figure 4a
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
pi_kids <- apply(sims_kids, 2, my_PI) %>% as.data.frame()
pi_kids <- t(pi_kids)

pi_kids <- cbind(kids,pi_kids) %>% as.data.frame()
colnames(pi_kids) <- c("obs","low","high")
pi_kids$count <- ifelse(pi_kids$obs>=pi_kids$low & pi_kids$obs<=pi_kids$high,0,1)
sum(pi_kids$count)

# figure 4b
pi_kids_before <- apply(sims_kids_before, 2, my_PI) %>% as.data.frame()
pi_kids_before <- t(pi_kids_before)

pi_kids_before <- cbind(kids_before,pi_kids_before) %>% as.data.frame()
colnames(pi_kids_before) <- c("obs","low","high")
pi_kids_before$count <- ifelse(pi_kids_before$obs>=pi_kids_before$low & pi_kids_before$obs<=pi_kids_before$high,0,1)
sum(pi_kids_before$count)

# figure 4c
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
#plot.margin=grid::unit(c(0,0,0,0), "mm"))
model_6
#ggsave(model_6, filename = "Figure S8.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)


model_outbred <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_30_outbred_all_w_all_interactions.rds")
model_kids <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/Model_31_kids_all_w_all_interactions.rds")

model_kids_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_61_kids_after_w_intxs.rds")
model_kids_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_60_kids_before_w_intxs.rds")
model_outbred_before <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_70_outbred_before_w_intxs.rds")
model_outbred_after <- readRDS("C:/Users/robert/Dropbox/Github/Migration_ms_NHB/revisions/Models for NHB revision/Final models for revision/model_38_outbred_after_w_all_interactions.rds")


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


post_model2 <- extract.samples(model_kids) %>% as.data.frame()
post_model2 <- post_model2 %>% select (1:19)
post_model2 <- post_model2 [c(13,1,2,3,6,10,11,15,16,17,18)]
post_model2$bage <- post_model2$bage/54
post_model2$brkage <- post_model2$brkage/54
names(post_model2) <- c(labels=c("Age","Hypergamy","Intermarriage","Returned to Karelia","Agriculture",
                                 "Office workers","Business workers","Married after war",
                                 "Returned to Karelia X\nAge","Married after war X\nReturned to Karelia",
                                 "Married after war X\nMale"))

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
post_model4 <- extract.samples(model_kids_before) %>% as.data.frame()
post_model4 <- post_model4 %>% select (1:15)
post_model4 <- post_model4 [c(1,2,5,9,10)]
names(post_model4) <- c(labels=c("Hypergamy","Intermarriage","Agriculture","Office workers","Business workers"))

### before and after the war (table S2 models)
post_model5 <- extract.samples(model_outbred_after) %>% as.data.frame()
post_model5 <- post_model5 %>% select (1:15)
post_model5 <- post_model5 [c(3,4,1,10,11,14)]
post_model5$bage <- post_model5$bage/54
names(post_model5) <- c(labels=c("Male","Age","Hypergamy","Service industry", "Office workers",
                                 "Returned to Karelia X\nMale"))

### before and after the war (table S2 models)
post_model6 <- extract.samples(model_kids_after) %>% as.data.frame()
post_model6 <- post_model6 %>% select (1:16)
post_model6 <- post_model6 [c(14,2,5)]
names(post_model6) <- c(labels=c("Returned to Karelia", "Intermarriage","Agriculture"))

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


color_scheme_set("green")
p2 <- mcmc_intervals(post_model2, prob = 0.5, prob_outer = 0.9,
                     point_est = c("median"), rhat = numeric())
plot_2<- p2 + 
  scale_x_continuous(name="Odds ratio",limits=c(-0.35,0.45), labels=c("0.75","1","1.5"),
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
#ggsave(plot_2, filename = "Figure S2.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")



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





###################Make all final figures - panels etcc.
library(ggpubr)
###################################################
# make a function that puts a gap in the y axis



## Make panel figures 3a-3b and 4a-4b
panel_1 <- ggarrange(b3,b4, labels=c("", 
                                     ""),
                     vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure3 <- annotate_figure(panel_1,
                           top = text_grob("Factors affecting Intermarriage", color = "black", face = "bold", size = 14),
                           fig.lab = "", fig.lab.face = "bold"
)
ggsave(figure3, filename = "Figure 3a-3b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

panel_2 <- ggarrange(b1,b2, labels=c("", 
                                     ""),
                     vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)
figure4 <- annotate_figure(panel_2,
                           top = text_grob("Factors affecting reproductive outcomes", color = "black", face = "bold", size = 14),
                           fig.lab = "", fig.lab.face = "bold"
)
ggsave(figure4, filename = "Figure 4a-4b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

###############################################################
# make panel plots for inermarriage before and after war fig 2a and 2b
# ################################################################
panel_plot1 <- ggarrange(plot_3,plot_5, labels=c("", 
                                                 ""),
                         vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=FALSE)
figure2a <-annotate_figure(panel_plot1,
                top = text_grob("Factors affecting Intermarriage", color = "black", face = "bold", size = 14),
                fig.lab = "", fig.lab.face = "bold"
)
ggsave(figure2a , filename = "Figure 2a.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

# make panel plots for reproduction and intermarriage posteriors before and after war fig 2a and 2b
panel_plot2 <- ggarrange(plot_4,plot_6, labels=c("", 
                                                 ""),
                         vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=FALSE)
figure2b <- annotate_figure(panel_plot2,
                top = text_grob("Factors affecting Reproductive outcomes", color = "black", face = "bold", size = 14),
                fig.lab = "", fig.lab.face = "bold"
)
ggsave(figure2b, filename = "Figure 2b.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


### save figures s1 and s2
ggsave(plot_1, filename = "Figure S1.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

ggsave(plot_2, filename = "Figure S2.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")


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
ggsave(figureS3, filename = "Figure S3a-c.jpeg", width = 16, height = 4, device = "jpeg", dpi = 600,units = "in")


panel_plot_s4 <- ggarrange(model_2,model_4,model_6, labels=c("", 
                                                             ""),
                           vjust=2.5, hjust= -2,ncol=3, nrow=1, common.legend=TRUE)
figureS4 <- annotate_figure(panel_plot_s4,
                            top = text_grob("Posterior predictive checks for models Factors affecting Reproductive outcomes", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
ggsave(figureS4, filename = "Figure S4a-c.jpeg", width = 12, height = 4, device = "jpeg", dpi = 600,units = "in")

library(ggplot2)


## Figure S5 (age histogram)
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

ggsave(age_dist, filename = "Figure S5.jpeg", width = 9, height = 7, device = "jpeg", dpi = 600,units = "in")

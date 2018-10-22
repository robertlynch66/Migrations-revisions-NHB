require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(dplyr)
library(rethinking)
require("lme4")
require("MuMIn")
require("lsmeans")

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

m$census_1950 <- as.factor(m$'1950_census')
m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)

m<- m %>% dplyr::select(kids,hypergamy,outbred,returnedkarelia,sex, age,log_pop, education, agricult, technical,factory,service,
                        office,business,transport, birthplaceid,married_after,birthlat,birthlon)
m <- m[complete.cases(m),] # N=26,757



library(ade4)

# run a mantel test for spatial autocrrelation with our 2 DV's - kids and intermarriage
m<- m[1:1000,]

birthplace.dists <- dist(cbind(m$birthlon,m$birthlat))
kids.dists <- dist(m$kids)
marriage.dists <- dist(m$outbred)


#birthplace.dists <- as.matrix(birthplace.dists)
#kids.dists <- as.matrix(kids.dists)
#marriage.dists <- as.matrix(marriage.dists)
# mantel test
mantel.rtest(birthplace.dists, kids.dists, nrepet=9999)

mantel.rtest(birthplace.dists, marriage.dists, nrepet=9999)

# check for glmm with and without

library(lme4)
# Mixed models with one random effect
model_1 <- glmer(outbred ~ age+ 
                   sex +
                   hypergamy +
                   returnedkarelia +
                   log_pop +
                   education +
                   agricult +
                   technical +
                   factory +
                   service +
                   office +
                   business +
                   transport +
                   married_after+
                   returnedkarelia*sex+
                   returnedkarelia*age+
                   married_after*returnedkarelia+
                   married_after*sex+
                   married_after*hypergamy+
                   (1 | birthplaceid), 
                 data = m,
                 family = binomial(link = "logit"), nAGQ = 1)
summary(model_1)

# without random effects

model_1_no_bplace_id <- glm(outbred ~ age+ 
                   sex +
                   hypergamy +
                   returnedkarelia +
                   log_pop +
                   education +
                   agricult +
                   technical +
                   factory +
                   service +
                   office +
                   business +
                   transport +
                   married_after+
                   returnedkarelia*sex+
                   returnedkarelia*age+
                   married_after*returnedkarelia+
                   married_after*sex+
                   married_after*hypergamy, 
                 data = m,
                 family = binomial(link = "logit"))
summary(model_1_no_bplace_id)

# GLM
model_2  <- glmer(kids ~ age+ 
                             sex +
                             hypergamy +
                             returnedkarelia +
                             log_pop +
                             education +
                             agricult +
                             technical +
                             factory +
                             service +
                             office +
                             business +
                             transport +
                             married_after+
                             returnedkarelia*sex+
                             returnedkarelia*age+
                             married_after*returnedkarelia+
                             married_after*sex+
                             married_after*hypergamy+
                             (1 | birthplaceid), 
                           data = m,
                           family = "poisson", nAGQ = 1)
summary(model_2)

# without random effects

model_2_no_bplace_id <- glm(kids ~ age+ 
                              sex +
                              hypergamy +
                              returnedkarelia +
                              log_pop +
                              education +
                              agricult +
                              technical +
                              factory +
                              service +
                              office +
                              business +
                              transport +
                              married_after+
                              returnedkarelia*sex+
                              returnedkarelia*age+
                              married_after*returnedkarelia+
                              married_after*sex+
                              married_after*hypergamy, 
                            data = m,
                            family = "poisson")
summary(model_2_no_bplace_id)

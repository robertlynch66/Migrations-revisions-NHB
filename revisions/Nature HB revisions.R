#### Run in rethinking
# Model 1 in rethinking
#### RUN kids all###########################################
#### #############################################################
#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after_war = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0
  ),
  chains=4,cores=4)
    
    path<- (paste0("results/"))
  filename <- "Model_1_kids_all.rds"
  
  saveRDS(model, paste0(path, filename))

###Model_2_outbred_all
#### outbred model  - USE THIS
outbred_model <- glmer (outbred ~  hypergamy + returnedkarelia  + sex + age + log_pop +education + agricult + technical + 
                          factory + service + office + 
                          business + transport + (1 | birthplaceid)  , data=m, family="binomial")
summary(outbred_model)

# Model 2 in rethinking
#### RUN Outbred all###########################################
#### #############################################################
#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after_war = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)

model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after_war,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0))



path<- (paste0("results/"))
filename <- "model_2_outbred_all.rds"
saveRDS(model, paste0(path, filename))


#  Model 3 kids remained only
#### RUN kids all###########################################
#### #############################################################
#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(remained))

data_list <- list(
  kids = remained$kids,
  hypergamy = remained$hypergamy,
  outbred = remained$outbred,
  returnedkarelia = remained$returnedkarelia,
  sex = remained$sex,
  education = remained$education,
  agriculture = remained$agricult,
  technical = remained$technical,
  factory = remained$factory,
  service = remained$service,
  office = remained$office,
  business = remained$business,
  transport = remained$transport,
  age = remained$age,
  population = remained$log_pop,
  married_after = remained$married_after,
  birthplace_id_seq = remained$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0
  ),
  chains=4,cores=4)
path<- (paste0("results/"))
filename <- "Model_3_kids_remained.rds"

saveRDS(model, paste0(path, filename))
# model 4 kids returned only
# Model 1 in rethinking
#### RUN kids all###########################################
#### #############################################################
#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(remained))

data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  population = returned$log_pop,
  married_after = returned$married_after,
  birthplace_id_seq = returned$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_4_kids_returned.rds"

saveRDS(model, paste0(path, filename))
# model 5 kids married after war

#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(remained))

data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  married_after = after$married_after,
  birthplace_id_seq = after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      brk * returnedkarelia,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, brk) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, brk=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_5_kids_after.rds"

saveRDS(model, paste0(path, filename))
# model 6 outbred remained only
#### RUN Outbred all###########################################
#### #############################################################
#### #################################################################
#load libraries

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = remained$kids,
  hypergamy = remained$hypergamy,
  outbred = remained$outbred,
  returnedkarelia = remained$returnedkarelia,
  sex = remained$sex,
  education = remained$education,
  agriculture = remained$agricult,
  technical = remained$technical,
  factory = remained$factory,
  service = remained$service,
  office = remained$office,
  business = remained$business,
  transport = remained$transport,
  age = remained$age,
  population = remained$log_pop,
  married_after_war = remained$married_after,
  birthplace_id_seq = remained$birthplace_id_seq
)

model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after_war,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0))



path<- (paste0("results/"))
filename <- "model_6_outbred_remained.rds"
saveRDS(model, paste0(path, filename))

# model 7 outbred returned only

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  population = returned$log_pop,
  married_after_war = returned$married_after,
  birthplace_id_seq = returned$birthplace_id_seq
)

model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after_war,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0))



path<- (paste0("results/"))
filename <- "model_7_outbred_returned.rds"
saveRDS(model, paste0(path, filename))
# model 8 outbred married after war only
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  married_after_war = after$married_after,
  birthplace_id_seq = after$birthplace_id_seq
)

model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brk * returnedkarelia,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brk) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,brk=0))



path<- (paste0("results/"))
filename <- "model_8_outbred_after.rds"
saveRDS(model, paste0(path, filename))


################## Model 9 kids returned after war#############################################################################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = remained_after$kids,
  hypergamy = remained_after$hypergamy,
  outbred = remained_after$outbred,
  returnedkarelia = remained_after$returnedkarelia,
  sex = remained_after$sex,
  education = remained_after$education,
  agriculture = remained_after$agricult,
  technical = remained_after$technical,
  factory = remained_after$factory,
  service = remained_after$service,
  office = remained_after$office,
  business = remained_after$business,
  transport = remained_after$transport,
  age = remained_after$age,
  population = remained_after$log_pop,
  married_after = remained_after$married_after,
  birthplace_id_seq = remained_after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0
  ),
  chains=4,cores=4)
path<- (paste0("results/"))
filename <- "Model_9_kids_remained_after.rds"

saveRDS(model, paste0(path, filename))

################## Model 10 kids returned after war#############################################################################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = returned_after$kids,
  hypergamy = returned_after$hypergamy,
  outbred = returned_after$outbred,
  returnedkarelia = returned_after$returnedkarelia,
  sex = returned_after$sex,
  education = returned_after$education,
  agriculture = returned_after$agricult,
  technical = returned_after$technical,
  factory = returned_after$factory,
  service = returned_after$service,
  office = returned_after$office,
  business = returned_after$business,
  transport = returned_after$transport,
  age = returned_after$age,
  population = returned_after$log_pop,
  married_after = returned_after$married_after,
  birthplace_id_seq = returned_after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0),
  chains=4,cores=4)
path<- (paste0("results/"))
filename <- "Model_10_kids_returned_after.rds"

saveRDS(model, paste0(path, filename))

#### Model 11 outbred remained after
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = remained_after$kids,
  hypergamy = remained_after$hypergamy,
  outbred = remained_after$outbred,
  returnedkarelia = remained_after$returnedkarelia,
  sex = remained_after$sex,
  education = remained_after$education,
  agriculture = remained_after$agricult,
  technical = remained_after$technical,
  factory = remained_after$factory,
  service = remained_after$service,
  office = remained_after$office,
  business = remained_after$business,
  transport = remained_after$transport,
  age = remained_after$age,
  population = remained_after$log_pop,
  married_after = remained_after$married_after,
  birthplace_id_seq = remained_after$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0))



path<- (paste0("results/"))
filename <- "model_11_outbred_remained_after.rds"
saveRDS(model, paste0(path, filename))


#### Model 12 outbred returned after
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
                        office,business,transport, birthplaceid,married_after)
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = returned_after$kids,
  hypergamy = returned_after$hypergamy,
  outbred = returned_after$outbred,
  returnedkarelia = returned_after$returnedkarelia,
  sex = returned_after$sex,
  education = returned_after$education,
  agriculture = returned_after$agricult,
  technical = returned_after$technical,
  factory = returned_after$factory,
  service = returned_after$service,
  office = returned_after$office,
  business = returned_after$business,
  transport = returned_after$transport,
  age = returned_after$age,
  population = returned_after$log_pop,
  married_after = returned_after$married_after,
  birthplace_id_seq = returned_after$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0))



path<- (paste0("results/"))
filename <- "model_12_outbred_returned_after.rds"
saveRDS(model, paste0(path, filename))

## Model comparisons code
# Make a correaltion matrix to find subset of data based on colinearity of data, look at the correlations among variables
cor_mat <- cor(m)

# correlation threshold
cor_threshold <- 0.5

# set correlations to 0 if absolute value bigger than threshold
cor_mat <- ifelse(abs(cor_mat) > cor_threshold, 0 , 1)

# generate the lower-triangular matrix with correlations
cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA

options(na.action = "na.fail") 
#model<-glm(HostOut ~ M1F2+RFA96+RFA2006+RelDev+IntRB+ExtRB+Christian+Church+baptize, data=RL_Data, family = binomial)
model<-glm(kids ~   sex + returnedkarelia +
             age +            
             technical + office + business + agricult +     
             transport + factory + service + ..., data=d,
           family = poisson)
modelset<-dredge(model, rank = AICc, trace=FALSE) #subset = (!RelDev | !IntRB))# find out from Dylan how to exclude more correlated  variables from being entered into the same model
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")

























#TOP Models with interactions and new age variable
#######################################################################################################################
#######################################################################################################################
#### Model 19 outbred  all
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after +
      brksex *returnedkarelia *sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0,brksex=0,brkage=0))



path<- (paste0("results/"))
filename <- "model_19_outbred_all_top_model.rds"
saveRDS(model, paste0(path, filename))

#### Model 20 outbred post 1945 all
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  married_after = after$married_after,
  birthplace_id_seq = after$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brksex *returnedkarelia *sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,brksex=0,brkage=0))



path<- (paste0("results/"))
filename <- "model_20_outbred_after_top_model.rds"
saveRDS(model, paste0(path, filename))

#### Model 21 outbred pre 1945 all
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  married_after = before$married_after,
  birthplace_id_seq = before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brksex *returnedkarelia *sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,brksex=0,brkage=0))



path<- (paste0("results/"))
filename <- "model_21_outbred_before_top_model.rds"
saveRDS(model, paste0(path, filename))


## Kids top models Model 22
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after + 
      bobage *outbred *age +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw,bobage, brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=100, warmup=50, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0, bobage=0, brkage=0
  ),
  chains=1,cores=1)

path<- (paste0("results/"))
filename <- "Model_22_kids_all_top_model.rds"

saveRDS(model, paste0(path, filename))

## Kids top models Model 23
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  birthplace_id_seq = after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bobage *outbred *age +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage, brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0, brkage=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_23_kids_after_war_top_model.rds"

saveRDS(model, paste0(path, filename))

## Kids top models Model 24
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  birthplace_id_seq = before$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bobage *outbred *age +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage, brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0, brkage=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_24_kids_before_war_top_model.rds"

saveRDS(model, paste0(path, filename))
######
######
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  married_after = returned$married_after,
  population = returned$log_pop,
  birthplace_id_seq = returned$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bobage *outbred *age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage,bmaw) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0,bmaw=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_25_kids_returned_top_model.rds"

saveRDS(model, paste0(path, filename))


#####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = remained$kids,
  hypergamy = remained$hypergamy,
  outbred = remained$outbred,
  returnedkarelia = remained$returnedkarelia,
  sex = remained$sex,
  education = remained$education,
  agriculture = remained$agricult,
  technical = remained$technical,
  factory = remained$factory,
  service = remained$service,
  office = remained$office,
  business = remained$business,
  transport = remained$transport,
  age = remained$age,
  married_after = remained$married_after,
  population = remained$log_pop,
  birthplace_id_seq = remained$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bobage *outbred *age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage,bmaw) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0,bmaw=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_26_kids_remained_top_model.rds"

saveRDS(model, paste0(path, filename))


#### 
#### ##################### Model 27 kids top returned after
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = returned_after$kids,
  hypergamy = returned_after$hypergamy,
  outbred = returned_after$outbred,
  returnedkarelia = returned_after$returnedkarelia,
  sex = returned_after$sex,
  education = returned_after$education,
  agriculture = returned_after$agricult,
  technical = returned_after$technical,
  factory = returned_after$factory,
  service = returned_after$service,
  office = returned_after$office,
  business = returned_after$business,
  transport = returned_after$transport,
  age = returned_after$age,
  population = returned_after$log_pop,
  birthplace_id_seq = returned_after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bobage *outbred *age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_27_kids_returned_after_top_model.rds"

saveRDS(model, paste0(path, filename))



#### 
#### ##################### Model 28 kids top remained after
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = remained_after$kids,
  hypergamy = remained_after$hypergamy,
  outbred = remained_after$outbred,
  returnedkarelia = remained_after$returnedkarelia,
  sex = remained_after$sex,
  education = remained_after$education,
  agriculture = remained_after$agricult,
  technical = remained_after$technical,
  factory = remained_after$factory,
  service = remained_after$service,
  office = remained_after$office,
  business = remained_after$business,
  transport = remained_after$transport,
  age = remained_after$age,
  population = remained_after$log_pop,
  birthplace_id_seq = remained_after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bobage *outbred *age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,bobage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bobage=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_28_kids_remained_after_top_model.rds"

saveRDS(model, paste0(path, filename))


### last models with additional interactions
### Outbred first
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after +
      bmawrk * married_after * returnedkarelia +
      bmawhyp * married_after * hypergamy +
      bmawsex * married_after * sex +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,brksex,brkage,bmawrk,bmawsex,bmawhyp) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0,brksex=0,
             brkage=0,bmawrk=0,bmawsex=0,bmawhyp=0))



path<- (paste0("results/"))
filename <- "model_30_outbred_all_w_all_interactions.rds"
saveRDS(model, paste0(path, filename))


# Kids model with all interactions
## Kids all interactions model 31
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  married_after = m$married_after,
  birthplace_id_seq = m$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bmawrk * married_after * returnedkarelia +
      bmawsex * married_after * sex +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw, bmawrk,
      bmawsex,brksex,brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0, brkage=0, bmawrk=0,
    bmawsex=0,brksex=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_31_kids_all_w_all_interactions.rds"

saveRDS(model, paste0(path, filename))



### last models with additional interactions
### Outbred first returned only w interactions
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  population = returned$log_pop,
  married_after = returned$married_after,
  birthplace_id_seq = returned$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after +
      bmawhyp * married_after * hypergamy +
      bmawsex * married_after * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,bmawsex,bmawhyp) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0
             ,bmawsex=0,bmawhyp=0))



path<- (paste0("results/"))
filename <- "model_32a_outbred_returned_w_all_interactions.rds"
saveRDS(model, paste0(path, filename))



## Kids retunred with interactions model 33
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  population = returned$log_pop,
  married_after = returned$married_after,
  birthplace_id_seq = returned$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bmawsex * married_after * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw,
      bmawsex) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0,
    bmawsex=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_33_kids_returned_w_all_interactions.rds"

saveRDS(model, paste0(path, filename))

### last models with additional interactions
### Outbred first returned only w interactions
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = remained$kids,
  hypergamy = remained$hypergamy,
  outbred = remained$outbred,
  returnedkarelia = remained$returnedkarelia,
  sex = remained$sex,
  education = remained$education,
  agriculture = remained$agricult,
  technical = remained$technical,
  factory = remained$factory,
  service = remained$service,
  office = remained$office,
  business = remained$business,
  transport = remained$transport,
  age = remained$age,
  population = remained$log_pop,
  married_after = remained$married_after,
  birthplace_id_seq = remained$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      bmaw * married_after +
      bmawhyp * married_after * hypergamy +
      bmawsex * married_after * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,bmawsex,bmawhyp) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,bmaw=0
             ,bmawsex=0,bmawhyp=0))



path<- (paste0("results/"))
filename <- "model_34_outbred_remained_w_all_interactions.rds"
saveRDS(model, paste0(path, filename))



## Kids remained with interactions model 35
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = remained$kids,
  hypergamy = remained$hypergamy,
  outbred = remained$outbred,
  returnedkarelia = remained$returnedkarelia,
  sex = remained$sex,
  education = remained$education,
  agriculture = remained$agricult,
  technical = remained$technical,
  factory = remained$factory,
  service = remained$service,
  office = remained$office,
  business = remained$business,
  transport = remained$transport,
  age = remained$age,
  population = remained$log_pop,
  married_after = remained$married_after,
  birthplace_id_seq = remained$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bmawsex * married_after * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, bmaw,
      bmawsex) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0,
    bmawsex=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_35_kids_remained_w_all_interactions.rds"

saveRDS(model, paste0(path, filename))


##### before and after with interaxtions
### Outbred first
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  married_after = before$married_after,
  birthplace_id_seq = before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,brksex=0,
             brkage=0))



path<- (paste0("results/"))
filename <- "model_36_outbred_before_w_all_interactions.rds"
saveRDS(model, paste0(path, filename))


# Kids model with all interactions
## Kids all interactions model 37
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  married_after = before$married_after,
  birthplace_id_seq = before$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
      brksex,brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, brkage=0,brksex=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_37_kids_before_w_all_interactions.rds"

saveRDS(model, paste0(path, filename))

# Outrbed after with interactions
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  married_after = after$married_after,
  birthplace_id_seq = after$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0,brksex=0,
             brkage=0))



path<- (paste0("results/"))
filename <- "model_38_outbred_after_w_all_interactions.rds"
saveRDS(model, paste0(path, filename))


# Kids model with all interactions
## Kids all interactions model 39
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

m$outbred<- as.numeric(m$outbred)
# split data into returned and remained groups and before and after war
# Returned == 1
returned <- m %>% filter(returnedkarelia==1)
remained <- m %>% filter(returnedkarelia==0)
#married before 1945 or unknown
before <- m %>% filter(married_after==0)
# married after 1945 for sure
after <- m %>% filter(married_after==1)

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

print(nrow(m))

data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  married_after = after$married_after,
  birthplace_id_seq = after$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      brksex *returnedkarelia * sex +
      brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
      brksex,brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, brkage=0,brksex=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_39_kids_after_w_all_interactions.rds"

saveRDS(model, paste0(path, filename))



###############################Do the returned/remined before model without intxs
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned_before$kids,
  hypergamy = returned_before$hypergamy,
  outbred = returned_before$outbred,
  returnedkarelia = returned_before$returnedkarelia,
  sex = returned_before$sex,
  education = returned_before$education,
  agriculture = returned_before$agricult,
  technical = returned_before$technical,
  factory = returned_before$factory,
  service = returned_before$service,
  office = returned_before$office,
  business = returned_before$business,
  transport = returned_before$transport,
  age = returned_before$age,
  population = returned_before$log_pop,
  birthplace_id_seq = returned_before$birthplace_id_seq
)

model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_40_kids_returned_before_no_intxs.rds"

saveRDS(model, paste0(path, filename))




library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned_before$kids,
  hypergamy = returned_before$hypergamy,
  outbred = returned_before$outbred,
  returnedkarelia = returned_before$returnedkarelia,
  sex = returned_before$sex,
  education = returned_before$education,
  agriculture = returned_before$agricult,
  technical = returned_before$technical,
  factory = returned_before$factory,
  service = returned_before$service,
  office = returned_before$office,
  business = returned_before$business,
  transport = returned_before$transport,
  age = returned_before$age,
  population = returned_before$log_pop,
  birthplace_id_seq = returned_before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0))



path<- (paste0("results/"))
filename <- "model_41_outbred_before_returned_no_intxs.rds"
saveRDS(model, paste0(path, filename))




library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = remained_before$kids,
  hypergamy = remained_before$hypergamy,
  outbred = remained_before$outbred,
  returnedkarelia = remained_before$returnedkarelia,
  sex = remained_before$sex,
  education = remained_before$education,
  agriculture = remained_before$agricult,
  technical = remained_before$technical,
  factory = remained_before$factory,
  service = remained_before$service,
  office = remained_before$office,
  business = remained_before$business,
  transport = remained_before$transport,
  age = remained_before$age,
  population = remained_before$log_pop,
  birthplace_id_seq = remained_before$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      brk * returnedkarelia +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, brk, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0, brk=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0
  ),
  chains=4,cores=4)




path<- (paste0("results/"))
filename <- "model_42_kids_before_remained_no_intxs.rds"
saveRDS(model, paste0(path, filename))




library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = remained_before$kids,
  hypergamy = remained_before$hypergamy,
  outbred = remained_before$outbred,
  returnedkarelia = remained_before$returnedkarelia,
  sex = remained_before$sex,
  education = remained_before$education,
  agriculture = remained_before$agricult,
  technical = remained_before$technical,
  factory = remained_before$factory,
  service = remained_before$service,
  office = remained_before$office,
  business = remained_before$business,
  transport = remained_before$transport,
  age = remained_before$age,
  population = remained_before$log_pop,
  birthplace_id_seq = remained_before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,bmaw,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0))



path<- (paste0("results/"))
filename <- "model_43_outbred_before_remained_no_intxs.rds"
saveRDS(model, paste0(path, filename))





library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned_before$kids,
  hypergamy = returned_before$hypergamy,
  outbred = returned_before$outbred,
  returnedkarelia = returned_before$returnedkarelia,
  sex = returned_before$sex,
  education = returned_before$education,
  agriculture = returned_before$agricult,
  technical = returned_before$technical,
  factory = returned_before$factory,
  service = returned_before$service,
  office = returned_before$office,
  business = returned_before$business,
  transport = returned_before$transport,
  age = returned_before$age,
  population = returned_before$log_pop,
  birthplace_id_seq = returned_before$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0
  ),
  chains=4,cores=4)




path<- (paste0("results/"))
filename <- "model_50_kids_before_returned_no_intxs.rds"
saveRDS(model, paste0(path, filename))




library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned_before$kids,
  hypergamy = returned_before$hypergamy,
  outbred = returned_before$outbred,
  returnedkarelia = returned_before$returnedkarelia,
  sex = returned_before$sex,
  education = returned_before$education,
  agriculture = returned_before$agricult,
  technical = returned_before$technical,
  factory = returned_before$factory,
  service = returned_before$service,
  office = returned_before$office,
  business = returned_before$business,
  transport = returned_before$transport,
  age = returned_before$age,
  population = returned_before$log_pop,
  birthplace_id_seq = returned_before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,btrans=0))



path<- (paste0("results/"))
filename <- "model_51_outbred_before_returned_no_intxs.rds"
saveRDS(model, paste0(path, filename))






# missing models kids reurned, kidsbefore, kids after, out before

library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned_before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  birthplace_id_seq = before$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      brkage * age +
      brksex * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, brk, brkage, brksex) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, brk=0, brkage=0, brksex=0
  ),
  chains=4,cores=4)




path<- (paste0("results/"))
filename <- "model_60_kids_before_w_intxs.rds"
saveRDS(model, paste0(path, filename))


# kids after w interactions
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = after$kids,
  hypergamy = after$hypergamy,
  outbred = after$outbred,
  returnedkarelia = after$returnedkarelia,
  sex = after$sex,
  education = after$education,
  agriculture = after$agricult,
  technical = after$technical,
  factory = after$factory,
  service = after$service,
  office = after$office,
  business = after$business,
  transport = after$transport,
  age = after$age,
  population = after$log_pop,
  birthplace_id_seq = after$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      brkage * age +
      brksex * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, brk, brkage, brksex) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, brk=0, brkage=0, brksex=0
  ),
  chains=4,cores=4)




path<- (paste0("results/"))
filename <- "model_61_kids_after_w_intxs.rds"
saveRDS(model, paste0(path, filename))





# kids returned w interactions
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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


data_list <- list(
  kids = returned$kids,
  hypergamy = returned$hypergamy,
  outbred = returned$outbred,
  returnedkarelia = returned$returnedkarelia,
  sex = returned$sex,
  education = returned$education,
  agriculture = returned$agricult,
  technical = returned$technical,
  factory = returned$factory,
  service = returned$service,
  office = returned$office,
  business = returned$business,
  transport = returned$transport,
  age = returned$age,
  married_after = returned$married_after,
  population = returned$log_pop,
  birthplace_id_seq = returned$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      bob * outbred +
      bs * sex +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bmawsex * sex,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop, brk, bmaw, bmawsex) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0, bob=0,  bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0, bmaw=0, bmawsex=0 
  ),
  chains=4,cores=4)




path<- (paste0("results/"))
filename <- "model_62_kids_returned_w_intxs.rds"
saveRDS(model, paste0(path, filename))



# outbred before w intxs
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
m <- readRDS(paste0(path, file))
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
m <- m[complete.cases(m),] # N=26,757

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



data_list <- list(
  kids = before$kids,
  hypergamy = before$hypergamy,
  outbred = before$outbred,
  returnedkarelia = before$returnedkarelia,
  sex = before$sex,
  education = before$education,
  agriculture = before$agricult,
  technical = before$technical,
  factory = before$factory,
  service = before$service,
  office = before$office,
  business = before$business,
  transport = before$transport,
  age = before$age,
  population = before$log_pop,
  married_after = before$married_after,
  birthplace_id_seq = before$birthplace_id_seq
)
model <- map2stan(
  alist(
    outbred ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a + 
      a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      brk * returnedkarelia +
      bs * sex +
      bage * age +
      bpop * population +
      bed * education +
      bag * agriculture +
      btech * technical +
      bfact * factory +
      bserv* service +
      boff * office +
      bbus * business +
      btrans * transport +
      brksex *returnedkarelia *sex +
      brkage * returnedkarelia * age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    
    #priors for all model intercepts
    a ~ dnorm (0,1),
    # priors for all slopes (b terms) in main model
    c(bhyp,brk,bs,bage,bpop,bed,bag,btech,bfact,bserv,boff,bbus,btrans,brksex,brkage) ~ dnorm(0,1)
  ),
  
  # put in priors for the standard deviations for the missing data
  data=data_list, iter=8000, warmup=2000, chains=4, cores=4, control=list(max_treedepth=20),
  start=list(bhyp=0,brk=0,bs=0,bage=0,bpop=0,bed=0,bag=0,btech=0,bfact=0,bserv=0,boff=0,bbus=0,
             btrans=0,brksex=0,brkage=0))



path<- (paste0("results/"))
filename <- "model_70_outbred_before_w_intxs.rds"
saveRDS(model, paste0(path, filename))

data_list <- list(
  kids = m$kids,
  hypergamy = m$hypergamy,
  outbred = m$outbred,
  returnedkarelia = m$returnedkarelia,
  sex = m$sex,
  education = m$education,
  agriculture = m$agricult,
  technical = m$technical,
  factory = m$factory,
  service = m$service,
  office = m$office,
  business = m$business,
  transport = m$transport,
  age = m$age,
  population = m$log_pop,
  birthplace_id_seq = m$birthplace_id_seq
)
model <- map2stan(
  alist(
    kids ~ dpois (lambda),
    # Here is the model with all the predictors
    log(lambda) <- a + a_birthplace[birthplace_id_seq] +
      bhyp * hypergamy +
      #brk * returnedkarelia +
      bs * sex +
      bob *outbred +
      bed *education +
      bag * agriculture +
      btech *technical +
      bfact * factory +
      bserv * service +
      boff * office +
      bbus * business +
      btrans * transport +
      bage * age +
      bpop * population +
      bmaw * married_after +
      bobs * outbred * sex +
      bobhyp * outbred * hypergamy +
      bobage * outbred * age,
    bobmaw * outbred * married_after +
    bmawrk * married_after * returnedkarelia +
    bmawsex * married_after * sex,
    brksex *returnedkarelia * sex +
    brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
      bobs,bobage,bobhyp,bobmaw,bmawrk,bmawsex,brksex,brkage) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0,  brk=0, bob=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0,  bobage=0,bobhyp=0,
    bobs=0,bobmaw=0,bmawrk=0,bmawsex=0,brksex=0,brkage=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_kids_all_w_all_INTX.rds"

saveRDS(model, paste0(path, filename))
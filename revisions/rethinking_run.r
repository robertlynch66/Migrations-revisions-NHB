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
  married_after_war = before$married_after,
  birthplace_id_seq = before$birthplace_id_seq
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
filename <- "model_8a_outbred_before.rds"
saveRDS(model, paste0(path, filename))

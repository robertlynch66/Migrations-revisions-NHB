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
      #bmaw * married_after +
      bobs * outbred * sex +
      bobhyp * outbred * hypergamy +
      bobage * outbred * age,
      #bobmaw * outbred * married_after +
      #bmawrk * married_after * returnedkarelia +
      #bmawsex * married_after * sex,
      #brksex *returnedkarelia * sex +
      #brkage * returnedkarelia*age,
    a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,1),
    
    # priors for all slopes (b terms) in main model
    c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
      bobs,bobage,bobhyp) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),start=list(
    bhyp=0,  brk=0, bob=0, bs=0, bed=0, bag=0, btech=0, bfact=0, bserv=0, boff=0, bbus=0, btrans=0, 
    bage=0, bpop=0,  bobage=0,bobhyp=0,
    bobs=0
  ),
  chains=4,cores=4)

path<- (paste0("results/"))
filename <- "Model_kids_returned_AFTER_w_INTX.rds"

saveRDS(model, paste0(path, filename))


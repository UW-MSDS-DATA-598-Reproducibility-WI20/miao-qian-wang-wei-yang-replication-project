###############################################################################
# Name: simulation_functions.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Defines functions used in the simulations
# The script assumes the following folder structure:
# Scripts are stored in "[project folder]/R"
# Data are stored in "[project folder]/data"
# Results are stored in "[project folder]/results"
###############################################################################


# R version and load packages and install if necessary

# version
# 
# _                           
# platform       x86_64-apple-darwin15.6.0   
# arch           x86_64                      
# os             darwin15.6.0                
# system         x86_64, darwin15.6.0        
# status                                     
# major          3                           
# minor          5.2                         
# year           2018                        
# month          12                          
# day            20                          
# svn rev        75870                       
# language       R                           
# version.string R version 3.5.2 (2018-12-20)
# nickname       Eggshell Igloo   


##### FERTILITY #####

#' Annual probability of birth as a function of calendar year and mother’s age.
#' HIV negative
#' 
#' The birth probability was set to NULL for women younger than 15 years and older than 49 years.
#' Pr(birth)_current year,age =ASFR_nearest year,age *(TFR_current year/TFR_nearest year)
#' current_year is the current year in the simulation, nearest year is the year nearest to the current
#' year for which ASFR are available, age is age of the mother in current year.
#' 
#' @param age (numeric) mother's age in current year
#' @param yr (numeric) current year in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @return (numeric) annual probability of birth.
prob.birth <- function(age,yr,asfr_s,tfr_s){
  
  # If yr is earlier than any of our data, get the first year available.
  
  if(yr<min(tfr_s[,"year"])){yr=min(tfr_s[,"year"])}
  
  # Get total fertility rate for that year
  tfr1 = tfr_s[,"tfr"][tfr_s[,"year"]==yr]
  
  # Get total fertility rate with nearest year (rounds down in case of tie)
  
  row = which.min(abs(asfr_s[,"refyr"]-yr))
  
  tfr2 = asfr_s[row,"tfr"]
  
  # Compute probability of birth according to the mother's age.
  # pr= asfr_nearest_yr * ( tfr1 / tfr2 )
  
  if(age>14 & age<20){ # aged 15-19
    pb=asfr_s[row,"asfr15"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>19 & age<25){ # aged 20-14
    pb=asfr_s[row,"asfr20"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>24 & age<30){ # aged 25-29
    pb=asfr_s[row,"asfr25"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>29 & age<35){ # aged 30-34
    pb=asfr_s[row,"asfr30"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>34 & age<40){ # aged 35-39
    pb=asfr_s[row,"asfr35"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>39 & age<45){ # aged 40-44 
    pb=asfr_s[row,"asfr40"]*(tfr1/tfr2)/1000
    return(pb)
  }
  if(age>44 & age<50){ # aged 45-49
    pb=asfr_s[row,"asfr45"]*(tfr1/tfr2)/1000
    return(pb)
  }
}

#' Annual probability of birth as a function of calendar year and mother’s age.
#' HIV negative
#' 
#' Uses prob.birth to compute annual probability of birth for each years, ages combination
#' The birth probability was set to zero for women younger than 15 years and older than 49 years.
#' 
#' @param ages (numeric) vector mother's ages
#' @param yr (numeric) vector of years in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @return (matrix) annual probability of birth for each age-year
prob.birth.ages.years <- function(ages,years,asfr_s,tfr_s){
  
  ages_as_character <- as.character(ages)
  years_as_character <- as.character(years)
  # Create an empty matrix
  prob.birth.all <- matrix(NA,length(years_as_character),length(ages_as_character) )
  row.names(prob.birth.all) <- years_as_character
  colnames(prob.birth.all) <- ages_as_character
  
  
  
  # Fill the matrix
  for (i in 1:length(years)) { # For each year
    y <- years[i]
    for (j in 1:length(ages)) { # For eacj age
      a <- ages[j]
      res <- prob.birth(a,y,asfr_s,tfr_s) # Compute annual probability of birth
      
      # set probability to zero when the result is NULL (women younger than 15 years and older than 49 years).
      if (length(res)>0 ){prob.birth.all[years_as_character[i],ages_as_character[j]] <- res
      } else {
        prob.birth.all[years_as_character[i],ages_as_character[j]] <- 0
      }
    }
  }
  
  prob.birth.all
  
}

#' Reduction in prob of giving birth due to HIV 
#'
#' Among women aged 15-19 years not on ART, those who were HIV-positive experienced higher ASFRs compared to 
#' HIV-negative women, with the ratio dependent on the percent of 15-19 year old women who were 
#' sexually active; also, among those aged 19, HIV-positive women experienced lower fertility rates 
#' relative to HIV-negative women. We use ratios estimated by Chen and Walker (2010).
#' 
#' Among women over age 19, ART erases half of the fertility decrease caused by HIV/AIDS. 
#' In other words, for women on ART, the ASFR ratios in Table S1 increase by half the difference 
#' from one (one indicating equal ASFRs between HIV-positive and HIV-negative women). 
#' ASFR for 15-19 year olds is not affected by ART.
#' 
#' @param age (numeric) mother's age
#' @param sexactive15 (numeric) percent of females aged 15-19 who are sexually active
#' @param art (integer) either: 0 (not on ART) or 1 (on ART)
#' @return (numeric) reduction in prob of giving birth due to HIV 
fert_hiv <- function(age,sexactive15,art){
  
  if(art==0){ # Not on ART
    
    if(age>14 & age<20){ # aged 15-19
      fert_hiv=2.528-.031*sexactive15
      return(fert_hiv)
    }
    if(age>19 & age<25){ # aged 20-24
      fert_hiv=.765
      return(fert_hiv)
    }
    if(age>24 & age<30){ # aged 25-29
      fert_hiv=.706
      return(fert_hiv)
    }
    if(age>29 & age<35){ # aged 30-34
      fert_hiv=.647
      return(fert_hiv)
    }
    if(age>34 & age<40){ # aged 35-39
      fert_hiv=.588
      return(fert_hiv)
    }
    if(age>39 & age<45){ # aged 40-44
      fert_hiv=.529
      return(fert_hiv)
    }
    if(age>44 & age<50){ # aged 45-49
      fert_hiv=.470
      return(fert_hiv)
    }
  }else{ #On ART
    if(age>14 & age<20){ # aged 15-19
      fert_hiv=2.528-.031*sexactive15
      return(fert_hiv)
    }
    if(age>19 & age<25){ # aged 20-24
      fert_hiv=.8825
      return(fert_hiv)
    }
    if(age>24 & age<30){ # aged 25-29
      fert_hiv=.853
      return(fert_hiv)
    }
    if(age>29 & age<35){ # aged 30-34
      fert_hiv=.824
      return(fert_hiv)
    }
    if(age>34 & age<40){ # aged 35-39
      fert_hiv=.794
      return(fert_hiv)
    }
    if(age>39 & age<45){ # aged 40-44
      fert_hiv=.765
      return(fert_hiv)
    }
    if(age>44 & age<50){ # aged 45-49
      fert_hiv=.735
      return(fert_hiv)
    }
  }
}


#' Reduction in prob of giving birth due to HIV for each art, age combination
#'
#' Uses fert_hiv to compute the reduciton in probability of giving birth due to HIV
#' for each art, ages combination.
#' 
#' @param ages (numeric) vector of mother's ages
#' @param sexactive15 (numeric) percent of females aged 15-19 who are sexually active
#' @param art (integer) vector of values that should be either: 0 (not on ART) or 1 (on ART)
#' @return (matrix) reduction in prob of giving birth due to HIV 
prob.birth.hiv <- function(ages,sexactive15,arts){
  
  # Create an empty matrix
  prob.birth.hiv <- matrix(NA,length(ages),length(arts))
  row.names(prob.birth.hiv) <- ages
  colnames(prob.birth.hiv) <- arts
  
  # Fill the matrix
  for (a in ages) {# For each age
    for (b in arts) {# For each art
      # Compute probability reduction
      res <- fert_hiv(a,sexactive15,b)
      # If the result is not NULL, store the result in the matrix, otherwise, set the value to 0
      if (length(res)>0 ){
        prob.birth.hiv[as.character(a),as.character(b)] <- res
      }else {
        prob.birth.hiv[as.character(a),as.character(b)] <- 0
      }
    }
  }
  prob.birth.hiv
}


##### MORTALITY #####

#' Per birth prob of death
#' 
#' Hogan et al IHME find annual rates of decline, SSA, 1990-2008, no HIV: 0% to 7.4%
#' MMR (no HIV) range, 2008: 53 to 879 per 100,000 births (implies 123 to 1296 in 1990)
#' Blanc et al find MMR 5-10x greater for 45-49 y/o compared to 20-24 y/o
#' 
#' @param mmr0 (numeric) initial MMR
#' @param mmr_dec (numeric) annual percent decline in MMR
#' @param age (integer) mother age
#' @param year (integer) current year in simulation
#' @return (numeric) per birth prob of death
pmatmort_vec <- function(mmr0,mmr_dec,age,year){
  
  # Current MMR
  mmr = mmr0*(1-mmr_dec)^(year-1990)
  
  # Mortality before 1990
  pmm_l25_l1990 <- mmr0
  pmm_g25_l1990 <- mmr0+((age-25)/25)*2*mmr0
  
  # Mortality after 1990
  
  pmm_l25_g1990 <- mmr
  pmm_g25_g1990 <- mmr+((age-25)/25)*2*mmr
  
  # Age categories
  
  l25 <- as.integer(age>=0 & age<25)
  g25 <- as.integer(age>=25)
  
  l0 <- as.integer(age<0)
  
  # Mortality
  
  # after 1990
  pmatmortl1990 = l25*pmm_l25_l1990 + g25*pmm_g25_l1990 + l0*0
  
  # before or during 1990
  pmatmortg1990 = l25*pmm_l25_g1990 + g25*pmm_g25_g1990 + l0*0
  
  if (year>1990) {
    return(pmatmortg1990)
  } else {
    return(pmatmortl1990)
  }
}


#' Annual probability of death, HIV negative women
#' 
#' Time series for the probability of dying between ages 15 and 60 (45q15) 
#' were taken from the Institute for Health Metrics and Evaluation (2010) 
#' for selected countries. To obtain age-specific annual probabilities of death, 
#' the 45q15 for an input “model country” and year in the simulation were matched
#' to the UN model life table (UN Population Division 2012) with the closest 45q15.
#' 
#' @param age (numeric) woman's age
#' @param year (numeric) year in simulation
#' @param mort_s (data.frame) mortality series for the model country, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param matmort (data.frame) Maternal mortality per country and year
#' @param am_cntry (character) model country for adult mortality
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @return (numeric) annual probability of death, HIV negative women.
phivneg.death <- function(age,year,mort_s,adultmort,am_cntry,matmort,u5m_c){
  
  # Get 45q15 from the Institute for Health Metrics and Evaluation for the model country
  #  during the simulation year. If the simulation year is earlier than 1970, get 1970 value.
  if(year<1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==1970]}
  if(year>=1970){q45_15 = mort_s[,"q45_15"][mort_s[,"year"]==year]}
  
  # Get E.0 from the UN model life table with the closest 45q15 (rounds down in case of tie) to that 
  # in Institute for Health Metrics series.
  # E.0 indicates which distribution we should use
  min45 = which.min(abs(adultmort[,"q45_15"]-q45_15))
  e0 = adultmort[min45,"E.0."]
  
  # Compute probability
  
  # For ages 0 to 4, compute as in baby.death.nohiv
  
  if(age<0){return(0)}
  if(year<min( u5m_c$year)){yr=min(u5m_c$year)
  }else{yr=year}
  if(age==0){return(u5m_c$q1_0[u5m_c$year==yr])}
  if(age==1){return(u5m_c$q1_1[u5m_c$year==yr])} 
  if(age==2){return(u5m_c$q1_2[u5m_c$year==yr])} 
  if(age==3){return(u5m_c$q1_3[u5m_c$year==yr])} 
  if(age==4){return(u5m_c$q1_4[u5m_c$year==yr])}
  
  # Compute probability women older than 4
  
  # Ages 5 and 6 interpolate linearly between pr at 4 and 7 
  
  if(age==5){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    q1_4=u5m_c$q1_4[u5m_c$year==yr]
    pd1yr = q1_4-((q1_4-q1_7)/3)
    return(pd1yr)
  }
  if(age==6){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    q1_4=u5m_c$q1_4[u5m_c$year==yr]
    pd1yr = q1_4-(2*((q1_4-q1_7)/3))
    return(pd1yr)
  }
  if(age==7){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7
    return(pd1yr)
  }
  
  # Ages 8 and 9 interpolate linearly between 7 and 10
  
  if(age==8){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    q1_10 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7-((q1_7-q1_10)/3)
    return(pd1yr)
  }
  if(age==9){
    pd5yr = adultmort$q.x.n.[adultmort$age==5 & adultmort$E.0.==e0]
    q1_7 = 1-(1-pd5yr)^(1/5)
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    q1_10 = 1-(1-pd5yr)^(1/5)
    pd1yr = q1_7-(2*(q1_7-q1_10)/3)
    return(pd1yr)  }
  
  
  if(age>9 & age<15){
    pd5yr = adultmort$q.x.n.[adultmort$age==10 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    return(pd1yr)
  }
  if(age>14 & age<20){
    pd5yr = adultmort$q.x.n.[adultmort$age==15 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==1]
    return(pd1yr)
  }
  if(age>19 & age<25){
    pd5yr = adultmort$q.x.n.[adultmort$age==20 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==2]
    return(pd1yr)
  }
  if(age>24 & age<30){
    pd5yr = adultmort$q.x.n.[adultmort$age==25 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==3]
    return(pd1yr)
  }
  if(age>29 & age<35){
    pd5yr = adultmort$q.x.n.[adultmort$age==30 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==4]
    return(pd1yr)
  }
  if(age>34 & age<40){
    pd5yr = adultmort$q.x.n.[adultmort$age==35 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==5]
    return(pd1yr)
  }
  if(age>39 & age<45){
    pd5yr = adultmort$q.x.n.[adultmort$age==40 & adultmort$E.0.==e0 ]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==6]
    return(pd1yr)
  }
  if(age>44 & age<50){
    pd5yr = adultmort$q.x.n.[adultmort$age==45 & adultmort$E.0.==e0]
    pd1yr = 1-(1-pd5yr)^(1/5)
    #pd1yr = pd1yr - matmort[,paste("X",year,sep="")][matmort$Country==am_cntry & matmort$Agegroup==7]
    return(pd1yr)  
  }
  
  # For ages older than 49, return 0
  
  if(age>49){return(0)}
}

#' Annual probability of death, HIV negative women for each age year combination
#' 
#' Uses phivneg.death to compute annual probability of death for each age-year combination
#' 
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param mort_s (data.frame) mortality series for the model country, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param matmort (data.frame) Maternal mortality
#' @param am_cntry (character) model country for adult mortality
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @param prob.birth.all (matrix) annual probability of birth for each age-year. As provided by `prob.birth.ages.years`
#' @param mmr0 (numeric) initial MMR
#' @param mmr_dec (numeric) annual percent decline in MMR
#' @return (matrix) annual probability of death, HIV negative women (years x ages)
phivneg.death.ages.years <- function(ages,years,mort_s,adultmort,am_cntry,matmort,u5m_c,prob.birth.all,mmr0,mmr_dec){
  
  prob.death.all <- matrix(NA,length(years),length(ages))
  row.names(prob.death.all) <- years
  colnames(prob.death.all) <- ages
  #head(prob.death.all)
  for (y in years) {
    for (a in ages) {
      ych <- as.character(y)
      ach <- as.character(a)
      res <- phivneg.death(a,y,mort_s,adultmort,am_cntry,matmort,u5m_c)
      ann.prob.death.after.birth <- prob.birth.all[ych,ach]*pmatmort_vec(mmr0,mmr_dec,a,y)
      res2 <- res - ann.prob.death.after.birth
      
      if (length(res2)>0 ){
        prob.death.all[ych,ach] <- res2
      }
      else {
        prob.death.all[ych,ach] <- 0
      }
    }
  }
  
  prob.death.all
  
}

#' Probability of death HIV positive adults not in ART. Cumulative mortality reported in Walker, Hill, and Zhao (2012)

aidsmort <- c(0.01,0.02020202,0.041237113,0.053763441,0.079545455,0.098765432,0.123287671,0.15625,0.185185185,0.204545455,0.228571429,	0.296296296,0.263157895,0.357142857,0.333333333,0.333333333,0.5,0.5)
aidsmort <- c(aidsmort,rep(1,50))

#' Probability of death HIV positive adults not in ART
#' 
#' The annual probability of death for HIV-positive women who were not on ART 
#' was based on cumulative mortality reported in Walker, Hill, and Zhao (2012)
#'
#' @param hiv_date (integer) year of HIV infection
#' @param year (integer) current year in simulation 
#' @return (numeric) Probability of death HIV positive adults not in ART
phiv.death <- function(hiv_date, year){
  
  
  
  if(year<hiv_date){
    return("error: year<hiv_date")
  }
  
  duration <- year - hiv_date
  
  #Assume no one survives more than 18 yrs with HIV
  if(duration>17){
    pd <- 1
  }else{
    pd <- aidsmort[duration+1]
  }
  return(pd)
}

# Weibull distribution transition probability
weib.tp <- function(t,scale,shape){
  tp = 1 - exp(-((1/scale)*t^shape - (1/scale)*(t-1)^shape))
  return(tp)
}

shape=1.6
t.steps <- 1:100
weib.tp.hvl.cd450.yg1 <- weib.tp(t.steps,13.7,shape)
weib.tp.hvl.cd4100.yg1 <- weib.tp(t.steps,16,shape)
weib.tp.hvl.cd4200.yg1 <- weib.tp(t.steps,16.9,shape)
weib.tp.hvl.cd4350.yg1 <- weib.tp(t.steps,23.3,shape)
weib.tp.hvl.cd4g350.yg1 <- weib.tp(t.steps,33.3,shape)

weib.tp.lvl.cd450.yg1 <- weib.tp(t.steps,24.4,shape)
weib.tp.lvl.cd4100.yg1 <- weib.tp(t.steps,28.4,shape)
weib.tp.lvl.cd4200.yg1 <- weib.tp(t.steps,30.1,shape)
weib.tp.lvl.cd4350.yg1 <- weib.tp(t.steps,41.4,shape)
weib.tp.lvl.cd4g350.yg1 <- weib.tp(t.steps,59.1,shape)

#' Probability of death for HIV pos adults on ART
#' 
#' HIV-positive women on ART faced an annual probability of death that 
#' was a function of CD4 count at ART initiation, presence or absence of 
#' symptoms at baseline, and time since initiation. The function was taken 
#' from the “medium” scenario published by Hallett et al. (2008). 
#' Women were assigned to “symptomatic” or “non-symptomatic” with probability 0.5, 
#' based on Braitstein et al. (2006).
#' 
#' @param year (integer) Current simulation year
#' @param cd4 (numeric) CD4 count at ART initiation
#' @param  art_date (integer) year of ART initiation
#' @return (numeric) probability of death for HIV positive adults.
art.surv.vec <- function(year,cd4,art_date){
  
  # Shape of the Weibull distribution
  shape=1.6
  
  # pacients have 50% probability of having high viral load ("symptomatic")
  # ART-LINC ART-CC Lancet 2006 50% patients have high viral load
  
  hvl <- as.integer(runif(length(cd4))<0.5)
  
  # If a patient doesnot have high viral load, she has a low viral load. ("non-symptomatic")
  lvl <- 1 - hvl
  
  # Time since ART initiation
  t = as.integer(year-art_date)
  
  # Annual probability of death is a function of CD4 count at ART initiation, 
  # presence or ausence of symptoms at baseline and time since initiation
  
  # Probabilities for "symptomatic" 
  
  # CD4 < 50
  
  # Computations are similar for other intervals of CD4
  
  # Proababiliy during the first year after ART
  
  hvl.cd450.y1 <- .109 
  
  # select only cases with the right amount of CD4 and one year since ART initiation 
  
  cd450.select <- cd4<50
  y1 <- t==1
  
  cd450.y1.select <- cd450.select & y1 #ifelse(cd4<50 & t==1,1,0) 
  
  hvl.cd450.y1.select <- hvl*cd450.y1.select
  
  # Use Weibull distribution to compute probability after the first year
  
  
  hvl.cd450.yg1 <- weib.tp.hvl.cd450.yg1[t]
  
  # select only cases with the right amount of CD4 and more than one year since ART initiation 
  
  yg1 <- t>1
  
  cd450.yg1.select <- cd450.select & yg1 #ifelse(cd4<50 & t>1,1,0)
  
  hvl.cd450.yg1.select <- hvl* cd450.yg1.select
  
  # CD4 in  [50,100)
  
  hvl.cd4100.y1 <- .67
  
  cd4100.select <- cd4>=50 & cd4<100
  
  cd4100.y1.select <- cd4100.select & y1
  
  hvl.cd4100.y1.select <- hvl*cd4100.y1.select #ifelse(cd4>=50 & cd4<100 & t==1,1,0)
  hvl.cd4100.yg1 <- weib.tp.hvl.cd4100.yg1[t]
  cd4100.yg1.select <- cd4100.select & yg1
  hvl.cd4100.yg1.select <- hvl*cd4100.yg1.select#ifelse(cd4>=50 & cd4<100 & t>1,1,0)
  
  # CD4 in  [100,200)
  
  hvl.cd4200.y1 <- .46
  
  cd4200.select <- cd4>=100 & cd4<200
  
  cd4200.y1.select <- cd4200.select & y1
  
  hvl.cd4200.y1.select <- hvl*cd4200.y1.select
  
  #hvl.cd4200.y1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
  hvl.cd4200.yg1 <- weib.tp.hvl.cd4200.yg1[t]
  
  cd4200.yg1.select <- cd4200.select & yg1
  hvl.cd4200.yg1.select <- hvl*cd4200.yg1.select
  
  #hvl.cd4200.yg1.select <- hvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
  
  # CD4 in  [200,350)
  
  hvl.cd4350.y1 <- .17
  
  cd4350.select <- cd4>=200 & cd4<350
  
  cd4350.y1.select <- cd4350.select & y1
  
  hvl.cd4350.y1.select <- hvl*cd4350.y1.select
  
  #hvl.cd4350.y1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
  hvl.cd4350.yg1 <- weib.tp.hvl.cd4350.yg1[t]
  
  cd4350.yg1.select <- cd4350.select & yg1
  hvl.cd4350.yg1.select <- hvl*cd4350.yg1.select
  
  #hvl.cd4350.yg1.select <- hvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
  
  # CD4 >= 350
  
  hvl.cd4g350.y1 <- .17
  
  cd4g350.select <- cd4>=350
  
  cd4g350.y1.select <- cd4g350.select & y1
  
  hvl.cd4g350.y1.select <- hvl*cd4g350.y1.select
  
  
  #hvl.cd4g350.y1.select <- hvl*ifelse(cd4>=350 & t==1,1,0)
  hvl.cd4g350.yg1 <- weib.tp.hvl.cd4g350.yg1[t]
  
  cd4g350.yg1.select <- cd4g350.select & yg1
  hvl.cd4g350.yg1.select <- hvl*cd4g350.yg1.select
  
  
  #hvl.cd4g350.yg1.select <- hvl*ifelse(cd4>=350 & t>1,1,0)
  
  # Probabilities for "non-symptomatic" 
  
  # Computations are similar as in "symptomatic"
  
  # CD4 < 50
  
  lvl.cd450.y1 <- .109
  
  lvl.cd450.y1.select <- lvl*cd450.y1.select
  
  #lvl.cd450.y1.select <- lvl*ifelse(cd4<50 & t==1,1,0)
  lvl.cd450.yg1 <- weib.tp.lvl.cd450.yg1[t]
  
  lvl.cd450.yg1.select <- lvl*cd450.yg1.select
  
  #lvl.cd450.yg1.select <- lvl*ifelse(cd4<50 & t>1,1,0)
  
  # CD4 in  [50,100)
  
  lvl.cd4100.y1 <- .67
  
  lvl.cd4100.y1.select <- lvl*cd4100.y1.select
  
  #lvl.cd4100.y1.select <- lvl*ifelse(cd4>=50 & cd4<100 & t==1,1,0)
  lvl.cd4100.yg1 <- weib.tp.lvl.cd4100.yg1[t]
  
  lvl.cd4100.yg1.select <- lvl*cd4100.yg1.select
  
  #lvl.cd4100.yg1.select <- lvl*ifelse(cd4>=50 & cd4<100 & t>1,1,0)
  
  # CD4 in  [100,200)
  
  lvl.cd4200.y1 <- .46
  
  lvl.cd4200.y1.select <- lvl*cd4200.y1.select
  
  #lvl.cd4200.y1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t==1,1,0)
  lvl.cd4200.yg1 <- weib.tp.lvl.cd4200.yg1[t]
  
  lvl.cd4200.yg1.select <- lvl*cd4200.yg1.select
  
  #lvl.cd4200.yg1.select <- lvl*ifelse(cd4>=100 & cd4<200 & t>1,1,0)
  
  # CD4 in  [200,350)
  
  lvl.cd4350.y1 <- .17
  
  lvl.cd4350.y1.select <- lvl*cd4350.y1.select
  
  #lvl.cd4350.y1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t==1,1,0)
  lvl.cd4350.yg1 <- weib.tp.lvl.cd4350.yg1[t]
  
  lvl.cd4350.yg1.select <- lvl*cd4350.yg1.select
  
  #lvl.cd4350.yg1.select <- lvl*ifelse(cd4>=200 & cd4<350 & t>1,1,0)
  
  # CD4 >= 350
  
  lvl.cd4g350.y1 <- .17
  
  lvl.cd4g350.y1.select <- lvl*cd4g350.y1.select
  
  #lvl.cd4g350.y1.select <- lvl*ifelse(cd4>=350 & t==1,1,0)
  lvl.cd4g350.yg1 <- weib.tp.lvl.cd4g350.yg1[t]
  
  lvl.cd4g350.yg1.select <- lvl*cd4g350.yg1.select
  
  #lvl.cd4g350.yg1.select <- lvl*ifelse(cd4>=350 & t>1,1,0)
  
  # Each block above computes a vector of probabilities and a vector that can be used to select which
  # cases fit in a given category (for example CD4 <50, t==1 and lvl). So, to get the probabilities for each 
  # case we multiply the selection vector by the probability vector in each category and then summ the results
  # obtained from all the categories.
  
  return(  hvl.cd450.y1.select*hvl.cd450.y1 + hvl.cd450.yg1.select*hvl.cd450.yg1+
             hvl.cd4100.y1.select*hvl.cd4100.y1 + hvl.cd4100.yg1.select*hvl.cd4100.yg1+
             hvl.cd4200.y1.select*hvl.cd4200.y1 + hvl.cd4200.yg1.select*hvl.cd4200.yg1+
             hvl.cd4350.y1.select*hvl.cd4350.y1 + hvl.cd4350.yg1.select*hvl.cd4350.yg1+
             hvl.cd4g350.y1.select*hvl.cd4g350.y1 + hvl.cd4g350.yg1.select*hvl.cd4g350.yg1+
             lvl.cd450.y1.select*lvl.cd450.y1 + lvl.cd450.yg1.select*lvl.cd450.yg1+
             lvl.cd4100.y1.select*lvl.cd4100.y1 + lvl.cd4100.yg1.select*lvl.cd4100.yg1+
             lvl.cd4200.y1.select*lvl.cd4200.y1 + lvl.cd4200.yg1.select*lvl.cd4200.yg1+
             lvl.cd4350.y1.select*lvl.cd4350.y1 + lvl.cd4350.yg1.select*lvl.cd4350.yg1+
             lvl.cd4g350.y1.select*lvl.cd4g350.y1 + lvl.cd4g350.yg1.select*lvl.cd4g350.yg1
  )
}


#' Annual probability of death for HIV-negative children for 
#' each age-year combination
#' 
#' Uses the time series for 5q0 and 1q0 estimates from the UN Inter-agency 
#' Group for Child Mortality Estimation (IGME) (2012) for one country.
#' 
#' @param ages (numeric) vector of child's ages
#' @param yr (numeric) vector of years in simulation
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality Estimation (UN IGME) (2012) for one country
#' @return (matrix) annual probability of death for  HIV-negative children for 
#' each age and year combination
baby.death.nohiv <- function(ages,years,u5m_c){
  
  # Create empty matrix: years x ages
  baby.death.nohiv <- matrix(NA,length(years),length(ages) )
  row.names(baby.death.nohiv) <- years
  colnames(baby.death.nohiv) <- ages
  
  # Fill the matrix
  for (y in years) { # For each year
    for (a in ages) { # For each age
      # if the year is previous to our earliest year of u5m_c, get the first year available
      if(y<min( u5m_c$year)){
        yr=min(u5m_c$year)
      }else{
        yr=y
      }
      
      # ages less than 0 and grater than 4 have 0 probablity
      if(a<0){baby.death.nohiv[as.character(y),as.character(a)]=0}
      if(a>4){baby.death.nohiv[as.character(y),as.character(a)]=0}
      
      # Age 0: use 1q0 ratio
      if(a==0){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_0[u5m_c$year==yr]}
      
      # Age 1: use 1q1 ratio
      if(a==1){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_1[u5m_c$year==yr]}   
      
      # Age 2: use 1q2 ratio
      if(a==2){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_2[u5m_c$year==yr]}   
      
      # Age 3: use 1q3 ratio
      if(a==3){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_3[u5m_c$year==yr]}   
      
      # Age 4: use 1q4 ratio
      if(a==4){baby.death.nohiv[as.character(y),as.character(a)]=u5m_c$q1_4[u5m_c$year==yr]}   
    }
  }
  
  baby.death.nohiv  
}



#' Probability of death for HIV positive children
#' 
#' Using cumulative child mortality due to AIDS from Walker, Hill, and Zhao (2012).
#' Probability at ages less than 0 and greater than 4 is zero.
#' 
#' @param ages (integer) vector of children ages
#' @return (numeric) Probability of death for HIV positive children
baby.death.hiv <- function(ages){
  
  
  
  # cumulative child mortality due to AIDS from Walker, Hill, and Zhao (2012)
  
  hivchild_mort = c(0.376,  0.2019,	0.1184,	0.07061,	0.0588, 0.0588,0.0588)  
  
  # Get probabilities
  
  baby.death.hiv <- hivchild_mort[ages+1]
  
  # Other ages get 0 prob
  
  baby.death.hiv[is.na(baby.death.hiv)] <- 0
  
  
  
  baby.death.hiv
  
}


##### HIV INFECTION #####

#' Annual probability of HIV infection
#' 
#' We start selecting the incidence value for the simulation year from 
#' the incidence values provided in hivinc_s (these are the values for a specific country,taken from 
#' Dan Hogan and based on data from ANC clinics, 1970-2015. Then, age-specific incidence is obtained using
#' relative age incidence from P Heuveline. Finally, age-specific incidence is adjusted by age structure so 
#' that overall incidence matches Hogan & Salomon
#'
#' @param age (integer) current age
#' @param year (integer) current year in simulation
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param inc2529_denom (numeric) denominator to compute incidence for the reference group: ages 25-29 
#' @return (numeric) Annual probability of HIV infection
prob.hiv <- function(age,year,hivinc_s,inc2529_denom){
  
  # Prob 0 before 1975
  if(year<1975){
    return(0)
  }else{
    
    # Get the incidence value for current year
    hivcol = (year-1970)+2
    hivinc = hivinc_s[hivcol]
    
    # Compute probability at specific ages.
    
    # p(HIV=positive|age group,year)=(incidence(year)*relative incidence age group)/sum(relative incidence age group * fraction of age gorup in population)
    
    # First compute incidence for the reference group: ages 25-29 
    
    inc2529=hivinc/inc2529_denom
    
    # Now, use use age-specific HIV incidence ratios from Heuveline (2003) to compute risk at each age group
    
    # Ages 15 to 19
    if(age>14&age<20){
      hrisk=.594*inc2529
      return(hrisk)
    }
    
    # Ages 20 to 24
    if(age>19&age<25){
      hrisk=1.325*inc2529
      return(hrisk)
    }  
    
    # Ages 25 to 29
    if(age>24&age<30){
      hrisk=inc2529
      return(hrisk)
    }  
    
    # Ages 30 to 34
    if(age>29&age<35){
      hrisk=.752*inc2529
      return(hrisk)
    }
    
    # Ages 35 to 39
    if(age>34&age<40){
      hrisk=.635*inc2529
      return(hrisk)
    }	
    
    # Ages 40 to 44
    if(age>39&age<45){
      hrisk=.551*inc2529
      return(hrisk)
    }	
    # Ages 45 to 49
    if(age>44&age<50){
      hrisk=.356*inc2529
      return(hrisk)
    }
    
    # People older than 49, have probability 0 
    if(age>49){
      hrisk=0
      return(hrisk)
    }	
  }
}


#' Annual probability of HIV infection for each age year combination
#' 
#' Uses phivneg.death to compute annual probability of death for each age-year combination
#' 
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param c15 (integer) number of people in the simulation between 15 and 19 years
#' @param c20 (integer) number of people in the simulation between 20 and 24 years
#' @param c25 (integer) number of people in the simulation between 25 and 29 years
#' @param c30 (integer) number of people in the simulation between 30 and 34 years
#' @param c35 (integer) number of people in the simulation between 35 and 39 years
#' @param c40 (integer) number of people in the simulation between 40 and 44 years
#' @param c45 (integer) number of people in the simulation between 45 and 49 years
#' @return (matrix) (length(ages) x length(years)) Annual probability of HIV infection.
prob.hiv.ages.years <- function(ages,years,hivinc_s,c15,c20,c25,c30,c35,c40,c45){
  
  # Create empty matrix (length(ages) x length(years))
  
  prob.hiv.vec <- matrix(NA,length(ages),length(years))
  row.names(prob.hiv.vec) <- ages
  colnames(prob.hiv.vec) <- years
  
  normalization <- c15+c20+c25+c30+c35+c40+c45
  
  inc2529_denom=(.594*(c15/(normalization))
                  +1.325*(c20/(normalization))
                  +(c25/(normalization))
                  +.752*(c30/(normalization))
                  +.635*(c35/(normalization))
                  +.551*(c40/(normalization))
                  +.356*(c45/(normalization))
  )
  
  # Compute probability of HIV for each age an year. 
  for (a in ages) {
    for (y in years) {
      
      # probability of HIV
      res <- as.numeric(prob.hiv(a,y,hivinc_s,inc2529_denom))
      
      
      if (length(res)>0 ){
        prob.hiv.vec[as.character(a),as.character(y)] <- res
      }else { # If we don't get a result, that means that the age/year is outside our range, assign probability 0
        prob.hiv.vec[as.character(a),as.character(y)] <- 0
      }
    }
  }  
  
  prob.hiv.vec  
}


#' Probability of mother to child transmission of HIV
#'
#' Probability of mother-to-child transmission of HIV was taken from Stover et al. (2008). 
#' Transmission depends on breastfeeding duration and ART. Using estimates for single-dose nevirapine.
#' 
#' @param art (integer) 0 if not in ART 1 if in ART
#' @param bfeed (integer) breastfeeding duration (months) in the general population (must be 6, 12 or 18)
#' @return (numeric) Probability of mother to child transmission
vert_trans <- function(art,bfeed){
  if(bfeed==6 & art==0){
    vt=.23
    return(vt)}
  if(bfeed==6 & art==1){
    vt=.17
    return(vt)}
  if(bfeed==12 & art==0){
    vt=.305
    return(vt)}
  if(bfeed==12 & art==1){
    vt=.215
    return(vt)}
  if(bfeed==18 & art==0){
    vt=.35
    return(vt)}
  if(bfeed==18 & art==1){
    vt=.26
    return(vt)}
}


#' CD4 progression
#' 
#' square root of CD4 is assumed to decline linearly over time. Parameters
#' derived from Hallett (2008) 
#' 
#' @param cd4 (numeric) initial CD4 count
#' @param cd4dec (numeric) CD4 decline per year
#' @param hivdate (integer) year of HIV infection
#' @param year (integer) current year in simulation
#' @return (numeric) updated CD4 count
cd4.prog <- function(cd4,cd4dec, hivdate,year){
  a = cd4^(.5)
  b = cd4dec
  ts = year-hivdate	
  cd4new = (a-b*ts)^2
  return(cd4new)
}

##### SIMULATION #####


#' Date of birth for initial age structure.
#' 
#' Dates are computed for the 50 years previous to the beginning of the simulation.
#' We start with initialpop and each year population grows according to the growth rate
#' 
#' @param growth (numeric) yearly population growth rate
#' @param initialpop (integer) initial population
#' @param yrstart (numeric) year to start the simulation
#' @return (numeric) date of birth
initial.DOBs <- function(growth,initialpop,yrstart){
  
  # Compute total population for each year
  
  # ratios <- (1+growth) ^ (0:49)
  # 
  # population = ratios*initialpop
  # 
  # # Compute DOB for each year, starting at 1897
  # 
  # 
  # dobs <- rep((yrstart-50+1):yrstart,times=as.integer(population))
  # 
  dobs = rep(yrstart-15, initialpop)
  dobs  
  
}

#' Create empty population matrix of women
#' 
#' Creates an empty matrix of length(dobs) women
#' 
#' @param dobs (numeric) vector of years of birth
#' @returm (matrix) empty population matrix with women only
women.empty.matrix <- function(dobs){
  
  w <- matrix(NA,length(dobs),20)
  colnames(w) <- c("id", "momid","dob","age","ceb", "hiv","hiv_date","death_date","hivdeath","cd4","cd4dec","art","art_date","art_e","momage","momhiv","male","cd","dead","birthlast")
  w[,1] <- c(1:length(dobs))
  w [,"ceb"]=0
  w[,"hiv"]=0
  w[,"hiv_date"]=NA
  w[,"hivdeath"]=0 
  w[,"art"]=0 
  w[,"art_date"]=NA
  w[,"art_e"]=NA
  w[,"dob"] = dobs
  w[,"momid"] = NA
  w[,"male"] = 0
  w[,"cd"]=0
  w[,"dead"]=0
  w[,"birthlast"]=0
  
  w
  
}


#' Empty matrix for birth counts by age group
#' 
#' @param yrstart (numeric) simulation start year
#' @param yrend (numeric) simulation end year
#' @return (matrix) empty matrix to store birth counts for each age group
birth.counts.by.age.empty.matrix <- function(yrstart,yrend){
  
  # The matrix has as many rows as the length of the simulation times 7 age groups
  
  births.age.yr <- matrix(NA, (yrend-yrstart+1)*7,4)
  colnames(births.age.yr) <- c("year","agegrp","births","women")
  
  # Fill years
  
  b<-vector()
  for(j in yrstart:yrend){
    a <- rep(j,7)
    b <- c(b,a)
  }
  births.age.yr[,"year"] <- b
  # Fill age groups
  
  c <- seq(15,45,5)
  d <- rep(c,length(births.age.yr[,1])/7)
  births.age.yr[,"agegrp"] <- d
  
  births.age.yr
}


#' Updates birth count by age for one year
#' 
#' @param births.age.yr (matrix) Matrix of birth counts by age and year, 
#' like the one provided by `birth.counts.by.age.empty.matrix`
#' @param nextbabies (matrix) population matrix of newborns, like the one provided by `next.babies`
#' @param yr (integer) current year in simulation
#' @param c15 (integer) number of people in the simulation between 15 and 19 years
#' @param c20 (integer) number of people in the simulation between 20 and 24 years
#' @param c25 (integer) number of people in the simulation between 25 and 29 years
#' @param c30 (integer) number of people in the simulation between 30 and 34 years
#' @param c35 (integer) number of people in the simulation between 35 and 39 years
#' @param c40 (integer) number of people in the simulation between 40 and 44 years
#' @param c45 (integer) number of people in the simulation between 45 and 49 years
#' @return (matrix) Matrix of birth counts by age and year
update.birth.counts.by.age <- function(births.age.yr,nextbabies,yr,c15,c20,c25,c30,c35,c40,c45){
  
  # Compute number of births per mother age category and store in births.age.yr
  
  # Ages 15-19
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==15] <-sum(nextbabies[,"momage"]<20 & nextbabies[,"momage"]>14)
  
  # Ages 20-24
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==20] <-sum(nextbabies[,"momage"]<25 & nextbabies[,"momage"]>19)
  
  # Ages 25-29
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==25] <-sum(nextbabies[,"momage"]<30 & nextbabies[,"momage"]>24)
  
  # Ages 30-34
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==30] <-sum(nextbabies[,"momage"]<35 & nextbabies[,"momage"]>29)
  
  # Ages 35-39
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==35] <-sum(nextbabies[,"momage"]<40 & nextbabies[,"momage"]>34)
  
  # Ages 40-44
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==40] <-sum(nextbabies[,"momage"]<45 & nextbabies[,"momage"]>39)
  
  # Ages 45-49
  births.age.yr[,"births"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==45] <-sum(nextbabies[,"momage"]<50 & nextbabies[,"momage"]>44)
  
  
  # Update number of women per category group
  
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==15] <-c15
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==20] <-c20
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==25] <-c25
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==30] <-c30
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==35] <-c35
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==40] <-c40
  births.age.yr[,"women"][births.age.yr[,"year"]==yr & births.age.yr[,"agegrp"]==45] <-c45
  
  births.age.yr
}

#' Empty matrix for birth counts by HIV status
#' 
#' @param yrstart (numeric) simulation start year
#' @param yrend (numeric) simulation end year
#' @return (matrix) empty matrix to store birth counts for each HIV status
birth.counts.by.hiv.status.empty.matrix <- function(yrstart,yrend){
  
  # The matrix has as one row per year
  
  hivbirths.momshiv <- matrix(NA, (yrend-yrstart+1),3)
  
  colnames(hivbirths.momshiv) <- c("year","birthpos","birthmompos")
  
  # Fill the years
  
  hivbirths.momshiv[,"year"]<-seq(yrstart,yrend,1)
  
  
  hivbirths.momshiv
  
}

#' Updates birth count by HIV status for one year
#' 
#' @param hivbirths.momshiv (matrix) Matrix of birth counts by HIV status and year, 
#' like the one provided by `birth.counts.by.hiv.status.empty.matrix`
#' @param nextbabies (matrix) population matrix of newborns, like the one provided by `next.babies`
#' @param yr (integer) current year in simulation
#' @return (matrix) Matrix of birth counts by HIV status and year
update.birth.counts.by.hiv.status <- function(hivbirths.momshiv,nextbabies,yr){
  
  # Update birth counts for HIV positive moms.
  
  hivbirths.momshiv[,"birthmompos"][hivbirths.momshiv[,"year"]==yr]<-sum(nextbabies[,"momhiv"]==1)
  
  # HIV positive births tracker for realized VT
  hivbirths.momshiv[,"birthpos"][hivbirths.momshiv[,"year"]==yr]<-sum(nextbabies[,"hiv"]==1)
  
  hivbirths.momshiv
  
}
#' Counts the number of people in each age group
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) matrix of population
#' @returm (numeric) named vector of counts for each group, names define the groups
count.women.age.groups <- function(yr,w){
  
  # Initialize counts to 0
  
  c15=0
  c20=0
  c25=0
  c30=0
  c35=0
  c40=0
  c45=0
  
  # For each age category, count how many people are not dead in that age category 
  
  not_death <- is.na(w[,"death_date"]) | (!is.na(w[,"death_date"]) &  yr<w[,"death_date"])
  
  women <- w[,"male"]==0
  
  # Ages 15-19
  
  c15 =   sum( not_death & yr-w[,"dob"]<20 & yr-w[,"dob"]>14 & women ) 
  
  # Ages 20-24
  
  c20 =   sum( not_death & yr-w[,"dob"]<25 & yr-w[,"dob"]>19 & women )
  
  # Ages 25-29
  
  c25 =   sum( not_death & yr-w[,"dob"]<30 & yr-w[,"dob"]>24 & women )
  
  # Ages 30-34
  
  c30 =   sum( not_death & yr-w[,"dob"]<35 & yr-w[,"dob"]>29 & women )
  
  # Ages 35-39
  
  c35 =   sum( not_death & yr-w[,"dob"]<40 & yr-w[,"dob"]>34 & women )
  
  # Ages 40-44
  
  c40 =   sum( not_death & yr-w[,"dob"]<45 & yr-w[,"dob"]>39 & women )
  
  # Ages 45-49
  
  c45 =   sum( not_death & yr-w[,"dob"]<50 & yr-w[,"dob"]>44 & women )
  
  # Return vector with counts
  
  c(c15=c15,c20=c20,c25=c25,c30=c30,c35=c35,c40=c40,c45=c45)
}


#' Updates women age
#' 
#' Current age = current year - year of birth
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @return (matrix) Population matrix with ages updated for the current year
update.women.age <- function(yr,w){
  
  # current age = current year - year of birth
  
  w[,"age"] = yr-w[,"dob"]
  
  w
  
}

#' Randomly determine if a women has a child this year according to her probability 
#' of giving birth this year.
#' 
#' 
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @param prob.birth.all (matrix) Annual probability of birth as a function of calendar 
#' year and mother’s age. HIV negative. As given by `prob.birth.ages.years`.
#' @param prob.birth.hiv (matrix) Reduction in prob of giving birth due to HIV for each art, age combination. As provided by `prob.birth.hiv`
#' @param ages.as.char (character) Same as w[,"age"] but as character. For performance.
#' @return (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE).
new.babies <- function(yr,w,prob.birth.all,prob.birth.hiv,ages.as.char){
  
  #ages.as.char <- w[,"age"]
  
  # Get probability of birth this year for each woman in w. HIV negative
  prob.birth.thisyear <- prob.birth.all[as.character(yr),ages.as.char]
  
  # Compute probability of birth thi yeat for each woman. HIV positive in ART
  prob.birth.thisyear.hiv.art <- prob.birth.thisyear*prob.birth.hiv[ages.as.char,"1"]
  
  # Compute probability of birth thi yeat for each woman. HIV positive not in ART
  prob.birth.thisyear.hiv.noart <- prob.birth.thisyear*prob.birth.hiv[ages.as.char,"0"]
  
  # Combine the three vectors above to get the probability of birth for each woman
  
  prob.birth.thisyear.adj <- (1-w[,"male"])*# If male, prob 0
    (prob.birth.thisyear*(1-w[,"hiv"])*is.na(w[,"death_date"]) + # Woman HIV negative
       (1-w[,"male"])*prob.birth.thisyear.hiv.art*w[,'art']*is.na(w[,"death_date"]) + # Woman HIV positive in ART
       (1-w[,"male"])*prob.birth.thisyear.hiv.noart*(1-w[,'art'])*w[,'hiv'])*is.na(w[,"death_date"])  # Woman HIV positive not in ART
  
  #  prob.birth.thisyear.adj <- prob.birth.thisyear.adj-prob.birth.thisyear.adj*(w[,"birthlast"])
  
  # Randomly create vector of TRUE (birth) or FALSE (no birth), according to the probability of birth of each
  # individual.
  
  
  newbaby <- runif(nrow(w))<prob.birth.thisyear.adj
  
  newbaby
}


#' Creates new people matrix for newborns
#' 
#' Probability of being male is 102.5/202.5
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) Population matrix
#' @param newbaby (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE). As computed by `new.babies`
#' @return (matrix) Population matrix
next.babies <- function(yr,w,newbaby){
  
  # Empty matrix with the same columns as w and as many rows as newborns
  nextbabies <- matrix(NA,length(which(newbaby)),length(w[1,]))
  colnames(nextbabies) <- colnames(w)
  
  # Fill the columns
  
  # Mother's data
  nextbabies[,"momid"] <- w[newbaby,"id"]
  nextbabies[,"momage"] <- w[newbaby,"age"]
  nextbabies[,"momhiv"] <- w[newbaby,"hiv"]
  nextbabies[,"art"] <- w[newbaby,"art"]
  
  # Newborn data
  nextbabies[,"dob"] <- yr
  nextbabies[,"id"] <- seq(max(w[,"id"])+1,max(w[,"id"])+length(nextbabies[,"id"]),1) # New Ids
  nextbabies[,"age"] <- 0
  nextbabies[,"ceb"] <- 0
  nextbabies[,"cd"] <- 0
  nextbabies[,"birthlast"] <-0
  
  # Determine randomly if is male/female
  nextbabies[,"male"] <- as.numeric(runif(nrow(nextbabies))<=102.5/202.5)  
  
  nextbabies
  
}

#' Updates ceb when individuals have a baby and update last births
#' 
#' @param w (matrix) Population matrix
#' @param newbaby (logical) Vector of length=nrow(w). Each element determines whether a woman gives birth 
#' that year (TRUE) or not (FALSE). As computed by `new.babies`
#' @return (matrix) Population matrix updated
update.women.ceb <- function(w,newbaby){
  
  w[newbaby,"ceb"] <- w[newbaby,"ceb"]+1
  
  # Reset last birth
  w[,"birthlast"] <- 0 
  
  # Update last births
  w[newbaby,"birthlast"] <- 1 
  
  w
}

#' Updates a newborns matrix to account for vertical transmission of HIV
#' 
#' Vertical transmision is determined randomly for each baby according to each individual's probability
#' of vertical transmission.
#' 
#' @param prob.vt.noart (numeric) Probability of mother to child transmission of HIV when mother is not in ART
#' @param prob.vt.art (numeric) Probability of mother to child transmission of HIV when mother is in ART
#' @param nextbabies (matrix) population matrix of newborns, like the one provided by `next.babies`
#' @param yr (integer) current simulation year.
#' @return (matrix) updated mopulation matrix of newborns
vertical.transmision.HIV <- function(prob.vt.noart,prob.vt.art,nextbabies,yr){
  
  # We compute two vectors: one with prov.vt.noart for individuals with HIV and not in art (and 0 otherwise)
  # and another one with prov.vt.art for individuals in art (and 0 otherwise). When we sum both vectors we get
  # a vector with probabilities of vertical transmision for HIV positive (and 0 por HIV negative)
  prob.vt.thisyear.adj <- prob.vt.noart*(nextbabies[,'momhiv']-nextbabies[,'art'])+ # probability not in art
    prob.vt.art*nextbabies[,'art'] #probability in art
  
  # Determine randomly if each baby gets infected
  hivbaby <- runif(nrow(nextbabies))<prob.vt.thisyear.adj
  nextbabies[,"hiv"] <- hivbaby
  
  # Set HIV year
  nextbabies[hivbaby,"hiv_date"] <- yr
  
  nextbabies
}

#' Updates the matrix of population to determine randomly which individuals died during the current
#' year of the simulation
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) population matrix
#' @param prob.death.all (matrix) Probability of dead being HIV negative for each age and year. 
#' As provided by `phivneg.death.ages.years`.
#' @param ages.as.char (character) Same as w[,"age"] but as character. For performance.
#' @return (matrix) population matrix
mortality <- function(yr,w,prob.death.all,ages.as.char){
  
  # Get probability of death for this year according to age
  
  # HIV negative
  prob.death.thisyear <- prob.death.all[as.character(yr),ages.as.char]
  
  # HIV positive and in ART
  prob.death.thisyear.hiv.art <- art.surv.vec(yr,w[,"cd4"],w[,"art_date"])
  
  
  # HIV positive and not in ART. aidsmort is defined as a constant next to `phiv.death`. Infected at age 15 or higher
  prob.death.thisyear.hiv.noart <- aidsmort[1+yr-w[,"hiv_date"]]*((w[,"age"]-(yr-w[,"hiv_date"]))>=15)
  
  # Babies infected at birth and 6 yo or younger
  
  prob.death.babies.infected <- baby.death.hiv(w[,"age"])*(yr-w[,"hiv_date"]==w[,"age"])*(w[,"age"]<=6)
  
  # Missing value of probability means 0 probability
  prob.death.thisyear.hiv.art[is.na(prob.death.thisyear.hiv.art)] <- 0
  prob.death.thisyear.hiv.noart[is.na(prob.death.thisyear.hiv.noart)] <- 0
  prob.death.babies.infected[is.na(prob.death.babies.infected)] <- 0
  
  # Combine the three vectors above to get the probability for each inidividual
  prob.death.thisyear.adj <- prob.death.thisyear*(1-w[,"hiv"]) + # HIV negative
    prob.death.thisyear.hiv.art*w[,'art'] + # HIV positive and in ART
    prob.death.thisyear.hiv.noart*(1-w[,'art'])*w[,'hiv']+ # HIV positive and not in ART
    prob.death.babies.infected*w[,'hiv'] # babies infected at birth
  
  # Determine randomly who died this year...
  died <- runif(nrow(w))<prob.death.thisyear.adj
  
  # ... and was not dead already
  diedthisyear <- died & is.na(w[,"death_date"])
  
  # Date of death
  w[diedthisyear,"death_date"] = yr
  
  # Who died being HIV positive?
  w[diedthisyear,"hivdeath"] = w[diedthisyear,"hiv"]
  
  w
}

#' Updates the matrix of population to determine randomly which individuals got infected during the current
#' year of the simulation. Also determines initial CD4 count and CD4 decline. Updates CD4 count in 
#' people infected in previous years.
#' 
#' When an individual is infected with HIV, the square root of her initial CD4 count is 
#' a random draw from a normal distribution with a mean of 25.9 and a standard deviation of 0.61.
#' For each woman under age 35 the absolute yearly decline in CD4 is defined by a random draw 
#' from a normal distribution with a mean of 1.32, and a standard deviation of 1. For women 35 
#' years or older the draw comes from a normal distribution with a mean of 2.0 and a 
#' standard deviation of 1.
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) population matrix
#' @param prob.hiv.vec (matrix) Probability of getting infected for each age and year. 
#' As provided by `prob.hiv.ages.years`.
#' @param ages.as.char (character) Same as w[,"age"] but as character. For performance.
#' @return (matrix) population matrix
HIV.infection <- function(yr,w,prob.hiv.vec,ages.as.char){
  
  # Probability of getting infected this year for each age category. 0 if already infected
  prob.hiv.thisyear <- prob.hiv.vec[ages.as.char,as.character(yr)]*(1-w[,"hiv"])*is.na(w[,"death_date"])
  
  # Determine randomly who gets infected.
  gothiv <- runif(nrow(w))<prob.hiv.thisyear
  
  # Remove those who died this year
  diedthisyear <- !is.na(w[,"death_date"]) & w[,"death_date"] == yr
  newlyinfected <- gothiv & !diedthisyear
  
  if(any(newlyinfected,na.rm=TRUE)){
    
    # Assign infection and year of infection.
    
    w[newlyinfected,"hiv"]=TRUE
    w[newlyinfected,"hiv_date"]=yr
    
    # When an individual is infected with HIV, the square root of her initial CD4 count is 
    # a random draw from a normal distribution with a mean of 25.9 and a standard deviation of 0.61.
    
    w[newlyinfected,"cd4"] = rnorm(length(which(newlyinfected)),25.91,.61)^2
    
    # For each woman under age 35 the absolute yearly decline in CD4 is defined by a random draw 
    # from a normal distribution with a mean of 1.32, and a standard deviation of 1. For women 35 
    # years or older the draw comes from a normal distribution with a mean of 2.0 and a 
    # standard deviation of 1.
    
    cd4decl35 <- rnorm(nrow(w),1.32,1)
    cd4decg35 <- rnorm(nrow(w),2,1)
    
    # Assign CD4 decline
    
    age_lt_35 <- w[,'age']<35
    age_ge_35 <- !age_lt_35
    
    newlyinfected_and_age_lt_35 <- newlyinfected & age_lt_35
    newlyinfected_and_age_ge_35 <- newlyinfected & age_ge_35
    
    
    w[newlyinfected_and_age_lt_35,'cd4dec'] = cd4decl35[newlyinfected_and_age_lt_35]
    w[newlyinfected_and_age_ge_35,'cd4dec'] = cd4decg35[newlyinfected_and_age_ge_35]
    
  }
  # Update CD4 in people infected in previous years.
  
  oldinfected <- w[,"hiv"] & !newlyinfected
  w[oldinfected,"cd4"] <- cd4.prog(w[oldinfected,"cd4"],w[oldinfected,"cd4dec"], w[oldinfected,"hiv_date"],yr)
  
  w
}

#' Updates the matrix of population to determine randomly which individuals started ART during the current
#' year of the simulation.
#' 
#' 
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) population matrix
#' @param artprobs (matrix)  Annual probabilities for initiating ART given that a woman’s CD4 was below threshold, 
#' for 2004 to 2010. Probability is 0 before 2004.
#' @param threshold (numeric) CD4 threshold. Bellow this value one person could start ART.
#' @return (matrix) population matrix
ART.initiation <- function(yr,w,artprobs,threshold){
  
  # Which individuals are not in ART
  
  noart = is.na(w[,"art_e"]) & w[,"hiv"]
  
  # Which individuals not on ART, are below the threshold, and could start ART
  w[noart,"art_e"] = ifelse(w[noart,"cd4"]<threshold,yr,NA)
  couldgetart = !is.na(w[,"art_e"]) & !w[,"art"]
  
  # Determine randomly which individuals start ART
  newlyart = runif(nrow(w))<artprobs[artprobs[,"yr"]==yr,2]
  
  # Update the matrix
  
  couldgetart_and_newlyart <- couldgetart & newlyart
  
  w[couldgetart_and_newlyart,"art"] = TRUE # New people in ART
  w[couldgetart_and_newlyart,"art_date"] = yr # Year ART was started
  
  w
}


#' Runs one year of simulation
#' 
#' 
#' @param yr (integer) current year in simulation
#' @param w (matrix) matrix of population
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param prob.birth.all (matrix) Annual probability of birth as a function of calendar 
#' year and mother’s age. HIV negative. As given by `prob.birth.ages.years`.
#' @param prob.birth.hiv (matrix) Reduction in prob of giving birth due to HIV for each art, age combination. As provided by `prob.birth.hiv`
#' @param births.age.yr (matrix) Matrix of birth counts by age and year, 
#' like the one provided by `birth.counts.by.age.empty.matrix`
#' @param hivbirths.momshiv (matrix) Matrix of birth counts by HIV status and year, 
#' like the one provided by `birth.counts.by.hiv.status.empty.matrix`
#' @param prob.vt.noart (numeric) Probability of mother to child transmission of HIV when mother is not in ART
#' @param prob.vt.art (numeric) Probability of mother to child transmission of HIV when mother is in ART
#' @param prob.death.all (matrix) Probability of dead being HIV negative for each age and year. 
#' As provided by `phivneg.death.ages.years`.
#' @param artprobs (matrix)  Annual probabilities for initiating ART given that a woman’s CD4 was below threshold, 
#' for 2004 to 2010. Probability is 0 before 2004.
#' @param threshold (numeric) CD4 threshold. Bellow this value one person could start ART.
#' @return (list) with elements: w, births.age.yr, hivbirths.momshiv updated.
w.loop.pass <- function(yr,w,ages,years,hivinc_s,prob.birth.all,prob.birth.hiv,births.age.yr,hivbirths.momshiv,prob.vt.noart,prob.vt.art,prob.death.all,artprobs,threshold){
  print(paste(Sys.time(),":",yr,":",ip,":",pset))
  
  # split in men and women
  
  males <- w[w[,"male"]==1,]
  
  w <- w[w[,"male"]==0,]
  
  # count women by age group to normalize hiv incidence
  counts <- count.women.age.groups(yr,w)
  
  # Update vector of HIV incidence rates
  prob.hiv.vec <- prob.hiv.ages.years(ages,years,hivinc_s,counts["c15"],counts["c20"],counts["c25"],counts["c30"],counts["c35"],counts["c40"],counts["c45"])
  
  w <- update.women.age(yr,w)
  
  males <- update.women.age(yr,males)
  
  # For performance, convert ages to character just once.
  
  ages.as.char <- as.character(w[,"age"])
  
  # Fertility  
  
  newbaby <- new.babies(yr,w,prob.birth.all,prob.birth.hiv,ages.as.char)
  
  nextbabies <- next.babies(yr,w,newbaby)
  
  w <- update.women.ceb(w,newbaby)
  
  # births by age group for tfr
  # women by age group for tfr
  
  births.age.yr <- update.birth.counts.by.age(births.age.yr,nextbabies,yr,counts["c15"],counts["c20"],counts["c25"],counts["c30"],counts["c35"],counts["c40"],counts["c45"])
  
  # Vertical transmission of HIV
  nextbabies <- vertical.transmision.HIV(prob.vt.noart,prob.vt.art,nextbabies,yr)
  
  
  # births to HIV positive women
  # HIV positive births tracker for realized VT
  hivbirths.momshiv <- update.birth.counts.by.hiv.status(hivbirths.momshiv,nextbabies,yr)
  
  w <- rbind(w,nextbabies,males)
  
  # For performance, convert ages to character just once.
  
  ages.as.char <- as.character(w[,"age"])
  
  # Mortality
  
  w<- mortality(yr,w,prob.death.all,ages.as.char)
  
  # HIV infection
  
  w <- HIV.infection(yr,w,prob.hiv.vec,ages.as.char)
  
  
  # ART intiation
  w <- ART.initiation(yr,w,artprobs,threshold)
  
  list(w=w,births.age.yr=births.age.yr,hivbirths.momshiv=hivbirths.momshiv)
  
}

#' Run simulation
#' 
#' @param yrstart (numeric) year to start the simulation
#' @param yrend (numeric) year to end the simulation
#' @param ages (numeric) vector of ages
#' @param years (numeric) vector of years in simulation
#' @param asfr_s (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_s (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @param sexactive15 (numeric) percent of females aged 15-19 who are sexually active
#' @param art (integer) vector of values that should be either: 0 (not on ART) or 1 (on ART)
#' @param mort_s (data.frame) mortality series for the model country, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param matmort (data.frame) Maternal mortality
#' @param am_cntry (character) model country for adult mortality
#' @param u5m_c (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @param bfeed (integer) breastfeeding duration (months) in the general population (must be 6, 12 or 18)
#' @param growth (numeric) yearly population growth rate
#' @param initialpop (integer) initial population
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012) for a specific country
#' @param artprobs (matrix)  Annual probabilities for initiating ART given that a woman’s CD4 was below threshold, 
#' for 2004 to 2010. Probability is 0 before 2004.
#' @param threshold (numeric) CD4 threshold. Bellow this value one person could start ART.
#' @param mmr0 (numeric) initial MMR
#' @param mmr_dec (numeric) annual percent decline in MMR
#' @return (list) simulation result with elements: w (population matrix), births.age.yr (Matrix of birth counts by age and year), hivbirths.momshiv updated (Matrix of birth counts by HIV status and year).
run.simulation <- function(yrstart,yrend,ages,years,asfr_s,tfr_s,sexactive15,arts,mort_s,adultmort,am_cntry,matmort,u5m_c,bfeed,growth,initialpop,hivinc_s,artprobs,threshold,mmr0,mmr_dec){
  
  # Annual probability of birth as a function of calendar year and mother’s age.
  # HIV negative
  
  prob.birth.all <- prob.birth.ages.years(ages,years,asfr_s,tfr_s)
  
  # Reduction in prob of giving birth due to HIV for each art, age combination
  
  prob.birth.hiv <- prob.birth.hiv(ages,sexactive15,arts)
  
  # Annual probability of death, HIV negative women for each age year combination
  
  prob.death.all <- phivneg.death.ages.years(ages,years,mort_s,adultmort,am_cntry,matmort,u5m_c,prob.birth.all,mmr0,mmr_dec)
  
  # Probability of mother to child transmission of HIV. Mother not in ART
  
  prob.vt.noart <- vert_trans(0,bfeed)
  
  # Probability of mother to child transmission of HIV. Mother in ART
  
  prob.vt.art <- vert_trans(1,bfeed)
  
  # Date of birth for initial population
  
  dobs <- initial.DOBs(growth,initialpop,yrstart)
  
  # Empty population matrix
  
  w <- women.empty.matrix(dobs)
  
  # Empty matrix of birth counts by age and year
  
  births.age.yr <- birth.counts.by.age.empty.matrix(yrstart,yrend)
  
  # Empty matrix of birth counts by HIV status and year
  
  hivbirths.momshiv <- birth.counts.by.hiv.status.empty.matrix(yrstart,yrend)
  
  # Run simulation
  
  w_dead <- list()
  
  for (yr in yrstart:yrend) {
    
    # Run simulation pass
    result <- w.loop.pass(yr,w,ages,years,hivinc_s,prob.birth.all,prob.birth.hiv,births.age.yr,hivbirths.momshiv,prob.vt.noart,prob.vt.art,prob.death.all,artprobs,threshold)
    
    # Extract results
    
    w <- result$w
    
    births.age.yr <- result$births.age.yr
    
    hivbirths.momshiv <- result$hivbirths.momshiv
    
    # Remove dead people and store them
    
    w_d <- w[!is.na(w[,"death_date"]),]
    
    w_dead <- c(w_dead,list(w_d))
    
    w <- w[is.na(w[,"death_date"]),]
    
    
    
  }
  
  # Put dead people back into the matrix
  
  w_dead <- do.call(rbind,w_dead)
  
  w <- rbind(w,w_dead)
  
  list(w=w,births.age.yr=births.age.yr,hivbirths.momshiv=hivbirths.momshiv)
  
}

##### RESULTS #####

#' For each year, computes realized HIV prevalence, incidence, and realized ART coverage, prevalence
#' 
#' @param w (matrix) population matrix
#' @param start_year (integer) year to start computation
#' @param end_year (integer) year to end computation
#' @return (list) with elements: hiv_prev (numeric) HIV prevalence, hiv_inc (numeric) HIV incidence,
#' art_prev (numeric) ART prevalence, art_cov (numeric) ART coverage
realized.hiv.art <- function(w,start_year,end_year){
  
  # Realized HIV prevalence, incidence
  hiv_prev = NA
  hiv_inc = NA
  numpos = NA
  num = NA
  hivnew=NA
  #And realized ART coverage, prevalence
  num_elig = NA
  num_art = NA
  art_cov = NA
  art_prev = NA
  for (i in start_year:end_year){
    
    # Number of HIV positive women, alive, 15-49
    hpos = subset(w,is.na(w[,"hiv_date"])==FALSE)
    hpos2 = subset(hpos, hpos[,"hiv_date"]<=i)		
    hpos3 = subset(hpos2,hpos2[,"dob"]<i-14 & hpos2[,"dob"]>i-49)
    
    # Women who didn't die in simulation 
    hpos4a = subset(hpos3,is.na(hpos3[,"death_date"]))
    
    # Women who died after year i
    hpos4b = subset(hpos3,hpos3[,"death_date"]>=i)
    hpos5 = rbind(hpos4a,hpos4b)
    numpos[i] = nrow(hpos5)
    
    # New HIV cases
    hpos6 = subset(hpos5,hpos5[,"hiv_date"]==i)
    hivnew[i] = nrow(hpos6)
    
    # Women alive in year i eligible for ART but not on ART
    # Eligible and not on ART in year i, but started after year i
    num_elig1a = subset(hpos5, hpos5[,"art_e"]>0 & hpos5[,"art_e"]<=i & hpos5[,"art_date"]>i & !is.na(hpos5[,"death_date"]))
    # Eligible and not on ART in year i, never started
    num_elig1b = subset(hpos5, hpos5[,"art_e"]>0 & hpos5[,"art_e"]<=i & is.na(hpos5[,"art_date"]) & !is.na(hpos5[,"death_date"]))
    
    num_elig2 = rbind(num_elig1a,num_elig1b) 
    num_elig[i] = nrow(num_elig2)
    
    # Women alive in year i on ART
    num_art1 = subset(hpos5, hpos5[,"art_date"]<=i & !is.na(hpos5[,"art_date"]))
    num_art[i] = nrow(num_art1)
    
    # ART coverage in year i
    art_cov[i] = num_art[i]/(num_elig[i]+num_art[i])
    
    # Total number of women 15-49 yrs old who were alive in year i
    num_w = subset(w,w[,"dob"]<i-14 & w[,"dob"]>i-49)
    num_w2 = subset(num_w,is.na(num_w[,"death_date"]))
    num_w2a = subset(num_w,num_w[,"death_date"]>=i)
    num_w3 = rbind(num_w2,num_w2a)
    num[i] = nrow(num_w3)
    
    # HIV prevalence, HIV incidence, ART prevalence in year i
    hiv_prev[i] = numpos[i]/num[i]
    hiv_inc[i]= hivnew[i]/(num[i]-numpos[i]+hivnew[i])
    art_prev[i] = num_art[i]/num[i]
    
    
  }
  
  list(hiv_prev=hiv_prev, hiv_inc=hiv_inc, art_prev=art_prev, art_cov=art_cov)
}


#' Computes realized TFR per year
#' 
#' @param births.age.yr (matrix) Matrix of birth counts by age and year, 
#' like the one provided by `birth.counts.by.age.empty.matrix`
#' @param yrstart (numeric) year to start the simulation
#' @param yrend (numeric) year to end the simulation
#' @return (matrix) realzed tfr per year
realized.tfr <- function(births.age.yr,yrstart,yrend){
  
  # Realized TFR
  
  asfr = births.age.yr[,"births"]/births.age.yr[,"women"]
  asfr5 = asfr*5
  tfr <- vector()
  seq(1,length(births.age.yr[,1]),7)
  for(k in seq(1,length(births.age.yr[,1]),7)){
    tfr[(k+6)/7] <- asfr5[k]+asfr5[k+1]+asfr5[k+2]+asfr5[k+3]+asfr5[k+4]+asfr5[k+5]+asfr5[k+6] 
  }
  tfr <- cbind(seq(yrstart,yrend,1),tfr)
  colnames(tfr) <- c("year","tfr")
  
  tfr
  
}

#' For each year, computes realized vertical transmission
#' 
#' @param hivbirths.momshiv (matrix) Matrix of birth counts by HIV status and year, 
#' like the one provided by `birth.counts.by.hiv.status.empty.matrix`
#' @param start_year (integer) year to start computation
#' @param end_year (integer) year to end computation
#' @return (numeric) realized vertical transmission for each year
realized.vert_trans <- function(hivbirths.momshiv,start_year,end_year){
  
  
  #  Realized vertical transmission
  vt=NA
  for (i in start_year:end_year){
    vt[i] = hivbirths.momshiv[,"birthpos"][hivbirths.momshiv[,"year"]==i]/hivbirths.momshiv[,"birthmompos"][hivbirths.momshiv[,"year"]==i]
  }
  
  vt
  
}

#' Calculates children dead per mother
#' 
#' @param w (matrix) population matrix
#' @return (matrix) population matrix with cd and dead updated
children.dead.per.mother <- function(w){
  
  # Who died during the simulation
  
  w[,"dead"] <- !is.na(w[,"death_date"])
  
  # Get children dead
  
  y <- w[!is.na(w[,"momid"]) & w[,"dead"],]
  
  # Count children dead per mother
  
  cd <- data.frame(table(y[,"momid"]))
  
  names(cd) <- c("id","cd2")
  
  cd[,"id"] <- as.integer(as.character(cd[,"id"]))
  
  # Merge with women matrix
  
  w <- as.matrix(left_join(data.frame(w),cd,by="id"))
  
  # w <- as.matrix(merge(w,cd,by="id",all.x=TRUE))
  
  w[,"cd"] <- w[,"cd2"]
  
  w[,"cd"][is.na(w[,"cd"])]<- 0
  
  w <- w[,colnames(w) != "cd2"]
  
  
  w
  
}

##### RUN SIMULATION #####


#' Call this function to run a full simulation
#' 
#' @param inp (data.frame) simulation parameters
#' @param initialpop (integer) initial population
#' @param years (numeric) vector of years in simulation
#' @param ages (numeric) vector of ages
#' @param hivinc_s (data.frame) HIV incidence curve estimated by Hogan and Salomon (2012)
#' @param mort_series (data.frame) mortality series, must contain columns year and q45_15 
#' from the Institute for Health Metrics and Evaluation.
#' @param adultmort (data.frame) UN model life table (UN Population Division). models mortality for each 45q15
#' @param worldfert (data.frame) estimates of age-specific fertility rates (ASFR) 
#' from the United Nations Population Division's World Fertility Data (2013) 
#' @param tfr_series (data.frame) Interpolated estimates of the total fertility rate (TFR) 
#' from the United Nations Population Division’s World Population Prospects (2012)
#' @param art_series (matrix)  Annual probabilities for initiating ART given that a woman’s CD4 was below threshold, 
#' for 2004 to 2010. Probability is 0 before 2004.
#' @param u5m_edit (data.frame) child mortatily UN Inter-agency Group for Child Mortality 
#' Estimation (UN IGME) (2012) for one country.
#' @param matmort (data.frame) Maternal mortality
#' @return (list) simulation result with elements: 
#' bigres (data.frame) with elements sexactive15,mmr0,mmr_dec,curve,bfeed,growth,hiv1985,hiv1990,hiv2000,hiv2010,art2004,art2006,art2008,art2010,vt2010,vt2000,vt1990,vt1985,art_prev2005,art_prev2007,art_prev2009,tfr1980,tfr1990,tfr2000,tfr2010,runsec,
#' tfr realized TFR,
#' art_cov[c(2000:2010)] realized ART coverage from 2000 to 2010, 
#' hiv_prev[c(1980:2010)] (vector) HIV prevalence between 1980 and 2010,
#' hivdeathrate NA,
#' inp (data.frame) input parameters used in the simulation,
#' w (population matrix),
#' art_prev[c(2000:2010)] (vector) ART prevalence between 2000 and 2010,
#' hiv_inc[c(1980:2010)] (vector) hiv incidence between 1980 and 2010
bigsim <- function(inp,initialpop,years,ages,hivhogan,mort_series,adultmort,worldfert,tfr_series,art_series,u5m_edit,matmort){
  
  
  # Parse out the set of inputs
  fertcountry= toString(inp$fertcountry)
  cm_cntry = toString(inp$cm_cntry)
  am_cntry = toString(inp$am_cntry)
  sexactive15 = inp$sexactive15
  mmr0 = inp$mmr0
  mmr_dec = inp$mmr_dec
  curve= toString(inp$curve)
  bfeed = inp$bfeed
  art_col = toString(inp$art_col)
  growth=inp$growth
  yrend=inp$yrend
  yrstart=inp$yrstart
  threshold=inp$threshold
  
  # bigsim TAKES SUBSETS OF DATASETS
  tfr_s = tfr_series[tfr_series[,"country"]==fertcountry,]
  asfr_s=worldfert[worldfert[,"country"]==fertcountry,]
  mort_s = mort_series[mort_series[,"country"]==am_cntry,]
  u5m_c = subset(u5m_edit, country==cm_cntry)
  artprobs=cbind(art_series[,"yr"],art_series[,art_col])
  colnames(artprobs) = c("yr","artinc")
  hivinc_s = as.numeric(hivhogan[hivhogan$Country==curve,])
  names(hivinc_s) <- NULL
  
  # Run simulation
  
  ptm <- proc.time()
  
  arts <- c(0,1)
  
  simulation.result <- run.simulation(yrstart,yrend,ages,years,asfr_s,tfr_s,sexactive15,arts,mort_s,adultmort,am_cntry,matmort,u5m_c,bfeed,growth,initialpop,hivinc_s,artprobs,threshold,mmr0,mmr_dec)
  
  # Extract results
  
  w <- simulation.result$w
  
  # Calculate children dead per mother    
  
  w <- children.dead.per.mother(w)
  
  
  births.age.yr <- simulation.result$births.age.yr
  
  hivbirths.momshiv <- simulation.result$hivbirths.momshiv
  
  
  start_year <- 1980
  
  end_year <- 2010
  
  realized.hiv.art.result <- realized.hiv.art(w,start_year,end_year)
  
  hiv_prev <- realized.hiv.art.result$hiv_prev
  hiv_inc <- realized.hiv.art.result$hiv_inc
  art_prev <- realized.hiv.art.result$art_prev
  art_cov <- realized.hiv.art.result$art_cov
  
  tfr <- realized.tfr(births.age.yr,yrstart,yrend)
  
  vt <- realized.vert_trans(hivbirths.momshiv,start_year,end_year) 
  
  hivdeathrate = NA
  
  hiv2010 <- hiv_prev[2010]
  hiv2000 <- hiv_prev[2000]
  hiv1990 <- hiv_prev[1990]
  hiv1985 <- hiv_prev[1985]
  
  art2010 <- art_cov[2010]
  art2008 <- art_cov[2008]
  art2006 <- art_cov[2006]
  art2004 <- art_cov[2004]
  
  art_prev2009 <- art_prev[2009]
  art_prev2007 <- art_prev[2007]
  art_prev2005 <- art_prev[2005]
  
  vt2010 <- vt[2010]
  vt2000 <- vt[2000]
  vt1990 <- vt[1990]
  vt1985 <- vt[1985]
  
  tfr1980 <- tfr[,2][tfr[,1]==1980]
  tfr1990 <- tfr[,2][tfr[,1]==1990]
  tfr2000 <- tfr[,2][tfr[,1]==2000]
  tfr2010 <- tfr[,2][tfr[,1]==2010]
  
  simt <- proc.time()-ptm
  runsec <- as.numeric(simt[3])
  
  bigres <- data.frame(sexactive15,mmr0,mmr_dec,curve,bfeed,growth,hiv1985,hiv1990,hiv2000,hiv2010,art2004,art2006,art2008,art2010,vt2010,vt2000,vt1990,vt1985,art_prev2005,art_prev2007,art_prev2009,tfr1980,tfr1990,tfr2000,tfr2010,runsec)
  
  meta.res1 = list(bigres,tfr,art_cov[c(2000:2010)],hiv_prev[c(1980:2010)],hivdeathrate,inp,w,art_prev[c(2000:2010)],hiv_inc[c(1980:2010)])
  
  return(meta.res1)
}


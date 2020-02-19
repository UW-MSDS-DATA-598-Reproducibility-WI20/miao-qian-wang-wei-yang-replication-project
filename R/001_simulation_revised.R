#system('docker stop $( docker ps -f "name=redis-server" -q)')
#system("docker container prune -f")
rm(list=ls())

###############################################################################
# Name: 001_simulation.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Run simulations and collect data.
#
# Notes: The code in 001_simulation.R does parallel processing and we need to 
# register a parallel backend. Here we use the doRedis package that offers an 
# interface to a Redis server installed on a docker. The number of workers should 
# be adjusted to the memory and cpu resources on the computer. Please visit 
# https://docs.docker.com to learn how to install docker on your computer.
#
# This script runs several simmulations and stores the results of each simulation
# in a temporary file. That way if the simulations are interrupted, the script should
# pick up the computations where they were left.
#
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

source("R/simulation_functions.R")

source("R/indirect_estimates_functions_revised.R")

install.packages('http://cran.r-project.org/src/contrib/Archive/tidyverse/tidyverse_1.2.1.tar.gz',repos=NULL, type="source")
packageVersion("tidyverse")
require(tidyverse)
# ── Attaching packages ───────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
# ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
# ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
# ✔ tidyr   0.8.2       ✔ stringr 1.4.0  
# ✔ readr   1.1.1       ✔ forcats 0.3.0  
# ── Conflicts ──────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::filter() masks stats::filter()
# ✖ dplyr::lag()    masks stats::lag()


install.packages('https://cran.r-project.org/src/contrib/Archive/foreach/foreach_1.4.4.tar.gz',repos=NULL, type="source")
packageVersion("foreach")
# 1.4.4
require(foreach)

install.packages('https://cran.r-project.org/src/contrib/Archive/iterators/iterators_1.0.10.tar.gz',repos=NULL, type="source")
packageVersion("iterators")
# 1.0.10
require(iterators)

if(!require(parallel)){
  install.packages("parallel",dependencies = TRUE,repos='http://cran.us.r-project.org')
}

require(parallel)
packageVersion("parallel")
# 3.5.2

##### DEFINE FOLDERS #######

if(!dir.exists("results/figdata")){
  dir.create("results/figdata",recursive = TRUE)
}

if(!dir.exists("results/regdata")){
  dir.create("results/regdata",recursive = TRUE)
}

# Simulation results will be stored here

if(!dir.exists("results/models")){
  dir.create("results/models",recursive = TRUE)
}

##### SIMULATION #####

# Read data

hivhogan = read.csv("data/inc_curves.csv",head=TRUE)
mort_series = read.csv("data/IHME_female_mortSMALL.csv",head=TRUE)
adultmort = read.csv("data/MLTfemSMALL.csv",head=TRUE)
worldfert= read.csv("data/world_fert.csv",head=TRUE)
tfr_series= read.csv("data/tfr_gapminder_long.csv",head=TRUE)
art_series= read.csv("data/sampleART.csv",head=TRUE)
u5m_edit= read.csv("data/u5m_edit.csv",head=TRUE)
matmort = read.csv("data/matmort.csv",head=TRUE)

# Add HIV-free populations to hivhogan
hivhogan$Country <- factor(hivhogan$Country,levels=c(levels(hivhogan$Country),"zero"))
hivhogan[63,1] = "zero"
hivhogan[63,c(2:47)]=0

# Create parameters sets

fertcountry <- c("Botswana","Uganda")
cm_cntry <-  c("Mali","Morocco")
am_cntry <-  c("Madagascar","Sudan")
sexactive15 <-  c(30,70)
mmr0 <- c(0.0012,0.012)
mmr_dec <- c(0,0.073) # annual percent decline in MMR
curve <- c("BotswanaUrban","LesothoRural","MalawiRural","UgandaRural","CamerounRural","zero","BotUrb2x")
bfeed <- c(6,18)
art_col <- c("zero","Botswana","Cameroon","Malawi","Bot_dub")
growth <-  c(0.03)
yrend <- 2010
# Starting year
yrstart <- 1906

# CD4 threshold

threshold <- 200


# expand.grid() will create dataset with unique combinations in each row

inputs <- expand.grid(fertcountry, cm_cntry, am_cntry, sexactive15,mmr0,mmr_dec,curve,bfeed,art_col,growth,yrend,yrstart,threshold)
names(inputs) = c("fertcountry", "cm_cntry","am_cntry","sexactive15","mmr0","mmr_dec","curve","bfeed","art_col","growth","yrend","yrstart","threshold")

# Save the inputs set for later

save(inputs,file = "results/models/inputs.RData")

file_number_format <- paste0("%0",nchar(as.character(nrow(inputs))),"d")


# Set size of initial population

initial_populations<- c(7500,15000,22500)
#initial_populations<- c(100,150,225)
years <- c(1906:2010)
#years <- c(1906:1966)
ages <- c(0:120)
#ages <- c(0:75)

# Run the simulations for each initial population

for(ip in initial_populations){
  
  # Parameter sets already processed
  #ip = 150
  psets_already <- as.integer(gsub("models|\\.Rdata","",list.files(paste0("./results/models/p",ip),pattern = "models\\d+",full.names = FALSE)))
  
  psets_to_process <- setdiff(1:nrow(inputs),psets_already)
  
  # Run simulation for each set of parameters
  
  foreach(pset=psets_to_process,.packages = c("dplyr")) %dopar%{
    
    inp <- inputs[pset,]
    
    source("R/simulation_functions.R")
    
    source("R/indirect_estimates_functions.R")
    
    
    
    # Run simmulation
    set.seed(1)
    results <- bigsim(inp,initialpop=ip,years,ages,hivhogan,mort_series,adultmort,worldfert,tfr_series,art_series,u5m_edit,matmort)
    
    # Save simulation results
    #save(meta.res1,file=file.path(paste0("./results/models"),paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
    
    save(results,file=file.path(paste0("./results/models/p",ip),paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
  }
  
}

# Collect hiv2000 differences among initial populations

# For each input row

hiv2000 <- foreach(pset=1:1000,.packages = c("dplyr")) %dopar%{
  
  # And each initial population
  paste0("p",initial_populations) %>% lapply(function(popini){
    
    # Load the results file for that initial population and input set
    
    load(file.path("results/models/",popini,paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
    print(paste(initial_populations,pset))
    # Extract the value of hiv2000
    r <- results[[1]]$hiv2000
    
    names(r) <- popini
    r
  }) %>% unlist() %>% t %>% data.frame(input_set=pset,.) # Put everything in one data.frame
  
  
} %>% bind_rows

# Compute difference

hiv2000 <- hiv2000 %>% mutate(p7500_minus_p15000=p7500-p15000,p15000_minus_p22500=p15000-p22500)

save(hiv2000,file = "results/hiv2000.RData")



##### INDIRECT ESTIMATES FOR INITAL POPULATION 22500 #####

# See if we have something not processed

psets_already <- intersect(as.integer(gsub("figdata.|\\.Rdata","",list.files("./results/figdata/p22500",pattern = "figdata.\\d+",full.names = FALSE))),
                           as.integer(gsub("regdata.|\\.Rdata","",list.files("./results/regdata/p22500",pattern = "regdata.\\d+",full.names = FALSE))))

psets_to_process <- setdiff(1:nrow(inputs),psets_already)

# For each input set not processed

foreach(pset=1:1000,.packages = c("dplyr")) %dopar%{
  
  source("R/indirect_estimates_functions_revised.R")
  
  load(file.path("results/models/p22500/",paste("models",sprintf(file_number_format,pset),".Rdata",sep="")))
  print(pset)
  
  w <- results[[7]]
  
  # Are babies of HIV moms getting HIV?
  # What percent of HIV moms babies have HIV?
  # How many born with HIV? 
  # df = as.data.frame(w)
  # y <- subset(df, hiv_date==2009)
  # plot(df$dob, df$hiv_date)  
  ##################################################  
  
  # format data
  momkidsclean <- as.data.frame(w)
  
  # Using all surviving women
  isf <-ind_est(momkidsclean)
  
  # Using all women
  isf_all <-ind_est_all(momkidsclean)
  
  #Using surviving women and women who died from HIV
  isf_hiv <- ind_est_hiv(momkidsclean)
  
  ie.three <- cbind(isf,isf_all,isf_hiv,pset)
  
  ###################################################################
  # Save results for regressions and for figures
  ###################################################################
  
  
  inp.plus <- as.data.frame(results[1])    
  regdata <- cbind(ie.three,inp.plus[1,], row.names = NULL) # Data for regressions
  figdata <- results[c(2:6,8:9)] # Data for figures
  
  
  save(regdata,file=file.path("results/regdata/p22500/",paste("regdata.",sprintf(file_number_format,pset),".Rdata",sep="")))
  save(figdata,file=file.path("results/figdata/p22500/",paste("figdata.",sprintf(file_number_format,pset),".Rdata",sep="")))
  print('Saved')
  rm(results,regdata,figdata,momkidsclean,isf,isf_all,isf_hiv,ie.three)
}

##### COLLECT DATA FROM ALL SIMULATIONS FOR INITIAL POPULATION 22500 #####


# Merge regdata

psets_to_process <- as.integer(gsub("regdata.|\\.Rdata","",list.files("./results/regdata/p22500",pattern = "regdata.\\d+",full.names = FALSE))) %>% sort

nbd2k <- psets_to_process %>% lapply(function(pset){
  
  filename <- file.path("results/regdata/p22500",paste("regdata.",sprintf(file_number_format,pset),".Rdata",sep=""))
  tryCatch({
    load(filename)
    
    regdata %>% mutate(i=pset) %>% select(i,everything())
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}) %>% bind_rows() %>% as.data.frame()


save(nbd2k,file="results/regdata/p22500/regdata_all.Rdata")

# Merge figdata


h <- list()
tf<- list()
ar<- list()
hd<- list()
u5m <- list()
inps <- list()
arp <- list()
hivinc <- list()

psets_to_process <- as.integer(gsub("figdata.|\\.Rdata","",list.files("results/figdata/p22500",pattern = "figdata.\\d+",full.names = FALSE))) %>% sort

for(pset in psets_to_process){

  filename <- file.path("results/figdata/p22500",paste("figdata.",sprintf(file_number_format,pset),".Rdata",sep=""))
  tryCatch({
    load(filename)
  
    
    h[[pset]] <- figdata[[3]]
    tf[[pset]] <- figdata[[1]]
    ar[[pset]] <- figdata[[2]]
    hd[[pset]] <- figdata[[4]]
    inps[[pset]] <- figdata[[5]]
    arp[[pset]] <- figdata[[6]]
    hivinc[[pset]] <- figdata[[7]]
    rm(figdata)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

save(h,tf,ar,hd,u5m,inps,arp,file="results/figdata/p22500/figdata_all.Rdata")

##### Clean #####

#system("kill $(ps -xa|grep 'exec/R'|awk '{print $1}')")

#system('docker stop $( docker ps -f "name=redis-server" -q)')
#system("docker container prune -f")

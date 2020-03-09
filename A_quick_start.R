##install and load packages
req_pack_cran=c("Cubist","asdreader","shiny","plyr","signal")

for(p in req_pack_cran){
  if(p %in% rownames(installed.packages()) == F){
    install.packages(p)
  }
}

lapply(req_pack_cran, require, character.only = TRUE)

##set working directory
setwd(".")

##load epo projection matrix
p=read.delim(paste0(getwd(),"/ASD_demo/read_only/EPO/EPO_projectionMatrix.txt"), header=F)

##load stored functions - packages unstable
source(paste0(getwd(),"/ASD_demo/read_only/R_code/spec_fns_copy.R"))

##load pre-processing protocol
source(paste0(getwd(),"/ASD_demo/read_only/R_code/pre_process.R"))

##load EPO pre-processing protocol
source(paste0(getwd(),"/ASD_demo/read_only/R_code/pre_process_EPO.R"))

##load all predictive models
mods=list.files(paste0(getwd(),"/ASD_demo/read_only/Models"),full.names=T)
sapply(mods,load,.GlobalEnv)

# ##launch shiny
runApp("ASD_demo")

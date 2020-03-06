##install and load packages
req_pack_cran=c("Cubist","prospectr","asdreader","shiny")

for(p in req_pack_cran){
  if(p %in% rownames(installed.packages()) == F){
    install.packages(p)
  }
}

if("soilspec" %in% rownames(installed.packages()) == F){
  devtools::install_github('AlexandreWadoux/soilspec')
}

if("spectroscopy" %in% rownames(installed.packages()) == F){
  devtools::install_github("mariofajardo/Spectracus",ref="develop")
}

lapply(req_pack_cran, require, character.only = TRUE)
lapply(c("soilspec","spectroscopy"), require, character.only = TRUE)


##set working directory
setwd(".")

##load epo projection matrix
p=read.delim(paste0(getwd(),"/read_only/EPO/EPO_projectionMatrix.txt"), header=F)

##load pre-processing protocol
source(paste0(getwd(),"/read_only/R_code/pre_process.R"))

##load EPO pre-processing protocol
source(paste0(getwd(),"/read_only/R_code/pre_process_EPO.R"))

##load all predictive models
mods=list.files(paste0(getwd(),"/read_only/Models"),full.names=T)
sapply(mods,load,.GlobalEnv)

# ##launch shiny
# runApp("NIR_demo")

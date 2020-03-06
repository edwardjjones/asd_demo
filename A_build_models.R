##install and load packages
req_pack_cran=c("Cubist","prospectr","asdreader")

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


##load calibration data
data("datsoilspc")

##set working directory
setwd(".")

##load epo projection matrix
p=read.delim(paste0(getwd(),"/read_only/EPO/EPO_projectionMatrix.txt"), header=F)

##load pre-processing protocol
source(paste0(getwd(),"/read_only/R_code/pre_process.R"))

##load EPO pre-processing protocol
source(paste0(getwd(),"/read_only/R_code/pre_process_EPO.R"))

####Pre-processing
preproc_nir = preproc(datsoilspc$spc)

####Pre-processing with EPO
preproc_epo = epo_preproc(datsoilspc$spc)


################################
###Build and export TC model ###
################################
soil_y<- datsoilspc$TotalCarbon
isrow<-complete.cases(soil_y)
sp_y<- preproc_nir[isrow,] 
soil_y<-soil_y[isrow]
nd<-length(soil_y)
set.seed(111)
ic<-sample(1:nd, round(nd*0.75))

#This forms the calibration set  
spec_c<-sp_y[ic,]
soil_c<-soil_y[ic]
#This forms the validation set
spec_v<-sp_y[-ic,]
soil_v<-soil_y[-ic]

###construct and test model
soil_c.cubist_model<-cubist(x= spec_c, y=soil_c, control=cubistControl(rules = 3, extrapolation=10), committees = 10)     

# Predict on the calibration data
soil_c.cubist_predict<-predict(soil_c.cubist_model, spec_c)# predict the value of soil variable based on spectra
# goodness of fit
gfc.cubist_predict<- goof(soil_c,soil_c.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Calibration")

# Now predict on the validation data
soil_v.cubist_predict<-predict(soil_c.cubist_model, spec_v)# predict the value of soil variable based on spectra

# goodness of fit
gfv.cubist_predict<- goof(soil_v,soil_v.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Validation")

gfc.cubist_predict
gfv.cubist_predict

##export final model
soil_c.cubist_model<-cubist(x= rbind(spec_c,spec_v), y=c(soil_c,soil_v), control=cubistControl(rules = 3, extrapolation=10), committees = 10)     
TC_model=soil_c.cubist_model
save("TC_model",file=paste0(getwd(),"/read_only/Models/TC_model.RData"))


################################
###Build and export TC EPO model
################################
soil_y<- datsoilspc$TotalCarbon
isrow<-complete.cases(soil_y)
sp_y<- preproc_epo[isrow,] 
soil_y<-soil_y[isrow]
nd<-length(soil_y)
set.seed(111)
ic<-sample(1:nd, round(nd*0.75))

#This forms the calibration set  
spec_c<-sp_y[ic,]
soil_c<-soil_y[ic]
#This forms the validation set
spec_v<-sp_y[-ic,]
soil_v<-soil_y[-ic]

###construct and test model
soil_c.cubist_model<-cubist(x= spec_c, y=soil_c, control=cubistControl(rules = 3, extrapolation=10), committees = 10)     

# Predict on the calibration data
soil_c.cubist_predict<-predict(soil_c.cubist_model, spec_c)# predict the value of soil variable based on spectra
# goodness of fit
gfc.cubist_predict<- goof(soil_c,soil_c.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Calibration")

# Now predict on the validation data
soil_v.cubist_predict<-predict(soil_c.cubist_model, spec_v)# predict the value of soil variable based on spectra

# goodness of fit
gfv.cubist_predict<- goof(soil_v,soil_v.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Validation")

gfc.cubist_predict
gfv.cubist_predict

##export final model
soil_c.cubist_model<-cubist(x= rbind(spec_c,spec_v), y=c(soil_c,soil_v), control=cubistControl(rules = 3, extrapolation=10), committees = 10)     
TC_EPO_model=soil_c.cubist_model
save("TC_EPO_model",file=paste0(getwd(),"/read_only/Models/TC_EPO_model.RData"))


################################
###Build and export clay model ###
################################
soil_y<- datsoilspc$clay
isrow<-complete.cases(soil_y)
sp_y<- preproc_nir[isrow,] 
soil_y<-soil_y[isrow]
nd<-length(soil_y)
set.seed(111)
ic<-sample(1:nd, round(nd*0.75))

#This forms the calibration set  
spec_c<-sp_y[ic,]
soil_c<-soil_y[ic]
#This forms the validation set
spec_v<-sp_y[-ic,]
soil_v<-soil_y[-ic]

###construct and test model
soil_c.cubist_model<-cubist(x= spec_c, y=soil_c, control=cubistControl(rules = 3, extrapolation=10), committees = 10)     

# Predict on the calibration data
soil_c.cubist_predict<-predict(soil_c.cubist_model, spec_c)# predict the value of soil variable based on spectra
# goodness of fit
gfc.cubist_predict<- goof(soil_c,soil_c.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Calibration")

# Now predict on the validation data
soil_v.cubist_predict<-predict(soil_c.cubist_model, spec_v)# predict the value of soil variable based on spectra

# goodness of fit
gfv.cubist_predict<- goof(soil_v,soil_v.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Validation")

gfc.cubist_predict
gfv.cubist_predict

##export final model
soil_c.cubist_model<-cubist(x= rbind(spec_c,spec_v), y=c(soil_c,soil_v), control=cubistControl(rules = 3, extrapolation=10), committees = 10)     
clay_model=soil_c.cubist_model
save("clay_model",file=paste0(getwd(),"/read_only/Models/clay_model.RData"))


################################
###Build and export clay EPO model
################################
soil_y<- datsoilspc$clay
isrow<-complete.cases(soil_y)
sp_y<- preproc_epo[isrow,] 
soil_y<-soil_y[isrow]
nd<-length(soil_y)
set.seed(111)
ic<-sample(1:nd, round(nd*0.75))

#This forms the calibration set  
spec_c<-sp_y[ic,]
soil_c<-soil_y[ic]
#This forms the validation set
spec_v<-sp_y[-ic,]
soil_v<-soil_y[-ic]

###construct and test model
soil_c.cubist_model<-cubist(x= spec_c, y=soil_c, control=cubistControl(rules = 3, extrapolation=10), committees = 10)     

# Predict on the calibration data
soil_c.cubist_predict<-predict(soil_c.cubist_model, spec_c)# predict the value of soil variable based on spectra
# goodness of fit
gfc.cubist_predict<- goof(soil_c,soil_c.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Calibration")

# Now predict on the validation data
soil_v.cubist_predict<-predict(soil_c.cubist_model, spec_v)# predict the value of soil variable based on spectra

# goodness of fit
gfv.cubist_predict<- goof(soil_v,soil_v.cubist_predict, xlab= "Observed", ylab="Predicted", main="Cubist Validation")

gfc.cubist_predict
gfv.cubist_predict

##export final model
soil_c.cubist_model<-cubist(x= rbind(spec_c,spec_v), y=c(soil_c,soil_v), control=cubistControl(rules = 3, extrapolation=10), committees = 10)     
clay_EPO_model=soil_c.cubist_model
save("clay_EPO_model",file=paste0(getwd(),"/read_only/Models/clay_EPO_model.RData"))

# ##launch shiny
# runApp("NIR_demo")
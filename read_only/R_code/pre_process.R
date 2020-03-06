####Pre-processing with no EPO
preproc=function(nir){
   
  ###splice correction
  spectra = spliceCorrection(nir, 350:2500, splice=c(1000,1800))
  
  ##Savitzky-Golay filter
  spectra = filter_spectra(spectra,type="S-Golay" ,n = 11, p = 2, m = 0)
  
  ###convert to absorbance
  spectra = log(1/as.matrix(spectra))
  
  ###Standard normal variate baseline correction
  spectra = filter_spectra(spectra, type = "SNV") 
  
  ###trimming
  spectra = strip_spectra(spectra, 350:2500, wavlimits = range(500:2450), which = 10)
  
  ##round
  spectra=round(spectra,3)
  
  ###rename column names
  colnames(spectra)=paste0("X",1:ncol(spectra))
  
  return(spectra) 
}
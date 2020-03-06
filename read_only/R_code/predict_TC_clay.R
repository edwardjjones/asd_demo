###id spectra
spectra= nir[i+1,]
spectra=rbind(spectra,spectra)
rownames(spectra)=1:2
spectra=as.matrix(spectra)

if(do_epo){
  spec_snv = epo_preproc(spectra)
  new_c_pred = predict(TC_EPO_model,spec_snv)[1]
  new_clay_pred = predict(clay_EPO_model, spec_snv)[1]
}else{
  spec_snv = preproc(spectra)
  new_c_pred = predict(TC_model,spec_snv)[1]
  new_clay_pred = predict(clay_model, spec_snv)[1]
}

##predict TC
if(new_c_pred>3){new_c_pred=5}
if(new_c_pred<0){new_c_pred=0}
predict_c<<-c(predict_c,new_c_pred)

##predict clay
if(new_clay_pred>100){new_clay_pred=100}
if(new_clay_pred<0){new_clay_pred=0}
predict_clay<<-c(predict_clay,new_clay_pred)

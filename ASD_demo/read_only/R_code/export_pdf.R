pdf(paste0(getwd(),"/out/",site_id_export,"_report.pdf"))
tot_scans=(100/scan_res_export+1)
#input$goButton
#isolate({
pars = c('plt','usr')
par(xpd=NA)
par(cex=1)
par(cex.axis=1.5)
par(cex.lab=1.8)
par(cex.main=2)
par(cex.sub=1)
par(mar=c(5.1,5.1,1,2.1))

# layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE),widths=c(1,3,3,3),heights=c(5,5,5,1))
layout(matrix(c(4,4,4,1,2,3), 2, 3, byrow = TRUE),widths=c(1,3,3,3),heights=c(5,5))

plot(NA,xlab="Colour",ylab="Depth (cm)", xaxt="n", yaxt="n", ylim=c(1,11),xlim=c(0,1), yaxs="i",xaxs="i")
axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
par1 <- c(list(mfg=c(1,1)), par(pars))

plot(NA,col="light grey",ylab="Depth (cm)",  yaxt="n", ylim=c(1,11),xlim=c(0,6), yaxs="i",xaxs="i", xlab="TC (%)")
axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
par3 <- c(list(mfg=c(1,2)), par(pars))

plot(NA,col="light grey",ylab="Depth (cm)",  yaxt="n", ylim=c(1,11),xlim=c(0,100), yaxs="i",xaxs="i", xlab="Clay (%)")
axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
par4 <- c(list(mfg=c(1,3)), par(pars))

plot(NA,type="l", lty=1, ylim=c(0,1), xlim=c(0,2151), yaxs="i",xaxs="i", xlab="Wavelength (nm)", ylab="Reflectance",xaxt="n")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light grey")
axis(1,at=seq(51,2151,350), labels=seq(400,2500,350))
par2 <- c(list(mfg=c(2,1)), par(pars))


##plot coloured polygon
par(par1)
for(j in 1:nrow(nir)){
  
  mid_pt=11-(j-1)*scan_res_export/10
  
  polygon(x=c(0,0,1,1), y=c(mid_pt - scan_res_export/20, mid_pt + scan_res_export/20,
                            mid_pt + scan_res_export/20, mid_pt - scan_res_export/20),col=cols[j])  
}

##Plot TC 
par(par3)
points(predict_c[1:i], seq(11,0,-(scan_res_export/10))[1:i], col="blue", pch=13, cex=2)

##Plot clay 
par(par4)
points(predict_clay[1:i], seq(11,0,-(scan_res_export/10))[1:i], col="red", pch=13, cex=2)

##Plot coloured spectra
par(par2)
matplot(1:ncol(nir), t(nir),col=cols, add=T, type="l", lty=1)
dev.off()
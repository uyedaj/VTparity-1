 
fishdata <- read.csv("fishdata2020.csv",  stringsAsFactors=TRUE)
 attach(fishdata)
names(fishdata) 
   
 quartz()
  plot(tfec, tamat, pch=as.numeric(major_group)+14, cex=1.5, col=as.numeric(reproductive_mode)+3, 	xlim=c(0,20))
  	 # textxy(tfec, tamat, labs=species, col="black", cex= 0.5, offset=0.6)
  legend("topright", legend=levels(reproductive_mode), text.col=1:length(levels(reproductive_mode))+3, cex=1.5, bty="n") 
  
 #legend("topright", legend=levels(clade), pch=1:length(levels(clade)), cex=0.7, bty="n", col=gray(0.6), text.col=gray(0.6)) 
  	
   	detach(fishdata) 

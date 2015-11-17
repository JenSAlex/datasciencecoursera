corr<-function(directory,threshold=0){
	j=1
	completedata<-complete(directory)
	
	##Get list of monitors in threshold
	passrows<-completedata[,"nobs"]>threshold
	mons<-completedata[,"id"]
	goodmons<-mons[passrows]
	corrvals<-vector("numeric",length=length(goodmons))	
	## Get Correlation for monitors in threshold
	for(i in goodmons){
		
		## Add zeros to beginning of file names
		monnum<- if(i<10){
			paste("00",as.character(i),sep="")
		} else if (i>=10 && i<100){
			paste("0",as.character(i),sep="")
		} else {
			as.character(i)
		}
		## Read file for monitor
		filename<- paste(directory,"/",monnum,".csv",sep="")
		con<-file(filename,"r")
		mondata<-read.csv(con)
		close(con)
		
		##Get correlation from each monitor
		corrvals[j] <- cor(mondata[,"nitrate"],mondata[,"sulfate"],use="complete.obs")
		j<-j+1

	}
	
	corrvals
	
}
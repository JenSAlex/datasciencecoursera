complete <-function(directory,id=1:332){
	monrowdata<-data.frame(id=integer(),
						nobs=integer())
						
	for(i in id){
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
		data<-read.csv(con)
		close(con)
		
		##Get Totals and Rows from each monitor
		nitrate<-data[,"nitrate"]
		sulfate<-data[,"sulfate"]
		z<-complete.cases(nitrate,sulfate)
		rowcount<-length(nitrate[z])
		monrowdata<-rbind(monrowdata,data.frame(id=i,nobs=rowcount))
	
	}
	
	##Return values
	monrowdata
	
}
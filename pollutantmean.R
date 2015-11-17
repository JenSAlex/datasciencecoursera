pollutantmean <-function(directory,pollutant,id=1:332){
	monrowdata<-data.frame(montotal=double(),
						monrows=integer())
						
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
		rowdata<-data[,pollutant]
		z<-complete.cases(data[,pollutant])
		filetotal<-sum(rowdata[z])
		rowcount<-length(rowdata[z])
		monrowdata<-rbind(monrowdata,data.frame(montotal= filetotal,monrows=rowcount))
	
	}
	##Calculate the mean across all monitors
	meanval<-sum(monrowdata$montotal)/sum(monrowdata$monrows)
	
	##Return values
	meanval
	
}
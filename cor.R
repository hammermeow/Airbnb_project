pollutantmean <- function(directory, pollutant, id = 1:332) {
        # Get full path of the specsdata folder
        directory <- paste(getwd(),"/",directory,"/",sep="")
        
        # list file
        file_list <- list.files(directory)
        data <- NA
     
        for (i in id) {
               
                file_dir <- paste(directory,file_list[i],sep="")
                file_data <- read.csv(file_dir)
                data <- rbind(data,file_data)
        }
        mean(data[[pollutant]],na.rm = TRUE)
}
pollutantmean("specdata", "nitrate", 70:72)



complete <- function(directory,id = 1:332){
        directory <- paste(getwd(),"/",directory,"/",sep="")
        file_list <- list.files(directory)
        nod<-rep(0,length(id))
        j<-1
        for (i in id) {
                file_dir <- paste(directory,file_list[i],sep="")
                file_data <- read.csv(file_dir)
                df<-file_data[!is.na(file_data$sulfate) & !is.na(file_data$nitrate),]
                nod[j] <-length(df$ID)
                j <- j+1
        }
        mydat<-data.frame(id=id,nod=nod)
        mydat
}
complete("specdata",1:2)
complete("specdata", c(2, 4, 8, 10, 12))


corr <-function(directory,threshold = 0){
        directory <- paste(getwd(),"/",directory,"/",sep="")
        file_list <- list.files(directory)
        correlation<-c()
        for (i in 1:332) {
                file_dir <- paste(directory,file_list[i],sep="")
                file_data <- read.csv(file_dir)
                df <- file_data[complete.cases(file_data),]
                if( nrow(df) > threshold ){
                        correlation<-c(correlation,cor(df$sulfate,df$nitrate))}
        }
        correlation
}
corr("specdata", 150)
corr("specdata", 5000)
cr <- corr("specdata")
length(cr)

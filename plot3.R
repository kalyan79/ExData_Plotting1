#The plot3.R function reads fromdate and todate as input and grep required values to intermediate file
# Plot empty values with type="n"  and then add lines one by one
#plot3("household_power_consumption.txt","1/2/2007","2/2/2007")

plot3 <- function (filename, fromdate,todate){
    pattern <- paste("Date|^",fromdate,"|^",to_date, sep="")
    #grep specific pattern from Input file and write into intermediate file
    write(grep(pattern,readLines(filename), value=T),"req_data.txt")
    df_hpc <- read.table("req_data.txt", header = TRUE, 
                         nrows = 5, sep=";")
    classes <- sapply(df_hpc, class)
    df_hpc <- read.table("req_data.txt", header = TRUE, 
                         colClasses = classes,sep=";", 
                         na.string="?" )
    #print plot as png file
    png(
        filename = "plot3.png",height=480,width=480,
        units = "px", bg=NA)
    #plot empty axis with type="n"  and then add lines for 3 sub metering values
    plot(x=strptime(paste(df_hpc$Date, df_hpc$Time), 
                    "%d/%m/%Y %H:%M:%S"), y=df_hpc$Sub_metering_1, 
         type="n",ylab= "Energy sub Metering", xlab="")
    lines(x=strptime(paste(df_hpc$Date, df_hpc$Time), 
                     "%d/%m/%Y %H:%M:%S"), 
          y=df_hpc$Sub_metering_1, type="l")
    lines(x=strptime(paste(df_hpc$Date, df_hpc$Time), 
                     "%d/%m/%Y %H:%M:%S"), y=df_hpc$Sub_metering_2, 
          type="l",col="red")
    lines(x=strptime(paste(df_hpc$Date, df_hpc$Time),
                     "%d/%m/%Y %H:%M:%S"), y=df_hpc$Sub_metering_3,
          type="l",col="blue")
    #Added legends to the plot
    legend("topright",legend = c('Sub_metering_1',
                                 'Sub_metering_2',	'Sub_metering_3'), 
           lty=c(1,1,1), col=c("black","red","blue"))
    dev.off()
}
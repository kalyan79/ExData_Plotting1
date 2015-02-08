#The plot4.R function reads fromdate and todate as input and grep required values to intermediate file
#Change par(mfrow) to get 2 x 2 plots
#plot4("household_power_consumption.txt","1/2/2007","2/2/2007")

plot4 <- function (filename, fromdate,todate){
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
        filename = "plot4.png",height=480,width=480,
        units = "px", bg = "transparent")
    #plot - par(mfrow) to get 2x2 plots. Then plot one by one to get multiple plots
    par(mfrow=c(2,2))	
    plot(x=strptime(paste(df_hpc$Date, df_hpc$Time), 
                    "%d/%m/%Y %H:%M:%S"), y=df_hpc$Global_active_power, 
         type="l" , ylab= "Global Active Power",
         xlab="")		
    plot(x=strptime(paste(df_hpc$Date, df_hpc$Time),
                    "%d/%m/%Y %H:%M:%S"), y=df_hpc$Voltage, 
         type="l" , ylab= "Voltage", xlab="datetime")	
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
    #Added legends to the plot 3
    legend("topright",legend = c('Sub_metering_1',
                                 'Sub_metering_2',	'Sub_metering_3'), 
           col=c("black","red","blue"))
    plot(x=strptime(paste(df_hpc$Date, df_hpc$Time),
                    "%d/%m/%Y %H:%M:%S"), y=df_hpc$Global_reactive_power, 
         type="l" , ylab= "Global_reactive_power", xlab="datetime")
    dev.off()
}
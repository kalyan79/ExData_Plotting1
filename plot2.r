#The plot2.R function reads fromdate and todate as input and grep required values to intermediate file
#Convert date and time as datetime field using strptime. Datetime will be the y axis in Plot function.
#plot2("household_power_consumption.txt","1/2/2007","2/2/2007")


plot2 <- function (filename, fromdate,todate){
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
        filename = "plot2.png",height=480,width=480,
        units = "px",  bg = "transparent")
    #Plotting - strptime date & time as Datetime value
    plot(x=strptime(paste(df_hpc$Date, df_hpc$Time), 
                    "%d/%m/%Y %H:%M:%S"),
         y=df_hpc$Global_active_power, 
         type="l" , xlab="",
         ylab= "Global Active Power(kilowatts)" )
    dev.off()
}
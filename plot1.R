#The plot1.R function reads fromdate and todate as input and grep required values to intermediate file
#Format of fromdate and todate should be same as exists in the input file.
#Histogram developed from data frame will be printed as plot1.png file
#plot1("household_power_consumption.txt","1/2/2007","2/2/2007")


plot1 <- function (filename, fromdate,todate){
    pattern <- paste("Date|^",fromdate,"|^",to_date, sep="")
    #grep specific pattern from Input file and write into intermediate file
    write(grep(pattern,readLines(filename), value=T),"req_data.txt")
    df_hpc <- read.table("req_data.txt", header = TRUE, 
                         nrows = 5, sep=";")
    classes <- sapply(df_hpc, class)
    df_hpc <- read.table("req_data.txt", header = TRUE, 
                         colClasses = classes,sep=";", 
                         na.string="?" )
    #print Histogram as png file
    png(
        filename = "plot1.png",height=480,width=480,
        units = "px", bg=NA)
    #histogram
    hist(df_hpc$Global_active_power, 
         col="red",
         main="Global Active Power",
         xlab= "Global Active Power(kilowatts)")
    dev.off()
}
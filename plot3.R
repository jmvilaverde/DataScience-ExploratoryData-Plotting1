##Function that draws the plot into the PNG
##The function calls to get_data_file_zip to obtain the data
plot3 <- function(){
        
        #Get data subset
        data <- get_data_file_zip()
        
        #set PNG values
        filePNG = "plot3.png"
        pngWidth = 480
        pngHeight = 480
        
        
        #Force output to English for weekdays label in x axis
        Sys.setlocale("LC_TIME", "English")
        
        #Open devide PNG, set width and height
        png(file=filePNG,width=pngWidth,height=pngHeight)
        
        #Create plot, type = "l" to obtain "lineal graphic"
        #label x axis = "", label y axis = "Energy sub metering"
        plot(data$datetime,data$Sub_metering_1, type="l", xlab = "", ylab = "Energy sub metering")
        #Add a line with data from Sub_metering_2, color red
        lines(data$datetime,data$Sub_metering_2, col = "red")
        #Add a line with data from Sub_metering_3, color blue
        lines(data$datetime,data$Sub_metering_3, col = "blue")
        #Add a legend in top right corner, set line wide, set colors and text per label
        legend("topright", lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        #Close PNG
        dev.off()
}

##Function that download the zip file, unzip it, process the data and subsetting it
get_data_file_zip <- function(directory = "data", 
                              fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                              fileDest="household_power_consumption.txt"){
        
        #Set directory path
        directory <- paste(".",directory,sep="/")
        
        #Check if the directory exists, if doesn't exists, create it
        if (!file.exists(directory)){
                dir.create(directory)
        }
        
        #set the name for our downloaded zip file
        tempZip <- paste(directory, "tempZip.zip", sep = "/")
        #set the name for our unzipped file
        fileDest <- paste(directory, fileDest, sep = "/")
        
        #Check if the temporal zip file existis, if doesn't then download it
        if (!file.exists(tempZip)){
                download.file(fileUrl, destfile = tempZip)
        }
        
        #Check if the unzipped file exists, if doesn't then unzip de zip file
        if (!file.exists(fileDest)){
                unzip(tempZip, exdir = directory)
        }
        
        #Read the data from the file, with header, separator is ";" and NA values are represented as ? in the file
        data <- read.table(fileDest, header=TRUE, sep=";", na.strings="?")
        
        #Set the field Date as a date format
        data$Date <- as.Date(data$Date, "%d/%m/%Y")
        
        #Keep only the subset with Date "2007-02-01" or "2007-02-02"
        data <- subset(data,Date == "2007-02-01" | Date == "2007-02-02")

        #Create a new column "datetime" to put on it date + time
        data["datetime"] <- NA
        data$datetime <- strptime(paste(data$Date,data$Time,sep=" "), "%F %H:%M:%S")
        
        data
}

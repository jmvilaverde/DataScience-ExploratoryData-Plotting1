##Function that draws the plot into the PNG
##The function calls to get_data_file_zip to obtain the data
plot1 <- function(){
        
        #Get data subset
        data <- get_data_file_zip()
        
        #set PNG values
        filePNG = "plot1.png"
        pngWidth = 480
        pngHeight = 480
                
        #Open devide PNG, set width and height
        png(file=filePNG,width=pngWidth,height=pngHeight)
        
        #Create hist, color red, header = "Global Active Power"
        #label x axis = "Global Active Power (kilowatts)", label y axis = "Frequency"
        hist(data$Global_active_power, col = "red", main = "Global Active Power"
             , xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
                
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
                
        data
}

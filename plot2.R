# Nicholas Roy Barker
# plot2.R
# 20140810

## Set some params
# set working dir
workingDir <- "C:/Users/Nick/Desktop/coursera/jhu_ds_cert_4_exploratoryDataAnalysis/courseProject1/exdata-data-household_power_consumption"
setwd(workingDir)

# define file input
fileName <- "household_power_consumption.txt"

# define wanted dates as a character vector, for subsetting entire dataset
wanted_dates <- c("1/2/2007", "2/2/2007")

# read the CSV, assign it to data frame 'df'
f          <- paste(getwd(), "/", fileName, sep = "")
df <- read.csv(f, 
               header = TRUE, 
               sep = ";",
               colClasses = "character")

# subset dataframe df, retaining records only for wanted dates
wanted.df <- df[df$Date %in% wanted_dates, ]

# cast Date/Time values to datetime objects, to support weekday names in plots
# create a new column "datetime.str" which concatenates the date and time
dts <- paste(wanted.df$Date,wanted.df$Time,sep=" ")
dt  <- strptime(dts,"%d/%m/%Y %H:%M:%S")
dt.df <- as.data.frame(dt)
names(dt.df) <- c("datetime.str")
wanted.df    <- cbind(wanted.df, dt.df)
# create a new column "weekday"
wkd <- strftime(wanted.df$datetime.str, "%a") # abbreviated weekday
wkd.df <- as.data.frame(wkd)
names(wkd.df) <- c("weekday")
wanted.df     <- cbind(wanted.df, wkd.df)


# checking for missing values, which are coded as "?" in this dataset - 
# but there don't seem to be any, at least not for the two days in question
#for (row_i in 1:nrow(wanted.df)) {row = wanted.df[row_i,]; for (x in row) {if (x == "?") print(row_i)}}

## Create plot2.png

# create X - vector of datetime values in datetime.str
dt  <- wanted.df$datetime.str

# create Y - numeric vector of values in Global_active_power
gap <- as.numeric(wanted.df$Global_active_power)

# define output filename / device
png_filename    <- "plot2.png"
png(png_filename, width = 480, height = 480, units = "px")

# create line chart matching example for plot2, save to png, close png
plot(dt,
     gap,
     xlab = "",
     ylab = "Global Active Power (kilowatts)",
     type = "n"
    )
lines(dt,
      gap,
      col = "black",
      type = "l"
     )
dev.off()

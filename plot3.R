# Nicholas Roy Barker
# plot3.R
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

## Create plot3.png

# define output filename / device
png_filename    <- "plot3.png"
png(png_filename, width = 480, height = 480, units = "px")

# loop over Y axis values to get max, to set as ylim parameter
smn <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
smn.max <- 0
for (i in 1:length(smn))
    {
        print(smn[i])
        cur.max <- max(as.numeric(wanted.df[[smn[i]]]))
        print(cur.max)
        if (cur.max > smn.max) smn.max <- cur.max
    }

# create line chart matching example for plot3
plot(dt,
     gap,
     xlab = "",
     ylab = "Energy sub metering",
     ylim = c(0, smn.max),
     type = "n"
)

# create X - vector of datetime objects in datetime.str
dt  <- wanted.df$datetime.str

# create the multiple Y vectors - for each Sub_metering_n column
smn <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
clr <- c("black", "red", "blue")
for (i in 1:length(smn))
    {
        sm     <- wanted.df[[smn[i]]]
        c.str  <- clr[i]
        lines(dt,
              sm,
              col = c.str,
              type = "l"
             )
    }

# create the legend
legend("topright", smn, lty = 1, col = clr)

dev.off()

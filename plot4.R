# Nicholas Roy Barker
# plot4.R
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

## Create plot4.png

# define output filename / device
png_filename    <- "plot4.png"
png(png_filename, width = 480, height = 480, units = "px")

# Use the par(mfcol()) function to define a 2x2 plot matrix, to be filled column-wise
par(mfcol=c(2,2))

# Since all 4 plots use same X, create X - vector of datetime objects in datetime.str
dt  <- wanted.df$datetime.str

# 1) Create the plot4 top left chart/plot4_tl
# create Y - numeric vector of values in Global_active_power
gap <- as.numeric(wanted.df$Global_active_power)

# create line chart matching example for plot2, save to png, close png
plot(dt,
     gap,
     xlab = "",
     ylab = "Global Active Power",
     type = "n"
    )
lines(dt,
      gap,
      col = "black",
      type = "l"
     )

# 2) Create the plot4 bottom left chart/plot4_bl
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
legend("topright", smn, lty = 1, col = clr, bty = "n")

# 3) Create the plot4 top right chart/plot4_tr

# get min/max Voltage for ylim
voltage.min <- min(as.integer(wanted.df$Voltage))
voltage.max <- max(as.integer(wanted.df$Voltage))

# create line chart matching example for plot4_tr
plot(dt,
     wanted.df$Voltage,
     xlab = "datetime",
     ylab = "Voltage",
     ylim = c(voltage.min, voltage.max),
     type = "n"
)

lines(dt,
      wanted.df$Voltage,
      col = "black",
      type = "l"
      )

# 4) Create the plot4 bottom right chart/plot4_br

# get max Global_reactive_power for ylim
grp.max <- max(as.double(wanted.df$Global_reactive_power))

# create line chart matching example for plot4_tr
plot(dt,
     wanted.df$Global_reactive_power,
     xlab = "datetime",
     ylab = "Global_reactive_power",
     ylim = c(0, grp.max),
     type = "n"
)

lines(dt,
      wanted.df$Global_reactive_power,
      col = "black",
      type = "l"
)

dev.off()
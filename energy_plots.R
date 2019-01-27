library(tidyverse)
library(lubridate)
library(ggplot2)



# MAKE DATA FRAME
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
df <- read.delim2(unz(temp, "household_power_consumption.txt"), sep = ";")
unlink(temp)
df <- as.tbl(df)
df <- mutate(df,
		Date = dmy(Date),
	)

compact_df <- filter(df,
		Date >= "2007-02-01" &
		Date <= "2007-02-02"
	)

compact_df <- mutate(compact_df,
		Global_active_power   = as.numeric(Global_active_power),
		Global_reactive_power = as.numeric(Global_reactive_power),
		Voltage               = as.numeric(Voltage),
		Global_intensity      = as.numeric(Global_intensity),
		Sub_metering_1        = as.numeric(Sub_metering_1),
		Sub_metering_2        = as.numeric(Sub_metering_2),	
		Sub_metering_3        = as.numeric(Sub_metering_3),
		datetime              = paste(Date, Time, sep = " ")
	)

compact_df <- mutate(compact_df,
		datetime              = as_datetime(datetime),
		weekday               = wday(datetime, label = TRUE, abbr = TRUE)
	)

compact_df <- select(compact_df,
		Date,
		Time,
		datetime,
		weekday,
		Global_active_power,
		Global_reactive_power,
		Voltage,
		Global_intensity,
		Sub_metering_1,
		Sub_metering_2,
		Sub_metering_3
	)

compact_df <- arrange(compact_df,
		datetime
	)


#PLOT 1
brks <- c(seq(0, 6, by=0.5), max(compact_df$Global_active_power))
plot1 <- ggplot(data = compact_df,
	 	aes(x = Global_active_power)) +
		geom_histogram(breaks = brks,
				col = "black",
				fill = "red") +
		labs(title = "Global Active Power") +
		labs(x = "Global Active Power (kilowatts)", y = "Frequency")


# PLOT 2
plot2 <-  ggplot(data = compact_df,
		aes(x = datetime, y =Global_active_power)) +
		geom_line() +
		scale_x_datetime(date_breaks = "1 day", date_labels = "%a", minor_breaks = NULL) +
		labs(y = "Global Active Power (kilowatts)", x = " ")


# PLOT3
plot3 <-  ggplot(data = compact_df,
		aes(x = datetime)) +
		geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1")) +
		geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2")) +
		geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3")) +
		scale_x_datetime(date_breaks = "1 day", date_labels = "%a", minor_breaks = NULL) +
		labs(y = "Energy sub metering", x = " ") +
		 scale_colour_manual(name  =" ",
				values= c("Sub_metering_1" = "black",
					"Sub_metering_2"   = "red",
					"Sub_metering_3"   = "blue")) +
		theme(legend.position = c(0.75,0.875))


#PLOT4 (2x2 multiplot)

plot4b <- ggplot(data = compact_df,
		aes(x = datetime, y =Voltage)) +
		geom_line() +
		scale_x_datetime(date_breaks = "1 day", date_labels = "%a", minor_breaks = NULL) +
		labs(y = "Voltage", x = " ")

plot4d <- ggplot(data = compact_df,
		aes(x = datetime, y = Global_reactive_power)) +
		geom_line() +
		scale_x_datetime(date_breaks = "1 day", date_labels = "%a", minor_breaks = NULL) +
		labs(y = "Global Reactive Power", x = " ")
plot4 <- arrangeGrob(grobs = list(plot2, plot4b, plot3, plot4d), cols=2)

ggsave(file="plot1.png", plot1)
ggsave(file="plot2.png", plot2)
ggsave(file="plot3.png", plot3)
ggsave(file="plot4.png", plot4)
# plot3.R
# assumes that spec *.rds are unzipped and in <pwd>/data/
# also assumes that EDA is "quick and dirty" and leaves much of the "window dressing" out

# load libraries
library(sqldf)
library(ggplot2)

# read data files
dat <- readRDS("./data/summarySCC_PM25.rds")
cod <- readRDS("./data/Source_Classification_Code.rds")

# user parameters
output_file <- "./plot3.png"

# select data for plot
sql <- "SELECT year, type, SUM(Emissions) AS Total_Emissions FROM dat WHERE fips = 24510 GROUP BY year, type ORDER BY type ASC, year ASC"
plot_dat <- sqldf(sql)

# Open the device
png(filename=output_file, width = 480, height = 480)

# generate plot
ggplot(plot_dat, aes(year, Total_Emissions))+geom_line(color="firebrick",lwd = 1)+facet_wrap(~type, ncol=2, scales = "free")+stat_smooth(method="lm", se=FALSE, lwd = 0.1, col="dodgerblue4", lty="dashed")

# Close the device
dev.off()
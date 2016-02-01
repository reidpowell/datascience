# plot6.R
# assumes that spec *.rds are unzipped and in <pwd>/data/
# also assumes that EDA is "quick and dirty" and leaves much of the "window dressing" out

# load libraries
library(sqldf)
library(ggplot2)

# read data files
dat <- readRDS("./data/summarySCC_PM25.rds")
cod <- readRDS("./data/Source_Classification_Code.rds")

# user parameters
output_file <- "./plot6.png"

# select data for plot
sql <- "SELECT year, CASE WHEN fips = '24510' THEN 'Baltimore City' ELSE 'LA County' END AS Location, SUM(Emissions) AS Total_Emissions FROM dat JOIN cod ON dat.SCC = cod.SCC WHERE [EI.Sector] LIKE '%-Road%' AND fips IN ('24510','06037') GROUP BY year, fips ORDER BY fips DESC, year ASC"
plot_dat <- sqldf(sql)

# Open the device
png(filename=output_file, width = 480, height = 480)

# generate plot
ggplot(plot_dat, aes(year, Total_Emissions))+geom_line(color="firebrick",lwd = 1)+facet_wrap(~Location, ncol=1)+stat_smooth(method="lm", se=TRUE, lwd = 0.1, col="dodgerblue4", lty="dashed")

# Close the device
dev.off()
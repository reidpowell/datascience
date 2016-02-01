# plot1.R
# assumes that spec *.rds are unzipped and in <pwd>/data/

# user parameters
output_file <- "./plot1.png"

# load libraries
library(sqldf)

# read data files
dat <- readRDS("./data/summarySCC_PM25.rds")
cod <- readRDS("./data/Source_Classification_Code.rds")

# select data for plot
sql <- "SELECT year, SUM(Emissions) AS Total_Emissions FROM dat GROUP BY year"
plot_dat <- sqldf(sql)

# Open the device
png(filename=output_file, width = 480, height = 480)

# generate plot
plot(plot_dat$year, plot_dat$Total_Emissions,
     type = "l",
     col = "red",
     lwd = 3,
     xlab = "Year",
     ylab = "Total Emissions from PM2.5 (tons)",
     main = "Total Emissions from PM2.5, 1999--2008"
    )

# add linear regression line
fit <- lm(Total_Emissions ~ year, data = plot_dat)
abline(fit, lty = "dashed")

# Close the device
dev.off()
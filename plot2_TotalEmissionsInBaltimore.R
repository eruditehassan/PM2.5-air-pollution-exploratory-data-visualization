library(dplyr)
plot2 <- function(){
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  # Subsetting only Baltimore city data
  bal <- subset(nei, fips == "24510")
  # Grouping data by each year
  bal_grouped_year <- group_by(bal, year)
  # Calculating total emissions by each year
  bal_summed_by_year <- summarize(bal_grouped_year, total_emission = sum(Emissions))
  # Creating PNG graphics device
  png("plot2.png", width = 480, height = 480)
  # Plotting the emissions by each year
  with(bal_summed_by_year, plot(year, total_emission, type = 'b', col = 'red',ylab = "Total Emission (tons)", main = "Total Emission of PM2.5 in Baltimore by year"))
  # Closing Graphics device
  dev.off()
}
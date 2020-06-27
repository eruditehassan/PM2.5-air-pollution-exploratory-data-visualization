plot1 <- function(){
  library(dplyr)
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  # Grouping data by each year
  nei_grouped_year <- group_by(nei, year)
  # Calculating total emissions by each year
  nei_summed_by_year <- summarize(nei_grouped_year, total_emission = sum(Emissions))
  # Creating PNG graphics device
  png("plot1.png", width = 480, height = 480)
  # Plotting the emissions by each year
  with(nei_summed_by_year, plot(year, total_emission, type = 'b', col = 'red',ylab = "Total Emission (tons)", main = "Total Emission of PM2.5 by year"))
  # Closing Graphics device
  dev.off()
}
# Importing libraries
library(dplyr)
library(ggplot2)

plot3 <- function(){
  library(dplyr)
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  # Subsetting only Baltimore city data
  bal <- subset(nei, fips == "24510")
  # Grouping data by each year
  bal_grouped <- group_by(bal, year, type)
  # Calculating total emissions by each year for each type
  bal_summed <- summarize(bal_grouped, total_emission = sum(Emissions))
  # Doingthe plotting
  g <- ggplot(bal_summed, aes(x=factor(year), y=total_emission, fill = type))
  g <- g + geom_bar(stat='identity')
  g <- g + facet_grid(. ~ type)
  g <- g + theme(legend.position = "none")
  g <- g + labs(title = "Total Emission of PM2.5 in Baltimore by year and source type", x = "Year", y = "Total Emission (tons)")
  g
  # Saving the plot
  ggsave("plot3.png")
}
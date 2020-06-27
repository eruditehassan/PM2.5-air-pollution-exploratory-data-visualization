# Importing libraries
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

plot6 <- function(){
  library(dplyr)
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  # Using only required columns
  scc_sub <- scc[,c("SCC","EI.Sector")]
  # Shortlisting coal related scc
  scc_sub <- subset(scc_sub, grepl("Vehicles",scc_sub$EI.Sector))
  # Subsetting the nei dataset for only Baltimore and Los Angeles data
  nei_sub <- subset(nei, fips == "24510"| fips == "06037")
  # Creating a new city Column
  nei_sub <- transform(nei_sub, city = factor(fips))
  # Adding meaningful names
  levels(nei_sub$city) <- c("Los Angeles", "Baltimore")
  # Merging dataframes
  nei_scc <- merge(nei_sub, scc_sub, by = "SCC")
  # Grouping data by each year
  grouped <- group_by(nei_scc, year, city)
  # Calculating total emissions by each year for the two cities
  summed <- summarise(grouped, total_emission = sum(Emissions))
  # Splitting the dataframe by cities
  summed_group <- group_by(summed, city)
  split <- group_split(summed_group)
  la <- split[[1]]
  bal <- split[[2]]
  # Calculating percentage change for both cities over the years
  la <- transform(la, percent_change = (total_emission - lag(total_emission)) / lag(total_emission))
  bal <- transform(bal, percent_change = (total_emission - lag(total_emission)) / lag(total_emission))
  # Doing the plotting
  plot1 <- ggplot(bal, aes(x = factor(year), y = total_emission))+ labs(title = "Emission from Vehicle sources in Baltimore",x="Year", y="Total Emission (tons)") + geom_bar(stat = 'identity', fill = '#a2d729') + geom_text(data=bal[-1,],aes(label = scales::percent(percent_change)), vjust = 1)
  plot2 <- ggplot(la, aes(x = factor(year), y = total_emission))+ labs(title = "Emission from Vehicle sources in LA",x="Year", y="Total Emission (tons)") + geom_bar(stat = 'identity', fill = '#29d7c5') + geom_text(data=la[-1,],aes(label = scales::percent(percent_change)), vjust = 1)
  grid.arrange(plot1, plot2, ncol=2)
  # Saving the plot
  ggsave("plot6.png", arrangeGrob(plot1, plot2, ncol = 2))
}
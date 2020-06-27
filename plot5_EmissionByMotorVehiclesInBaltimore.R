# Importing libraries
library(dplyr)
library(ggplot2)

plot5 <- function(){
  library(dplyr)
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  # Subsrtting only baltimore city entries
  nei_sub <- subset(nei, fips == "24510")
  # Using only required columns
  scc_sub <- scc[,c("SCC","EI.Sector")]
  # Shortlisting coal related scc
  scc_sub <- subset(scc_sub, grepl("Vehicles",scc_sub$EI.Sector))
  # Merging dataframes
  nei_scc <- merge(nei_sub, scc_sub, by = "SCC")
  # Grouping data by each year
  veh <- group_by(nei_scc, year)
  # Calculating total emissions by each year
  veh_summed <- summarize(veh, total_emission = sum(Emissions))
  # Doingthe plotting
  g <- ggplot(veh_summed, aes(x=factor(year), y=total_emission))
  g <- g + geom_bar(stat='identity', fill = "forest green")
  g <- g + labs(title = "Total Emission of PM2.5 by motor vehicle sources in Baltimore city", x = "Year", y = "Total Emission (tons)")
  g
  # Saving the plot
  ggsave("plot5.png")
}
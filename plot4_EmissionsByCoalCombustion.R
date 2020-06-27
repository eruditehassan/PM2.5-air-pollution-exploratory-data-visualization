# Importing libraries
library(dplyr)
library(ggplot2)

plot4 <- function(){
  library(dplyr)
  # Loading data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  # Using only required columns
  scc_sub <- scc[,c("SCC","EI.Sector")]
  # Shortlisting coal related scc
  scc_sub <- subset(scc_sub, grepl("Coal",scc_sub$EI.Sector))
  # Merging dataframes
  nei_scc <- merge(nei, scc_sub, by = "SCC")
  # Grouping data by each year
  coal <- group_by(nei_scc, year)
  # Calculating total emissions by each year
  coal_summed <- summarize(coal, total_emission = sum(Emissions))
  # Doingthe plotting
  g <- ggplot(coal_summed, aes(x=factor(year), y=total_emission))
  g <- g + geom_bar(stat='identity', fill = "forest green")
  g <- g + labs(title = "Total Emission of PM2.5 by coal combustion related sources in the US", x = "Year", y = "Total Emission (tons)")
  g
  # Saving the plot
  ggsave("plot4.png")
}
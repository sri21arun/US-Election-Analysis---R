library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(fiftystater)
library(ggplot2)

#using fift_states separately as a .csv file which we later use
write.csv(fifty_states,file="fifty_states.csv")

# Get a shape file of states in the US
# Get the data to be plotted
usa.df <- read.table("fifty_states.csv", header = T, sep = ",")
usa.dat <- read.table("us_2016_election_data.csv", header = T, sep = ",")
usa.dat$State <- tolower(usa.dat$State)
usa.dat$Clinton..<- as.numeric(sub("%","",usa.dat$Clinton..)) #converting percentages to numeric values
usa.dat$Trump..<- as.numeric(sub("%","",usa.dat$Trump..))

fu <- function(cl,tr)  #function to find difference between Clinton and Trump vote share
{ 
  c <-cl -tr
}
usa.dat$newColumn <- mapply(fu, usa.dat$Clinton..,usa.dat$Trump..) #add the data as a new column
# Merge the data with the shape file
colnames(usa.df)[7]="State"
usa.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where thy should be plotted
states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usa.df$State # exclude Hawaii as there is no data for this state
states <- states[subset, ]


#changing state label position
states$x[states$state.abb=="AK"] <- -117.5
states$y[states$state.abb=="AK"] <- 28.5
states$x[states$state.abb=="HI"] <- -104.5
states$y[states$state.abb=="HI"] <- 24.5

#  function that does the plotting 
p <- function(data, brks, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = newColumn), color = "black", size = 0.15) + 
    # 	use shades of blue for states where clinton won and red for states where trump won.
    scale_fill_gradient2(low = "firebrick1",high = "deepskyblue1" , breaks = brks) + 
    #		Add legend
    theme_nothing(legend = TRUE) + labs(title = title, fill="Clinton") + 
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  return(ggp)
}

# Get the break points for different shades
brks.to.use <- seq(-100, 90, by = 20) # give intervals:  
figure.title <- "US Presidential Election"

# Save the map to a file to viewing lso, but it takes
# much longer that way. The ratio of US height to width is 1:9.)
ggsave(p(usa.df, brks.to.use, figure.title), height = 4, width = 4*1.9,
       file = "usa_voters.jpg")

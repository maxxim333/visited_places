# Load required libraries
install.packages("ggplot2")
install.packages("maps")
install.packages("mapprooj")
install.packages("rworldmap")
install.packages("rworldxtra")
install.packages("deldir")
install.packages("spatstat")

library(ggplot2)
library(maps)
library(mapproj)
library(grid)
library(rworldmap)
library(rworldxtra)
library(deldir)  # Load the deldir library for computing Delaunay triangulation
library(spatstat)



# Extract the coordinates from the 9th column and split them
data <- read.csv("/Users/maxxim333/Downloads/zeemaps06062023_.csv", sep = ";")
coordinate_strings <- data[, 10]
coordinates <- strsplit(coordinate_strings, ",")

# Create a data frame with separate x and y vectors
coordinates_df <- data.frame(longitude = sapply(coordinates, function(coord) as.numeric(coord[1])),
                             latitude = sapply(coordinates, function(coord) as.numeric(coord[2])))

# Print the resulting data frame
print(coordinates_df)



# Get the world map
worldMap <- getMap(resolution = "high")

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria",
                   "Czech Rep.","France",
                   "Germany","Greece","Italy",
                   "Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Slovenia","Spain",
                    "Morocco", "United Kingdom", "Ukraine", "Switzerland", "S. Korea", "Japan")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Create a new dataframe with points having latitude < 100
filtered_df <- coordinates_df[coordinates_df$latitude < 100, ]

# Compute Delaunay triangulation
triangulation <- deldir(filtered_df$latitude, filtered_df$longitude)

# Extract convex hull polygon
convex_hull_indices <- chull(filtered_df$latitude, filtered_df$longitude)

## Create data frame with convex hull coordinates
convex_hull_df <- data.frame(
  long = filtered_df$latitude[convex_hull_indices],
  lat = filtered_df$longitude[convex_hull_indices]
)


P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = 
                                                        value),
                             colour = "black", linewidth = 0.1) +
  coord_map(xlim = c(-20, 150),  ylim = c(20, 60)) +
  geom_point(data = coordinates_df, aes(x = latitude, y = longitude), color = "red", size = 0.1)

P <- P +
  geom_polygon(data = convex_hull_df, aes(x = long, y = lat),
               fill = "transparent", color = "green", linewidth = 0.8)


P
dev.off()

ggsave("/Users/maxxim333/Desktop/map_plot.png", plot = P, dpi = 2000)

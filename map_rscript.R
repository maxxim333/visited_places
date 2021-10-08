###############################
#R version 4.1.1
##############################
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#Package "sf" was installed manually from https://cran.r-project.org/web/packages/sf/index.html verssion  sf_1.0-3.zip 
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
install.packages("rgeos")
library(ggplot2) # Load the librarie (you have to do this one on each new session)
library("rgeos")


world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

#Open file
visited2 <- read.delim("C:/Users/Maksym/Downloads/zeemaps08102021.csv", 
                       sep = ",", header = T)
head(visited2)

#Extract coordinates
coordinates<-visited2['Description']
coordinates

#Extract names
names<-visited2['Name']
names

#Put coordinates and names in the right format
library(tidyr)
coordinates_split <- separate(data = coordinates, col = Description, into = c("latitude", "longitude"), sep = ",")
coordinates_split
class(coordinates)

names_split <- separate(data = names, col = Name, into = c("title", "details"), sep = "\\|")
names_split


sites <- st_as_sf(data.frame(longitude=coordinates_split["longitude"],latitude=coordinates_split["latitude"]), coords = c("longitude", "latitude"), crs = 4326, 
                  agr = "constant")
class(sites)

class(coordinates_split)
#Transform to vectors
long_vector<-dplyr::pull(coordinates_split, longitude)
lat_vector<-dplyr::pull(coordinates_split, latitude)
names_vector<-dplyr::pull(names_split, title)


long_vector
lat_vector
names_vector

(florida <- ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 1.5, shape = 23, fill = "green") +
    
    coord_sf(xlim = c(-17.35, 34), ylim = c(25, 55)) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))

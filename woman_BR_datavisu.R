#==========================================#
# MAP OF WOMEN DEATH IN BRAZILIAN CITIES   #
#==========================================#
# DATA VISUALIZATION                       #
#==========================================#
# autor: Claudio A. Monteiro               #
# email: claudiomonteiro@protonmail.com    #
#------------------------------------------#
# #usefreesoftware                         #
#------------------------------------------#
# instalar pacotes necessarios
# install.packages(c("readr","plyr", "rgdal", "ggplot2", "ggmap", "maps", "mapdata", "raster"), dependencies = T )

# carregar pacotes
library(readr)
library(plyr)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(raster)

# carregar dados
shp_bracity <- shapefile("Original Data/Geodata/bra_cities_2010/municipios_2010.shp")

# load data
library(readr)
data_women <- read_delim("Data Analysis/data_women.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                         trim_ws = TRUE)

#---- create variable ----#
data_women$prop_mortfem_2015_2 <- (data_women$abs_mort_fem_2015 / data_women$pop_2010) *10000
data_women$abs_10to15 <- data_women$abs_mort_fem_2010 + data_women$abs_mort_fem_2011 +
  data_women$abs_mort_fem_2012 + data_women$abs_mort_fem_2013 + data_women$abs_mort_fem_2014 +
  data_women$abs_mort_fem_2015

#------ merge data and shape ------#
shp_bracity$city_code2 <- shp_bracity$codigo_ibg
shp_bracity <- merge(shp_bracity, data_women, by = "city_code2", all = T)
  
#==== MAP ====#

  library(ggrepel)
  library(purrr)
  library(ggplot2)
  library(stringi)
  
  # tranformar shapefile em polygonsdataframe
  fort_brcity <- fortify(shp_bracity, region = "id")
  fort_data1 <- join(fort_brcity, shp_bracity@data, by = "id")
  
  ggplot(data = fort_data1, aes(map_id = fort_data1$nome)) + 
    geom_map(aes(fill = fort_data1$prop_mortfem_2015_2), colour = "grey",  map = fort_data1) +
    # scale_fill_gradient(colours=inferno(10, alpha = 1, begin = 1, end = 0))+
    scale_fill_gradient(name = "" , low=	"#ffad60", high= "#4c0000")+
    coord_fixed(1) +
    #labs(title = title)
    theme_nothing(legend = T)

setwd("Outputs")

#===== PROPORTION
  plotbr1 <- ggplot(fort_data1,  aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = fort_data1$prop_mortfem_2015_2 
                     #color = NA
    )) + 
    scale_fill_gradient(name = "Proportion" , low=	"lightblue", high= "darkblue")+
    coord_equal()  +
    labs(title = "Women Death by 1000 hab in Brazil (2015)")+
    theme_nothing(legend = T)
  plotbr1

  ggsave("plotbr1.png", plotbr1, width = 7, height = 7, units = "in")
  
#==== ABSOLUTE

  plotbr2 <- ggplot(fort_data1,  aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = fort_data1$abs_10to15 
                     #color = NA
    )) + 
    scale_fill_gradient(name = "Death" , low=	"lightblue", high= "darkblue")+
    coord_equal()  +
    labs(title = "Women Death by City in Brazil (2010-2015)")+
    theme_nothing(legend = T)
  plotbr2
  
  ggsave("plotbr2.png", plotbr2, width = 7, height = 7, units = "in")
  

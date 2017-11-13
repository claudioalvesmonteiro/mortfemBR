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
data_women$
#---- carregar shape dos bairros ----#

# converter shapefile em dataframe para ser usado pelo ggplot2
shp_bracity@data$id <- rownames(shp_bracity@data)
shapefile_points <- fortify(shp_bracity, region = "id")
 
# juntar dados e shapedata
shp_bracity <- join(shapefile_points, shp_bracity@data, by="id")

#------ merge data and shape ------#
shp_bracity$city_code2 <- shp_bracity$codigo_ibg
  
  
#==== MAP ====#

  library(ggrepel)
  library(purrr)
  library(ggplot2)
  library(stringi)
  
  
  shp_data <- merge(shp_bracity, data_women, by = "city_code2", all = T)
  
  
  # tranformar shapefile em polygonsdataframe
  data_fortity <- fortify(shp_data, region = "nome")
  Localidade <- shp_dat
  
  # extrair centroides dos poligonos
  centroids.df <- as.data.frame(coordinates(shp_data))
  names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column Localidades
  
  # This shapefile contained population data, let's plot it.
  variavel <- shp_data@data$variavel
  nomes_centroides <- shp_data$bairros_detasq
  
  map_dataframe <- data.frame(Localidade, variavel, centroids.df, nomes_centroides)
  
  ggplot(data = shp_data, aes(map_id = Localidade)) + 
    geom_map(aes(fill = shp_data$variavel), colour = grey(0.85),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) + 
    # scale_fill_gradient(colours=inferno(10, alpha = 1, begin = 1, end = 0))+
    scale_fill_gradient(name = "" , low=	"#ffad60", high= "#4c0000")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude),
                     size = 3, color = "black") + #add labels at centroids
    coord_fixed(1) +
    #labs(title = title)
    theme_nothing(legend = T)
  return(plot)
}

acid2 <- mapa.funcao(shp_recife, acid_bairro2015, acid_bairro2015$Freq)
ggsave("scid_bairros_2015_2.png", acid2, width = 6, height = 9, units = "in")
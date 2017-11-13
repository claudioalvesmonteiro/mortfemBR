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

#==== MAP FUNCTION ====#

mapa.funcao <- function(shape, data, variable) { 
  library(ggrepel)
  library(purrr)
  library(ggplot2)
  library(stringi)
  
  
  # function to create merge string based on similarity
  best_match= function(string_vector,string_replacement){
    s<-string_replacement %>% 
      purrr::map_int(~{
        .x %>% 
          RecordLinkage::levenshteinSim(string_vector) %>%
          match(max(.),.)
      })
    string_vector[s] = string_replacement
    return(string_vector)
  }
  
  data$EBAIRRNOME = data$Localidade
  data$EBAIRRNOME = toupper(data$EBAIRRNOME)
  data$EBAIRRNOME = stri_trans_general(data$EBAIRRNOME , "Latin-ASCII")
  data$EBAIRRNOME = best_match(data$EBAIRRNOME, shape$EBAIRRNOME)
  data$variavel <- variable
  data$variavel[is.na(data$variavel)] <- 0
  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa
  shp_data <- shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq <- 1
  #shp_data$bairros_detasq[1:5] <- ""
  shp_data$bairros_detasq[c(length(shp_data)-5):c(length(shp_data))] <- ""
  
  shp_data$bairros_detasq <- with(shp_data, paste0(shp_data$bairros_detasq, shp_data$Localidade))
  shp_data$bairros_detasq_cod <- grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] <- ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity <- fortify(shp_data, region = "Localidade")
  Localidade <- shp_data@data$Localidade
  
  # extrair centroides dos poligonos
  centroids.df <- as.data.frame(coordinates(shp_data))
  names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column Localidades
  
  # This shapefile contained population data, let's plot it.
  variavel <- shp_data@data$variavel
  nomes_centroides <- shp_data$bairros_detasq
  
  map_dataframe <- data.frame(Localidade, variavel, centroids.df, nomes_centroides)
  
  plot <- ggplot(data = map_dataframe, aes(map_id = Localidade)) + 
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
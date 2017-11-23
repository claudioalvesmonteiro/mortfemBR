#====================================================#
# MAP OF WOMEN AGRESSION DEATH IN BRAZILIAN CITIES   #
#====================================================#
# autor: Claudio A. Monteiro                         #
# email: claudiomonteiro@protonmail.com              #
#----------------------------------------------------#
# #usefreesoftware                                   #
#----------------------------------------------------#



#==================================#
# Functions for manipulating data  #

# function for load multiple xls datasets 
multi_xls <- function(file_list)  {
  
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read_excel(file)}
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <-read_excel(file)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  return(dataset)
}

#=====================#
# Women death by age  #

# set working directory
setwd("~/Documents/Claudio/untitled folder/womandeathBR/Original Data/Woman Death by Age/")

# load data
file_list1 <- list.files()
death_age_data <- multi_xls(file_list1)

# extrair numeros da variavel municipio e atribuir a var codigo
death_age_data$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", death_age_data$Município))

# filter NA data
death_age_data <- death_age_data[!is.na(death_age_data$code_muni), ]
death_age_data <- death_age_data[!death_age_data$code_muni == 0, ]

# 





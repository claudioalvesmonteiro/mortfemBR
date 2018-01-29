#====================================================#
# MAP OF WOMEN AGRESSION DEATH IN BRAZILIAN CITIES   #
#====================================================#
# autor: Claudio A. Monteiro                         #
# email: claudiomonteiro@protonmail.com              #
#----------------------------------------------------#
# #usefreesoftware                                   #
#----------------------------------------------------#

# install.packages
# install.packages(c("readxl", "readODS", "stringi", "dplyr"))

# load packages
library(stringi)
library(dplyr)

#==================================#
# Functions for manipulating data  #

# function for load multiple xls datasets 
multi_xls <- function(file_list)  {
  library(readxl)
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

# function for load multiple csv datasets 
multi_csv <- function(file_list)  {
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.csv(file, sep = ";",  stringsAsFactors = F)}
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <-read.csv(file, sep = ";", stringsAsFactors = F)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  return(dataset)
}
#=====================#
# Women death by age  #

# set working directory
setwd("Original Data/Woman Death by Age/")

# load data
file_list1 <- list.files()
death_age_data <- multi_xls(file_list1)

# extrair numeros da variavel municipio e atribuir a var codigo
death_age_data$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", death_age_data$Município))

# filter NA data
death_age_data <- death_age_data[!is.na(death_age_data$code_muni), ]
death_age_data <- death_age_data[!death_age_data$code_muni == 0, -1]

# replace '-' for 0
death_age_data <- data.frame(lapply(death_age_data, function(x) { gsub("-", "0", x) }), stringsAsFactors = F)
death_age_data <- data.frame(lapply(death_age_data, function(x)  as.numeric(x) ))

# rename columns
colnames(death_age_data) <- c("wom_violentdeath_1", "wom_violentdeath_1to4", "wom_violentdeath_5to9", "wom_violentdeath_10to14",  "wom_violentdeath_15to19",
                              "wom_violentdeath_20to29", "wom_violentdeath_30to39", "wom_violentdeath_40to49", "wom_violentdeath_50to59", 
                              "wom_violentdeath_60to69", "wom_violentdeath_70to79", "wom_violentdeath_80", "wom_violentdeath_NAage","wom_violentdeath_total", "year", "code_muni")

#==========================#
# Women population by age  #

# set working directory
setwd("~/GitProjects/womendeathBR/womendeathBR/Original Data/Woman Population by Age")

# load data
file_list4 <- list.files()
population_age_data <- multi_xls(file_list4)

# extrair numeros da variavel municipio e atribuir a var codigo
population_age_data$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", population_age_data$Município))

# filter NA data
population_age_data <- population_age_data[!is.na(population_age_data$code_muni), ]
population_age_data <- population_age_data[!population_age_data$code_muni == 0, -1]

# replace '-' for 0
population_age_data <- data.frame(lapply(population_age_data, function(x) { gsub("-", "0", x) }), stringsAsFactors = F)
population_age_data <- data.frame(lapply(population_age_data, function(x)  as.numeric(x) ))

# rename columns
colnames(population_age_data) <- c("wom_pop_0to4", "wom_pop_5to9", "wom_pop_10to14", "wom_pop_15to19",  "wom_pop_20to24",
                              "wom_pop_25to29", "wom_pop_30to34", "wom_pop_35to39", "wom_pop_40to44",   "wom_pop_45to49",
                              "wom_pop_50to54",   "wom_pop_55to59", "wom_pop_60to64", "wom_pop_65to69","wom_pop_70to74", 
                              "wom_pop_75to79", "wom_pop_80", "wom_pop_total", "year", "code_muni")

# clean data
population_age_data <- population_age_data[!population_age_data$wom_pop_total == 0,]

#================================================#
# Proportion of Families With Woman Responsible  #

# set working directory
setwd("~/GitProjects/womendeathBR/womendeathBR/Original Data/Proportion of Families With Woman Responsible")

# load data
file_list5 <- list.files()
woman_family_data <- multi_csv(file_list5)

# rename columns
colnames(woman_family_data) <- c("code_muni", "municipio", "prop_wom_headfamily")

# replace ',' by '.'
woman_family_data$prop_wom_headfamily <- gsub(",", ".", woman_family_data$prop_wom_headfamily)

# as numeric
woman_family_data$prop_wom_headfamily <- as.numeric(woman_family_data$prop_wom_headfamily)
woman_family_data$code_muni <- as.numeric(woman_family_data$code_muni)

# remove NA and duplicated
woman_family_data <- woman_family_data[!is.na(woman_family_data$code_muni),]
woman_family_data <- woman_family_data[!duplicated(woman_family_data$code_muni),]

#======================================#
# AtlasBrasil Data
#--------------------------------------#
# Inequality, Sanitation, Child Death,
# Income per Capita
#--------------------------------------#

# set working directory
setwd("~/GitProjects/womendeathBR/womendeathBR/Original Data/Data From AtlasBrasil")

# import
atlas_data <- read_xls("data_atlasbrasil.xls")

# remove inwanted columns
atlas_data <- atlas_data[,-c(1:2)]

# rename columns
colnames(atlas_data) <- c("code_muni", "Theil-L", "Gini", "%_pop_with_sanitation", "child_death",  "income_percapita" )


#====================#
# merge data     

# merge death by age and population by age
women_death_database <- merge(death_age_data, population_age_data, by = c("code_muni", "year"))  
women_death_database <- women_death_database[!duplicated(women_death_database[,1:2]),]

# merge database and woman head of family
women_death_database <- merge(women_death_database, woman_family_data, by = "code_muni")

# merge database and atlasbrasil data
women_death_database <- merge(women_death_database, atlas_data, by = "code_muni")

#===================#
# create variables 

#------------------------------#
# women death by 10.000 hab    #

women_death_database <- mutate(women_death_database, wom_death_100.000hab = (wom_violentdeath_total / wom_pop_total) * 100000)
























#====================================================#
# MAP OF WOMEN AGRESSION agression IN BRAZILIAN CITIES   #
#====================================================#
# autor: Claudio A. Monteiro                         #
# email: claudiomonteiro@protonmail.com              #
#----------------------------------------------------#
# #usefreesoftware                                   #
#----------------------------------------------------#

# install.packages
# install.packages(c("readxl", "readODS", "stringi", "dplyr"))

# load packages
library(stringi); library(stringr); library(dplyr)

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
# Women agression by age  

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/Woman Death by Age")

# load data
file_list1 <- list.files()
agression_age_data <- multi_xls(file_list1)

# create code of city_name
agression_age_data$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", agression_age_data$Município))

# filter NA data
agression_age_data <- agression_age_data[!is.na(agression_age_data$code_muni), ]
agression_age_data <- agression_age_data[!agression_age_data$code_muni == 0, -1]

# replace '-' for 0
agression_age_data <- data.frame(lapply(agression_age_data, function(x) { gsub("-", "0", x) }), stringsAsFactors = F)
agression_age_data <- data.frame(lapply(agression_age_data, function(x)  as.numeric(x) ))

# rename columns
colnames(agression_age_data) <- c("wom_violentagression_1", "wom_violentagression_1to4", "wom_violentagression_5to9", "wom_violentagression_10to14",  "wom_violentagression_15to19",
                               "wom_violentagression_20to29", "wom_violentagression_30to39", "wom_violentagression_40to49", "wom_violentagression_50to59", 
                               "wom_violentagression_60to69", "wom_violentagression_70to79", "wom_violentagression_80", "wom_violentagression_NAage","wom_violentagression_total", "year", "code_muni")

#==========================#
# Women population by age  #

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/Woman Population by Age")

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


#================================================#
# Proportion of Families With Woman Responsible  #

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/Proportion of Families With Woman Responsible")

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

#====== Urbanization ======#

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/Urbanization 2010")

# load data
file_list6 <- list.files()
urbanization_data <- multi_csv(file_list6)

# clean variable
urbanization_data <- mutate(urbanization_data, urbanization = urbanization_data$Cod..Loc.)
urbanization_data$urbanization <- str_replace(urbanization_data$urbanization, ",", ".")

# remove duplicated
urbanization_data <- urbanization_data[!duplicated(urbanization_data$Cod..Loc.), ] 
urbanization_data$code_muni <- urbanization_data$Cod..Loc.


#====== Proportion of Woman Income to Men Income ======#

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/Proportion of Woman Income to Men Income")

# load data
list_wtom_income <- list.files()
data_wtom_income <- multi_xls(list_wtom_income)

#======================================#
# AtlasBrasil Data
#--------------------------------------#
# Inequality, Sanitation, Child agression,
# Income per Capita
#--------------------------------------#

# set working directory
setwd("~/Documents/git_projects/womendeathBR/original_data/atlas_brasil")

# import
atlas_data <- read_xls("data_atlasbrasil.xls")

# remove columns
atlas_data <- atlas_data[,-c(1:2)]

# rename columns
colnames(atlas_data) <- c("code_muni", "Theil-L", "Gini", "%_pop_with_sanitation", "child_agression",  "income_percapita" )

#====================#
# merge data     

# merge agression by age and population by age
women_agression_database <- merge(agression_age_data, population_age_data, by = c("code_muni", "year"))  
women_agression_database <- women_agression_database[!duplicated(women_agression_database[,1:2]),]

# merge database and woman head of family
women_agression_database <- merge(women_agression_database, woman_family_data, by = "code_muni")

# merge database and atlasbrasil data
women_agression_database <- merge(women_agression_database, atlas_data, by = "code_muni")

# merge database and urbanization data
women_agression_database <- merge(women_agression_database, urbanization_data, by = "code_muni")

#===== create variables =====#

#----- women agression by 10.000 hab -----#    
women_agression_database <- mutate(women_agression_database, wom_agression_10.000hab = (wom_violentagression_total / wom_pop_total) * 10000)

#----- pop young woman -----#    
women_agression_database <- mutate(women_agression_database, w_young_prop = wom_pop_15to19 + wom_pop_20to24 + wom_pop_25to29 / wom_pop_total)

#==== save data =====#
women_agression_database <- women_agression_database[,c(1, 2, 35:44)]
write.csv(women_agression_database, file = "women_agression_database.csv", row.names = F)






















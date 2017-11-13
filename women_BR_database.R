#==========================================#
# MAP OF WOMEN DEATH IN BRAZILIAN CITIES   #
#==========================================#
# autor: Claudio A. Monteiro               #
# email: claudiomonteiro@protonmail.com    #
#------------------------------------------#
# #usefreesoftware                         #
#------------------------------------------#

# opa

#-------------------------------------#
# Criar Banco de Mortalidade Feminina #
#-------------------------------------#

# definir pasta de trabalho
setwd("C:/Users/MONTEIRO_/Documents/Pesquisa/Mortalidade Feminina")

# carregar banco de mortalidade feminina (2010-2014)
data_fem <- read.csv("data_mort_fem.csv", sep = ";", stringsAsFactors = F)

# extrair numeros da variavel municipio e atribuir a var codigo
data_fem$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", data_fem$municipio))


#carregar banco mortalidade feminina (2015)
data_fem_2015 <- read.csv("data_mort_fem_2015.csv", sep= ";", stringsAsFactors = F)

# extrair numeros da variavel municipio e atribuir a var codigo 
data_fem_2015$code_muni <- as.numeric(gsub("([0-9]+).*$", "\\1", data_fem_2015$municipio))

# remover municipios do banco
data_fem <- data_fem[,-1]

# ordenar e mergir bancos
data_fem <- data_fem[order(data_fem$code_muni),]
data_fem_2015 <- data_fem_2015[order(data_fem_2015$code_muni),]

data_fem <- merge(data_fem, data_fem_2015, by = "code_muni")

#----------------------------------#
# Capturar variaveis de interesse  #
#----------------------------------#

# definir pasta de trabalho para captura
setwd("C:/Users/MONTEIRO_/Documents/Analytique/Consultoria/Carol/Elaboração Banco de Dados/metadados")

# ler banco 
data_var <- read.csv("data_carol_atualizado3.csv",  sep = ";", stringsAsFactors = F)

#capturar variaveis de interesse
data_var <- data_var[,c("Municipio", "municipio", "code_muni", "code_muni2", "IDHM", 
                        "log_distancia_capital", "Pop_2010", "log_pop_2010", "regiao",
                        "MUNICIPIOX", "MUNICIPIOY", "CAPITALX", "CAPITALY")]

# ler outro banco
data_metro <- read.csv("dados_metropolitano.csv", sep = ";", stringsAsFactors = F)


#----------------------------#
# Mergir Bancos e Variaveis  #
#----------------------------#

data_var <- merge(data_var, data_metro, by = "code_muni2")


data_fem <- merge(data_var, data_fem, by = "code_muni")

#----- -------------------#
# Manipular/Limpar banco  #
#-------------------------#

data_fem <- data_fem[, - 34]

# selecionar e ordenar apenas variaveis de interesse
data_fem <- data_fem[, c("code_muni","Municipio","UF","Nome_UF",
                         "X2010", "X2011", "X2012", "X2013", "X2014","obitos", 
                         "IDHM", "Pop_2010", "log_pop_2010", "log_distancia_capital",
                         "code_muni2", "municipio.x","metropolitano","MicrorregiãoGeográfica",
                         "MicrorregiãoGeográfica_Nome", "Distrito","Distrito_Nome" ,"regiao",
                         "MUNICIPIOX", "MUNICIPIOY", "CAPITALX", "CAPITALY"
)]


# carregando pacote de manipulacao de string
library(stringr)

# retirar simbolo "-" de variaveis e colocar 0
manipul_data <- data.frame(sapply(data_fem[,c(5:10)], function(x) # substituir - por '0' em todo o banco
  str_replace(x, pattern = "-", "0")))

# retirar simbolo "," e colocar "." em var IDHM
data_fem$IDHM <- str_replace(data_fem$IDHM, pattern = ",", ".")

# transformar variaveis chr em numericas
manipul_data <- data.frame(sapply(manipul_data, as.character), stringsAsFactors = F)
manipul_data <- data.frame(sapply(manipul_data, as.numeric))

# mergir bancos manipulados
data_fem <- data.frame(manipul_data, data_fem[,c(1:4, 11:26)])

# organizar variaveis no banco
data_fem_final <- data_fem[, c("code_muni","Municipio","UF","Nome_UF",
                               "X2010", "X2011", "X2012", "X2013", "X2014","obitos", 
                               "IDHM", "Pop_2010", "log_pop_2010", "log_distancia_capital",  "code_muni2", 
                               "municipio.x","metropolitano","MicrorregiãoGeográfica", "MicrorregiãoGeográfica_Nome",
                               "regiao","MUNICIPIOX", "MUNICIPIOY", "CAPITALX", "CAPITALY"
)]

# padronizar nomes das variaveis no banco
colnames(data_fem_final) <- c("code_muni","Municipio","UF","UF_name",
                              "mort_fem_2010", "mort_fem_2011", "mort_fem_2012", "mort_fem_2013", "mort_fem_2014","mort_fem_2015",
                              "IDHM", "pop_2010", "log_pop_2010", "log_distancia_capital", "code_muni2",
                              "municipio","metropolitano","microregiao_code","microregiao_name",
                              "regiao", "lon_muni", "lat_muni", "lon_capital", "lat_capital"
)

#----transformar mortalidade absoluta em prop----#
mort_data <- data_fem_final[,5:10]
mort_data <- data.frame(apply(mort_data, MARGIN = 2, function(x) x / data_fem$Pop_2010 * 10000) )

# remover linha missing cases
data_fem_final <- data_fem_final[ - 5605,]

# tranformar chr em numeric
data_fem_final$IDHM <- as.numeric(data_fem_final$IDHM)


#------------------------------#
# Finalizar e padronizar banco #
#------------------------------#

data_fem_final <- data.frame(data_fem_final, mort_data)


data_fem_final <- data_fem_final[,c("code_muni","Municipio","UF","UF_name",
                                    "mort_fem_2010", "mort_fem_2011", "mort_fem_2012", "mort_fem_2013", "mort_fem_2014","mort_fem_2015",
                                    "mort_fem_2010.1", "mort_fem_2011.1", "mort_fem_2012.1", "mort_fem_2013.1", "mort_fem_2014.1","mort_fem_2015.1",
                                    "IDHM", "pop_2010", "log_pop_2010", "log_distancia_capital", "code_muni2",
                                    "municipio","metropolitano","microregiao_code","microregiao_name",
                                    "regiao", "lon_muni", "lat_muni", "lon_capital", "lat_capital")]



colnames(data_fem_final) <- c("city_code","City_Name","UF_code","UF_name",
                              "abs_mort_fem_2010", "abs_mort_fem_2011", "abs_mort_fem_2012", "abs_mort_fem_2013", "abs_mort_fem_2014","abs_mort_fem_2015",
                              "prop_mort_fem_2010", "prop_mort_fem_2011", "prop_mort_fem_2012", "prop_mort_fem_2013", "prop_mort_fem_2014","prop_mort_fem_2015",
                              "HDI", "pop_2010", "log_pop_2010", "to_capital_log", "city_code2",
                              "city_name","metrop","microreg_code","microreg_name",
                              "regiao", "city_lon", "city_lat", "lon_capital", "lat_capital"
)


# visualizar estrutura do banco final
str(data_fem_final)

# salvar banco
setwd("C:/Users/MONTEIRO_/Documents/Pesquisa/Mortalidade Feminina")

write.table(data_fem_final, file="Banco Mortalidade Feminina.csv", 
            fileEncoding = "latin1", sep = ";", dec = ".")

data_fem <- read.csv("Banco Mortalidade Feminina.csv", 
                     fileEncoding = "latin1", sep = ";", dec = ".")




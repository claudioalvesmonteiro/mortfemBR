#====================================================#
# MAP OF WOMEN AGRESSION DEATH IN BRAZILIAN CITIES   #
#====================================================#
# DATA ANALYSIS                                      #
#====================================================#
# autor: Claudio A. Monteiro                         #
# email: claudiomonteiro@protonmail.com              #
#----------------------------------------------------#
# #usefreesoftware                                   #
#----------------------------------------------------#

# install packages
install.packages(c("stargazer", "QuantPsyc"))

# load packages
library(stargazer); library(QuantPsyc)

wom_death_model1 <- glm(data = women_agression_database, 
                        wom_death_100.000hab ~
                        `%_pop_with_sanitation`+
                        income_percapita +
                        child_death +
                        Gini + 
                        prop_wom_headfamily
                       )


summary(wom_death_model1)
lm.beta(wom_death_model1)
stargazer(wom_death_model1, type = "text", title = "Results", style = "ajps", 
          column.labels  = c("Modelo 1"))  



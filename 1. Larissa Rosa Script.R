#                            POF 2017-2018
#Brazil household budget survey - 2017/2018 (Pesquisa de orcamento familiar - POF)
#Code developed by Larissa Rosa for the analysis of her undergraduate thesis: 
#PUBLIC EXPENDITURES IN BRAZIL: 
#An analysis of gender and race transversality in the federal budget


# First it was necessary to run the file “Microdata Reading - R.R.”
# which is located in the compressed file “Programas_de_Leitura.zip”
# This step was necessary to generate the files with the .rds extension
# corresponding to the .txt files containing the POF microdata


setwd(choose.dir()) #Set the path to the database: "Dados_20230713"

#   Initial definitions  #####
# Loading the necessary libraries 
packages <- c('car', 'data.table', 'DescTools', 'devtools', 'dplyr', 'foreign', 'ggeffects', 'gglorenz',
              'ggplot2', 'ggthemes', 'hrbrthemes', 'lubridate', 'MASS', 'openxlsx',
              'plotly', 'psych', 'QuantPsyc', 'readxl','reldist', 'reshape2', 'scales', 
              'sjPlot', 'sqldf', 'srvyr', 'survey', 'tidyr', 'writexl')

#If you haven’t yet installed the packages, install them before loading:
#install.packages(packages)

# Downloading all the packages on the list
lapply(packages, require, character.only = TRUE)

# READING OF THE REGISTER - ESTIMATED RENT ####

# READING of all entries
# Expenditure figures are itemised and then calculated as a monthly average.

aluguel_estimado <- readRDS("ALUGUEL_ESTIMADO.rds")

#  Annualisation and expansion of the figures used to obtain the results
#  (variable V8000_defla).

# a) To annualise, we use the field “factor_annualisation”. In this record,
#    which contains information relating to monthly rent figures, we also use
#    the variable V9011 (number of months).
#    The values are annualised and then an average monthly figure is calculated.

# b) To extrapolate, we use the variable “peso_final”.

# c) Subsequently, the result is divided by 12 to obtain the monthly estimate.


alu_estimado <- 
  transform( aluguel_estimado,
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [3] 
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" ,"valor_mensal") ]
rm(aluguel_estimado)
alu_estimado [is.na(alu_estimado)] <- 0

# The columns (“UF”, “ESTRATO_POF”, “TIPO_SITUACAO_REG”, “COD_UPA”, “NUM_DOM”, “NUM_UC”, “PESO_FINAL”)
# are the KEY for the search and will be used to join the databases.

# READING OF THE REGISTER – COLLECTIVE EXPENDITURE ####

despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds")

#   Annualisation and expansion of the figures used to obtain the results
#   (variables V8000_defla and V1904_defla). Variable V1904_defla refers to expenditure
#   on ‘INSS and other labour contributions’, which is used in the group
#   ‘Other current expenditure’.

#    In the specific case of Tables 10 and 19, where the information relates to monthly figures,
#    we also use variable V9011 (number of months).
#    The figures are annualised and then averaged to obtain a monthly average.

desp_coletiva <- 
  transform( despesa_coletiva,
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             ) , 
             inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" , "valor_mensal","inss_mensal") ]

desp_coletiva [is.na(desp_coletiva)] <- 0
rm(despesa_coletiva)


# READING OF THE REGISTER – COLLECTIVE REGISTER (POF 3 Questionnaire) ####

caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")

cad_coletiva <- 
  transform( caderneta_coletiva,
             valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" , "valor_mensal" ) ]

cad_coletiva [is.na(cad_coletiva)] <- 0
rm(caderneta_coletiva)


# READING OF THE REGISTER - INDIVIDUAL SPENDING ####

despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")

desp_individual <-
  transform( despesa_individual,
             valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             )
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" , "valor_mensal" ) ]
desp_individual [is.na(desp_individual)] <- 0
rm(despesa_individual)


# READING OF THE REGISTER - LABOUR PRODUCTIVITY #### 

rendimento_trabalho <- readRDS("RENDIMENTO_TRABALHO.rds")

rend_trabalho <- 
  transform( 
    subset(rendimento_trabalho,
           !is.na(V8500_DEFLA)
    ) ,
    valor_mensal=(V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "V9001" , "valor_mensal" ) ] 


rm(rendimento_trabalho)

rendimento_trabalho <- readRDS("RENDIMENTO_TRABALHO.rds")

rend_trabalho <-
  transform( rendimento_trabalho,
             prev_pub_mensal=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
             imp_renda_mensal=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
             iss_mensal=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" , "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal" ) ]

rend_trabalho [is.na(rend_trabalho)] <- 0
rm(rendimento_trabalho)


# READING OF THE REGISTER - OTHER INCOME ####

outros_rendimentos <- readRDS("OUTROS_RENDIMENTOS.rds")

outros_rend <-
  transform( outros_rendimentos,
             deducao_mensal = ifelse( QUADRO==54,
                                      (V8501_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                      (V8501_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
             ) 
  ) [ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
           "PESO_FINAL", "V9001" , "deducao_mensal" ) ]
outros_rend  [is.na(outros_rend )] <- 0

rm(outros_rendimentos)


# MERGING OF RECORDS ####
# [1] Comprising the component items of the general expenditure table.

# [2] Conversion of the item code (variable V9001) into a 5-digit number, to bring it into line with
#     the format of the codes contained in the table translation files.

desp_coletiva_n <- cbind( desp_coletiva , NA , NA , NA , NA )
names(desp_coletiva_n) <- c( names( desp_coletiva) , 
                             "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal" , "deducao_mensal" )
rm(desp_coletiva)

cad_coletiva_n <- cbind( cad_coletiva , NA , NA , NA , NA , NA )
names(cad_coletiva_n) <- c( names( cad_coletiva) , 
                            "inss_mensal" , "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal" , "deducao_mensal" )
rm(cad_coletiva)

desp_individual_n <- cbind( desp_individual , NA , NA , NA , NA , NA )
names(desp_individual_n) <- c( names( desp_individual) , 
                               "inss_mensal" , "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal" , "deducao_mensal" )
rm(desp_individual)

alu_estimado_n <- cbind( alu_estimado , NA , NA , NA , NA , NA )
names(alu_estimado_n) <- c( names( alu_estimado) , 
                            "inss_mensal" , "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal" , "deducao_mensal" )
rm(alu_estimado)

rend_trabalho_n <- rend_trabalho[1:8]

rend_trabalho_n$valor_mensal <- NA
rend_trabalho_n$inss_mensal <- NA

rend_trabalho_n$prev_pub_mensal <- rend_trabalho[, 9]
rend_trabalho_n$imp_renda_mensal <- rend_trabalho[, 10]
rend_trabalho_n$iss_mensal <- rend_trabalho[, 11]

rend_trabalho_n$deducao_mensal <- NA

rm(rend_trabalho)

outros_rend_n <- data.frame( cbind( outros_rend [1:8] , NA , NA , NA , NA , NA , outros_rend[,9] ) )
names(outros_rend_n) <- c( names (outros_rend [1:8]),"valor_mensal" , "inss_mensal" , "prev_pub_mensal" , "imp_renda_mensal" , "iss_mensal", "deducao_mensal")
rm(outros_rend)

junta <- 
  rbind( desp_coletiva_n , 
         cad_coletiva_n , 
         desp_individual_n ,
         alu_estimado_n ,
         rend_trabalho_n ,
         outros_rend_n ) # [1]

rm( desp_coletiva_n , 
    cad_coletiva_n , 
    desp_individual_n ,
    alu_estimado_n ,
    rend_trabalho_n ,
    outros_rend_n)

junta <- 
  transform( junta ,
             codigo = trunc(junta$V9001/100) )

junta <- subset(junta, select = -V9001)
junta [is.na(junta)] <- 0


# CONVERTING CODES TO ASSET EXPENDITURES ####

# Reading the file from the general expenses table translator:
tradutor_despesa <- readxl::read_excel("Tradutor_Despesa_Geral.xls") 

# Merging the database with the translator for the general expenditure table by code.
# Configure the driver to use in-memory SQLite
options(sqldf.driver = "SQLite")

junta_tradutor <-
  sqldf::sqldf("SELECT a.*,
               b.variavel,
               b.nivel_0,
               b.nivel_1,
               b.nivel_2,
               b.nivel_3,
               b.nivel_4,
               b.nivel_5
               from junta as a 
               left join tradutor_despesa as b
               on a.codigo = b.codigo"
  )

# Creation of the variable “valor”, which will receive the values of the variables according
# to the expenditure group with which the code is associated. Most use the item V8000_DEFLA,
# relating to expenditure/purchase values for goods and services.
# The “Other current expenses” group, on the other hand, in addition to the V8000_DEFLA item, also uses the items
# INSS (V1904_DEFLA) and deductions (V531112_DEFLA, V531122_DEFLA, V531132_DEFLA or V8501_DEFLA)

merge1 <- transform( junta_tradutor ,
                     valor = ifelse( Variavel == 'V8000_DEFLA' , valor_mensal , 
                                     ifelse( Variavel == 'V1904_DEFLA' , inss_mensal ,
                                             ifelse( Variavel == 'V531112_DEFLA' , prev_pub_mensal ,
                                                     ifelse( Variavel == 'V531122_DEFLA' , imp_renda_mensal ,
                                                             ifelse( Variavel == 'V531132_DEFLA' , iss_mensal ,
                                                                     ifelse( Variavel == 'V8501_DEFLA' , deducao_mensal , NA)))))))

merge1 <- merge1[!is.na(merge1$valor), ]
merge1 [is.na(merge1)] <- 0

sum(merge1$valor) #The correct value should be 320865144894

rm(junta, junta_tradutor)

# Expenditure Table by household - Here we have grouped by key
# Summing the monthly figures for level 3.
# Level 3 selected in accordance with the methodological definition for grouping expenditure
Despesa_Geral_Familias <- merge1 %>%
  group_by(UF, ESTRATO_POF,TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, Nivel_3, PESO_FINAL) %>%
  summarise(valor = sum (valor))

# Transpose the values from level 3 (product codes) into the columns
Despesa_Geral_Familias <- dcast(Despesa_Geral_Familias, ... ~ Nivel_3) 

Despesa_Geral_Familias[is.na(Despesa_Geral_Familias)] <- 0

# Renaming the columns in the General Expenditure Table in accordance with the ‘Level 3’ description provided by the Translator
Dicionario_despesa <- setNames(tradutor_despesa$Descricao_3, tradutor_despesa$Nivel_3)
nomes_colunas <- colnames(Despesa_Geral_Familias)
nomes_colunas <- ifelse(nomes_colunas %in% names(Dicionario_despesa), Dicionario_despesa[nomes_colunas], nomes_colunas)
colnames(Despesa_Geral_Familias) <- nomes_colunas

Despesa_Geral_Familias <- rename(Despesa_Geral_Familias, Despesas_Patrimonio = '0')
#data <- rename(data, new_name = old_name)  to rename a specific column

#New column showing each household’s total expenditure
#This will be used to compare expenditure on education and healthcare
Despesa_Geral_Familias$Despesa_Total <- rowSums(Despesa_Geral_Familias[, 8:25], na.rm = TRUE) 
#Here, capital expenditure is included!!!
sum(Despesa_Geral_Familias$Despesa_Total) # valor: 297458159645
rm(merge1)

Despesa_Geral_Familias[is.na(Despesa_Geral_Familias)] <- 0


#_______________________________________________________________________________________________________

# CALCULATING NON-MONETARY INCOME ####

## We start with the ‘non-monetary consumption’ table, transposing it and renaming the columns.
## Remember that this table was created earlier when we dealt with consumption expenditure

# READING OF THE REGISTER  – COLLECTIVE EXPENDITURE ####
despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds")
# [2] Filtering only non-monetary information
desp_coletiva <- 
  transform( 
    subset( despesa_coletiva,
            V9002 == 7 
    ) , # [2]
    valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )  
  )[ , c("UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
         "PESO_FINAL", "V9001" , "valor_mensal" ) ]

rm(despesa_coletiva)


# READING OF THE REGISTER – COLLECTIVE REGISTER (POF 3 Questionnaire) ####

caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
cad_coletiva <- 
  transform( 
    subset( caderneta_coletiva,
            V9002 == 7
    ) ,  # [2]
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [1]
  )[ , c( "UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
          "PESO_FINAL", "V9001" , "valor_mensal" ) ]

rm(caderneta_coletiva)


# READING OF THE REGISTER  - INDIVIDUAL SPENDING ####
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
desp_individual <-
  transform( 
    subset( despesa_individual,
            V9002 == 7
    ) , 
    valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    ) 
  )[ , c("UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
         "PESO_FINAL", "V9001" , "valor_mensal" ) ]

rm(despesa_individual)


# MERGINF OF RECORDS ####
#including those containing non-monetary values

junta_nao_monet <- 
  rbind( desp_coletiva , 
         cad_coletiva , 
         desp_individual
  )

junta_nao_monet <- 
  transform( junta_nao_monet,
             codigo = trunc(junta_nao_monet$V9001/100) )

junta_nao_monet <- subset(junta_nao_monet, select = -V9001)

# Joining the database with the translator for the general expenditure table by code.
# Configure the driver to use in-memory SQLite
options(sqldf.driver = "SQLite")

renda_nao_monet <-
  sqldf::sqldf("SELECT a.*,
               b.variavel,
               b.nivel_0,
               b.nivel_1,
               b.nivel_2,
               b.nivel_3,
               b.nivel_4,
               b.nivel_5
               from junta_nao_monet as a 
               left join tradutor_despesa as b
               on a.codigo = b.codigo"
  )
# Adding up the monthly figures for level 3.
# Level 3 selected in accordance with the methodological definition for grouping expenditure

renda_nao_monet <- renda_nao_monet %>%
  group_by(UF, ESTRATO_POF,TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, Nivel_3, PESO_FINAL) %>%
  summarise(valor = sum (valor_mensal))

rm(cad_coletiva, desp_coletiva, desp_individual)

# Transpose the values from level 3 (product codes) into the columns 
renda_nao_monet <- dcast(renda_nao_monet, ... ~ Nivel_3) 

renda_nao_monet[is.na(renda_nao_monet)] <- 0

# Renaming the columns in the General Expenditure Table in accordance with the ‘Level 3’ description provided by the Translator
Dicionario_despesa <- setNames(tradutor_despesa$Descricao_3, tradutor_despesa$Nivel_3)
nomes_colunas <- colnames(renda_nao_monet)
nomes_colunas <- ifelse(nomes_colunas %in% names(Dicionario_despesa), Dicionario_despesa[nomes_colunas], nomes_colunas)
colnames(renda_nao_monet) <- nomes_colunas

renda_nao_monet <- rename(renda_nao_monet, Despesas_Patrimonio = 'NA')

#New column showing each household’s total expenditure
#This will be used to compare expenditure on education and healthcare
renda_nao_monet$Despesa_Total <- rowSums(renda_nao_monet[, 8:18], na.rm = TRUE)
#Also do not add net income
sum(renda_nao_monet$Despesa_Total) # 12443953176 

#Renaming Columns
colunas_a_modificar <- colnames(renda_nao_monet)[8:19]
# Identifying non-monetary values by adding ‘_NM’ to the column names
colnames(renda_nao_monet)[8:19] <- paste0(colunas_a_modificar, "_Nm")
rm(tradutor_despesa, junta_nao_monet, colunas_a_modificar, Dicionario_despesa, nomes_colunas)
renda_nao_monet[is.na(renda_nao_monet)] <- 0

#_________________________________________________________________________________________________

# READING INDIVIDUAL'S REGISTER ####

# Reading the ‘RESIDENT’ RECORD – required to calculate the expanded number of UCs.
# It is worth noting that this is the only record in the microdata set that encompasses all UCs
# Extracting all UCs from the resident file

DECIMO <- readRDS("MORADOR_QUALI_VIDA.rds") 


# Selecting and grouping variables
DECIMO_uc <- DECIMO [ ,
                      c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC", "RENDA_DISP_PC", "C7", "PESO_FINAL") ] 

# C7 represents the POF variable itself, which indicates the decile for each household
# Adding an ‘ID’ column with the number 1 in each row (1 individual per row):
DECIMO_uc$ID <- 1
DECIMO_uc [is.na(DECIMO_uc)] <- 0

# Grouping households (UCs) and increasing the total number of people
DECIMO_uc2 <- DECIMO_uc %>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL, C7, RENDA_DISP_PC ) %>%
  summarise(
    Qtd_Integrantes = sum(ID)
    , Total_Familia = sum(ID * PESO_FINAL)) #I multiply by weight to expand
# Here’s what I have: Each row represents a family with a certain number of members. ‘Total_Family’ is the number of people
# that this family as a whole represents
# THE TOTAL FAMILY IS THE EXPANDED POPULATION!

# Calculate the sum of the ‘ID’ column, grouped by income decile
soma_por_decimo <- DECIMO_uc2 %>%
  group_by(C7) %>% 
  summarise(soma_Total_familia = sum(Total_Familia, na.rm = TRUE))
print(soma_por_decimo)
# Each tenth has 20 million, which matches our population of 200 million
rm(soma_por_decimo)

DECIMO_uc2 <- rename(DECIMO_uc2, decimo_renda = 'C7')
#changing the name to decimo_renda to make it easier to identify in the analyses 


# FAMILY CHARACTERISTICS ####

# Selecting the variables
#!!! Here I have amended the code: I have changed it to retrieve data from the MORADOR database, which is the table containing the individual’s characteristics
MORADOR <- readRDS("MORADOR.rds") 
Individuos_uc <- MORADOR [ ,
                           c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                              "V0306","V0403","V0404", "V0405", "V0406", "V0414", "V0415", "V0416", "V041711", 
                              "V041712", "V041721", "V041722", "V041731", "V041732", "V041741", "V041742", 
                              "V0418", "V0419", "V0420", "V0421", "V0422", "V0423", "V0424", "V0425", 
                              "V0426", "V0427", "V0428", "V0429", "V0430", "ANOS_ESTUDO", "RENDA_TOTAL", "RENDA_DISP_PC", "RENDA_NAO_MONET_PC", 'RENDA_MONET_PC', "PESO_FINAL") ]


#TOTAL_INCOME: Amount in reais (R$), including cents, of the total gross monthly income of the Household.
#Total income is calculated as the sum of the gross monthly monetary incomes of all residents
#of the Household, derived from employment, transfers and other income, plus the portion relating
#of the household’s monthly non-monetary income, plus the CHANGE IN NET WORTH,
#which comprises sales of property, inheritance receipts and the positive balance of financial transactions.

#DISPOSABLE_INCOME_PC: Per capita household disposable income. This is calculated by dividing the total disposable income of the
#Consumption Unit by the total number of residents in the Consumption Unit. Disposable income was defined as the sum of
#monetary and non-monetary income minus direct taxes, social contributions, and other
#compulsory or quasi-compulsory deductions. Per capita household disposable income may be negative, as the
#calculations include deductions recorded in the POF 2, POF 4 and POF 5 questionnaires
#(see file Renda_disponível_dedução_micro_v1). Residents with the family status
#‘domestic worker’ and ‘relative of a domestic worker’ are excluded from all stages of the variable’s construction.

#Adding an ‘ID’ column with the number 1 in each row (1 individual per row):
Individuos_uc [is.na(Individuos_uc)] <- 0
Individuos_uc $ID <- 1

#Grouping households (UCs) and increasing the total number of people
Individuos_uc_A <- Individuos_uc %>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL, RENDA_TOTAL) %>%
  summarise(
    Total_Familia = (ID * PESO_FINAL)) #Note: Test OK – 58,039 observations. This figure must match across all tables
rm(Individuos_uc_A)

#Working on the ‘Resident’ variables

#  |      Code         |                     Variable                     |
#  |:-----------------:|:------------------------------------------------:|
#  |       V0404       |                GENDER:1- MEN 2- WOMAN            |
#  |       V0405       |                       RACE                       |

# For the IBGE in Brazil, the ‘black’ variable encompasses ‘Black’ and ‘Brown’; 
# therefore, IF the number identified in the variable is 2 or 4 (black or brown), 
# then the ‘black’ variable is set to 1

#Urban vs Rural
Individuos_uc$Urbano <- case_when(Individuos_uc$TIPO_SITUACAO_REG == 1 ~ 1 , TRUE ~ 0)
Individuos_uc$Rural <- case_when(Individuos_uc$TIPO_SITUACAO_REG == 2 ~ 1 , TRUE ~ 0)

#V0405 - RACE
Individuos_uc$Negros <- case_when(Individuos_uc$V0405 == 2 | Individuos_uc$V0405 == 4 ~ 1 , TRUE ~ 0) 
Individuos_uc$Brancos <- case_when(Individuos_uc$V0405 == 1 ~ 1 , TRUE ~ 0)
Individuos_uc$Amarela <- case_when(Individuos_uc$V0405 == 3 ~ 1 , TRUE ~ 0)
Individuos_uc$Indigena <- case_when(Individuos_uc$V0405 == 5 ~ 1 , TRUE ~ 0)
Individuos_uc$S_Decl <- case_when(Individuos_uc$V0405 == 9 ~ 1 , TRUE ~ 0)

# V0404 - GENDER
Individuos_uc$Homem <- case_when(Individuos_uc$V0404 == 1 ~ 1 , TRUE ~ 0)
Individuos_uc$Mulher <- case_when(Individuos_uc$V0404 == 2 ~ 1 , TRUE ~ 0)

# AGE STAGE
Individuos_uc$Crianca <- case_when(Individuos_uc$V0403 >= 0 & Individuos_uc$V0403 <= 9 ~ 1 , TRUE ~ 0)
Individuos_uc$Adolescente <- case_when(Individuos_uc$V0403 > 9 & Individuos_uc$V0403 <= 19 ~ 1 , TRUE ~ 0)
Individuos_uc$Adulto <- case_when(Individuos_uc$V0403 >= 20 & Individuos_uc$V0403 <= 59 ~ 1 , TRUE ~ 0)
Individuos_uc$Idoso <- case_when(Individuos_uc$V0403 >= 60 ~ 1 , TRUE ~ 0)

Individuos_uc$Escolaridade_da_Ref_uc <- ifelse(Individuos_uc$V0306 == 1, Individuos_uc$ANOS_ESTUDO, 0)
Individuos_uc$Genero_da_Ref_uc <- ifelse(Individuos_uc$V0306 == 1 & Individuos_uc$V0404 == 2 , 1, 0)
Individuos_uc$Raca_da_Ref_uc <- ifelse(Individuos_uc$V0306 == 1 & Individuos_uc$V0405 == 2 |Individuos_uc$V0306 == 1 & Individuos_uc$V0405 == 4, 1, 0)

Individuos_uc$Plano_saude <- case_when(Individuos_uc$V0406 == 1 ~ 1 , TRUE ~ 0)
Individuos_uc[is.na(Individuos_uc)] <- 0

#Expanded family assessment
Individuos_uc2 <- Individuos_uc%>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL, RENDA_TOTAL)%>%  
  summarise( 
    
    Escolaridade_da_Ref_uc = sum(Escolaridade_da_Ref_uc)
    
    , Urbano = sum(Urbano)
    , Rural = sum(Rural)
    
    , Negros = sum(Negros) 
    , Brancos = sum(Brancos) 
    , Outras_R = sum(Amarela, Indigena, S_Decl)
    
    , Mulheres = sum(Mulher)
    , Homens = sum(Homem) 
    
    , Criancas = sum(Crianca) 
    , Adolescentes = sum(Adolescente) 
    , Adultos = sum(Adulto) 
    , Idosos = sum(Idoso)
    
    , Total_Integrantes = sum(ID)
    
    , Fam_Branca = ifelse(Brancos/Total_Integrantes > 0.50, 1, 0)
    , Fam_Preta = ifelse(Negros/Total_Integrantes > 0.50, 1, 0)
    , Fam_Interracial = ifelse(Negros/Total_Integrantes == 0.50 & Brancos/Total_Integrantes == 0.50, 1, 0)
    , Fam_Outras_R = ifelse(Outras_R/Total_Integrantes == 1 , 1, 0)
    
    , Plano_saude = ifelse(sum(Plano_saude) >= 1, 1, 0)
    
    , Cri_Adol = sum(Criancas, Adolescentes)
    , Mulher_responsavel_uc = sum(Genero_da_Ref_uc) 
    , Raca_responsavel_uc = sum(Raca_da_Ref_uc)
    , Total_Familia = sum(ID * PESO_FINAL)  
    , RENDA_DISP_TOT= sum(RENDA_DISP_PC) # It is the total disposable income of the Consumption Unit divided by the total number of residents in the Consumption Unit
    , RENDA_NAO_MONET_TOT = sum(RENDA_NAO_MONET_PC) # It is the total non-monetary income of the Household divided by the total number of residents in the Household.
    , RENDA_MONET_Familia = sum(RENDA_MONET_PC)
    , RENDA_MONET_PC = (sum(RENDA_MONET_PC)/Total_Integrantes)
  )

Individuos_uc2[is.na(Individuos_uc2)] <- 0

sum(Individuos_uc2$Total_Familia)#Expanded population figure – it should be 207 million and a bit

#Generating test statistics:
stats::weighted.mean(x = (Individuos_uc2$RENDA_TOTAL), w=(Individuos_uc2$PESO_FINAL))
#Result: 5,426.699
#According to the data from this survey, the average total household income
#in Brazil was approximately R$ 5,426 per month.

### Remove ‘Total_Familia’ because it will be duplicated when added to the tithes
Individuos_uc2 <- subset(Individuos_uc2 , select = -Total_Familia)
Individuos_uc2 <- subset(Individuos_uc2 , select = -Total_Integrantes)
Individuos_uc2[is.na(Individuos_uc2)] <- 0

#_______________________________________________________________________________________________________

# INDIVIDUALS + INCOME GROUPS ####

Individuos_Classes <- merge(DECIMO_uc2, Individuos_uc2, by = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", 
                                                               "NUM_DOM", "NUM_UC", "PESO_FINAL"), all = TRUE)

Individuos_Classes[is.na(Individuos_Classes)] <- 0
rm(DECIMO_uc2, DECIMO, DECIMO_uc, MORADOR, Individuos_uc, Individuos_uc2)

# TABLE: Total household income and average monthly change in household net worth (R$) by DECILE
# Calculating the weighted average income for each decile
Tabela_Renda_por_decimo <- Individuos_Classes %>%
  group_by(decimo_renda) %>%
  summarise(
    Renda_total      = stats::weighted.mean(x = RENDA_TOTAL, w = PESO_FINAL),  
    Renda_Disp_pc    = stats::weighted.mean(x = RENDA_DISP_PC, w = PESO_FINAL),  
    Renda_Disp_pc_Nm = stats::weighted.mean(x = (RENDA_NAO_MONET_TOT/Qtd_Integrantes), w = PESO_FINAL)
  ) 
rm(Tabela_Renda_por_decimo)

# Analysis of expenditure by income bracket
# Education, Health, Income, and total expenditure for comparison
# COMBINE THE TABLES: Residents and Expenditure
Tabela_Geral <- merge(Individuos_Classes, Despesa_Geral_Familias, by=c("UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC", "PESO_FINAL"))
Tabela_Geral <- merge(Tabela_Geral, renda_nao_monet, by=c("UF", "ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC", "PESO_FINAL"), all.x = T)
Tabela_Geral [is.na(Tabela_Geral)] <- 0

rm(Individuos_Classes, Despesa_Geral_Familias, renda_nao_monet)
#sum(General_Table$Total_Household)  - check on number of people - correct: 207349143

# COMPARATIVE TEST #1#
# Average total expenditure of weighted household units (Expanded)
media_gastos_uc <- sum(Tabela_Geral$Despesa_Total)/sum(Tabela_Geral$PESO_FINAL)
media_gastos_uc #Display result: 4,649.027
# Expected result (R$4,659.03) when compared with Table 1.1.1 in the file containing the initial POF results:
# (https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Primeiros_resultados/tabelas_despesas/)
rm(media_gastos_uc)

# COMPARATIVE TEST #2#
# Average monthly expenditure for the lowest income bracket
Estrato1 <- subset(Tabela_Geral, decimo_renda == '1')
Total_Famílias_1 <- sum(Estrato1$Total_Familia)

media_gastos_1 <- subset(Tabela_Geral, decimo_renda  == '1')
sum(media_gastos_1$Despesa_Total) / Total_Famílias_1  
rm(media_gastos_1)
rm(Estrato1)
rm(Total_Famílias_1)

#_______________________________________________________________________________________________________

# TABLE OF AVERAGE MONTHLY HOUSEHOLD EXPENDITURE, ####

# By total income bracketes and monthly changes in household net worth,
# By type of expenditure (Total, Education, Health, Monetary, Non-monetary)
# Brazil household budget survey - 2017/2018

#Identifying tenths
Tabela_Geral <- Tabela_Geral %>%
  mutate(
    decimo_faixa = case_when(
      decimo_renda == 1  ~ "0–10",
      decimo_renda == 2  ~ "10–20",
      decimo_renda == 3  ~ "20–30",
      decimo_renda == 4  ~ "30–40",
      decimo_renda == 5  ~ "40–50",
      decimo_renda == 6  ~ "50–60",
      decimo_renda == 7  ~ "60–70",
      decimo_renda == 8  ~ "70–80",
      decimo_renda == 9  ~ "80–90",
      decimo_renda == 10 ~ "90–100"
    )
  )

# Overview of household consumption patterns by income decile. Expenditure figures are on a monthly basis 
Tabela_Geral_Medias <- Tabela_Geral%>%
  group_by(decimo_faixa) %>%
  summarise( 
    Qtd_Familias = sum(Total_Familia),  #Number of households in each decile
    Tamanho_medio_familia = sum(Qtd_Integrantes * PESO_FINAL) / sum(PESO_FINAL), #Average household size (number of members)
    Med_Escolaridade_da_Ref_uc = weighted.mean(Escolaridade_da_Ref_uc, PESO_FINAL), #Average years of schooling of the reference person in the household
    
    Educacao_GDPC = sum(Educacao/Qtd_Integrantes) / sum(PESO_FINAL), #Per capita household expenditure on education
    Educacao_GDPC_Nm = sum(Educacao_Nm/Qtd_Integrantes) / sum(PESO_FINAL), #per capita household expenditure on healthcare
    
    Saude_GDPC = sum(`Assistencia a saude`/Qtd_Integrantes) / sum(PESO_FINAL), #Non-monetary per capita household expenditure on education
    Saude_GDPC_Nm = sum(`Assistencia a saude_Nm`/Qtd_Integrantes) / sum(PESO_FINAL), #Non-monetary per capita household expenditure on healthcare
    
    Renda_total = weighted.mean(x = RENDA_TOTAL, PESO_FINAL), #Total income
    Renda_total_monetaria = weighted.mean(x = RENDA_MONET_Familia, PESO_FINAL), #Non-monetary total income
    Renda_monetaria_pc = weighted.mean(x = RENDA_MONET_PC, PESO_FINAL), #Per capita monetary income
    Renda_media_disp_PC = weighted.mean(x = RENDA_DISP_PC, PESO_FINAL), #Average per capita disposable income
    Renda_Nm_media_disp_PC = weighted.mean(x = (RENDA_NAO_MONET_TOT/Qtd_Integrantes), PESO_FINAL), #Average per capita non-monetary disposable income
    
    Desp_Total_GDPC = sum(Despesa_Total/Qtd_Integrantes)/sum(PESO_FINAL), #Total per capita household expenditure
    Desp_Total_GDPC_Nm = sum(Despesa_Total_Nm/Qtd_Integrantes)/sum(PESO_FINAL) #Total per capita non-monetary household expenditure
  )


setwd(choose.dir()) # Specify the folder where everything will be saved

write_xlsx(Tabela_Geral_Medias, "Tabela_Geral_Medias.xlsx")  #Saving as xlsx format


# GINI ANALYSIS ####

# First, calculating Gini coefficient based on disposable income as IBGE did.

valor_gini <- reldist::gini(
  x = Tabela_Geral$RENDA_DISP_PC, 
  weights = Tabela_Geral$PESO_FINAL * Tabela_Geral$Qtd_Integrantes)

texto_gini <- sprintf("Gini: %.3f", valor_gini)

ggplot(Tabela_Geral_Medias, aes(x = Renda_media_disp_PC, n = Qtd_Familias)) +
  stat_lorenz(geom = "area", fill = "#2c7fb8", alpha = 0.7) +
  stat_lorenz(geom = "line", color = "#1d4e89", linewidth = 0.8) +
  coord_fixed() +
  geom_abline(linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0.02)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0.02)) +
  theme_ipsum_rc(grid = "XY") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    plot.caption = element_text(size = 9, color = "gray50")
  ) +
  annotate("text", x = 0.15, y = 0.85, label = texto_gini, 
           size = 5, color = "#b30000", fontface = "bold") +
  labs(
    x = "Cumulative share of Disposable Income",          # "Renda"
    y = "Cumulative share of Families",        # "Quantidade de famílias"
    title = "Lorenz Curve: Gini POF 2017-18",  # "Gini POF 2017-18"
    subtitle = "Income distribution among families",
    caption = "Source: POF-IBGE (accessed August 08, 2023)"
  ) 

ggsave("Lorenz Curve Gini POF 2017 and 2018.png", width = 10, height = 6, dpi = 300)

# The graph shows the Lorenz curve based on household expenditure. The curve derived 
# from disposable income yielded the same result as the Gini coefficient for Brazil in 2017-2018.
# https://agenciadenoticias.ibge.gov.br/agencia-sala-de-imprensa/2013-agencia-de-noticias/releases/29490-pof-2017-2018-cerca-de-da-renda-disponivel-das-familias-brasileiras-e-nao-monetaria

rm(valor_gini, texto_gini)

#________________________________________________________________________________________________________

# APPLYING LOGIT MODEL ####

#HEALTH LOGIT EVALUATION:

# In this logit model, we use the logit model in R to calculate the probability of a family
# access health resources based on three variables:
# gender, race and whether they have health insurance or not

#Creating a General Table for the model
Tabela_Geral_logit <- Tabela_Geral%>%
  mutate(
    Med_Escolaridade_da_Ref_uc = weighted.mean(x = Escolaridade_da_Ref_uc, w = PESO_FINAL),
    access_the_resource_health  = ifelse((`Assistencia a saude_Nm`) > 0, 1, 0),
    access_the_resource_educ = ifelse((Educacao_Nm) > 0, 1, 0)
  )

# Health access logit model
modelo_saude <- glm(access_the_resource_health ~ Mulher_responsavel_uc + Fam_Preta + Plano_saude, 
                    data = Tabela_Geral_logit, family = "binomial")

# Probabilities
probabilidades <- predict(modelo_saude, type = "response")

# Model Summary
summary(modelo_saude)
# The results of the logit model for a health budget show that all the coefficients 
# have very low p-values, indicating that all the variables are statistically 
# significant in relation to access to public healthcare 


# Checking for outliers/leverage points
plot(modelo_saude, which = 5)
# No points appear beyond the red dotted line (it doesn’t even appear),
# so there is no indication of any outliers

# Viewing the results for each variable and its effect:
race_effect <- ggpredict(modelo_saude, terms = "Fam_Preta")
plot(race_effect) +
  theme_classic() +
  labs(title = "Probability of Access to Healthcare by Race",
       x = "Black Family (0 = No, 1 = Yes)",
       y = "Probability of Access")

ggsave("Probability of Access to Healthcare by Race.png", width = 10, height = 6, dpi = 300)

#Gender
gender_effect <- ggpredict(modelo_saude, terms = "Mulher_responsavel_uc")
plot(gender_effect) +
  theme_classic() +
  labs(title = "Probability of Access to Healthcare by household's responsable gender",
       x = "Woman as the responsable (0 = No, 1 = Yes)",
       y = "Probability of Access")

ggsave("Probability of Access to Healthcare by household's responsable gender.png", width = 10, height = 6, dpi = 300)

# 3. Insurance effect
insurance_effect <- ggpredict(modelo_saude, terms = "Plano_saude")
plot(insurance_effect) +
  theme_classic() +
  labs(title = "Probability of Access to Healthcare: With/Without Health insurance",
       x = "Health insurance (0 = No, 1 = Yes)",
       y = "Probability of Access")

ggsave("Probability of Access to Healthcare With or Without Health insurance.png", width = 10, height = 6, dpi = 300)


# EDUCATION LOGIT EVALUATION:

# In this logit model, we use the logit model in R to calculate the probability of a family
# access education resources based on gender and race.

# education access logit model
modelo_educacao <- glm(access_the_resource_educ ~ Mulher_responsavel_uc + Fam_Preta, 
                       data = Tabela_Geral_logit, family = "binomial")

# Probabilities
probabilidades_e <- predict(modelo_educacao, type = "response")

# Model Summary
summary(modelo_educacao)
# The results of the logit model for education access show that Women as the household's
# representant has significant and positive influence in access public education resources
# Black families has even more positive influence

# Viewing the results for each variable and its effect:
gender_effect_educ <- ggpredict(modelo_educacao, terms = "Mulher_responsavel_uc")
plot(gender_effect_educ) +
  theme_classic() +
  labs(title = "Probability of Access to Education by household's responsable gender",
       x = "Woman as the responsable (0 = No, 1 = Yes)",
       y = "Probability of Access")

ggsave("Probability of Access to Education by household's responsable gender.png", width = 10, height = 6, dpi = 300)

#Race
race_effect_educ <- ggpredict(modelo_educacao, terms = "Fam_Preta")
plot(race_effect_educ) +
  theme_classic() +
  labs(title = "Probability of Access to Education by Race",
       x = "Black Family (0 = No, 1 = Yes)",
       y = "Probability of Access")

ggsave("Probability of Access to Education by Race.png", width = 10, height = 6, dpi = 300)

rm(Tabela_Geral_logit, modelo_saude, modelo_educacao, race_effect, 
   gender_effect, insurance_effect, gender_effect_educ, race_effect_educ, 
   probabilidades, probabilidades_e)

#________________________________________________________________________________________________________

# COMPOSITION GRAPHS ####

#Average years of education by decile
ggplot(Tabela_Geral_Medias, aes(x = factor(decimo_faixa), 
  y = Med_Escolaridade_da_Ref_uc))+
  
  geom_col(fill = "#45ff66", width = 0.7) +
  geom_text(
    aes(label = round(Med_Escolaridade_da_Ref_uc, 1)),
    position = position_stack(vjust = 0.5),
    size = 7,
    color = "white",
    fontface = "bold"
  ) +
  xlab("Income Decile") +
  ylab("Average years of education") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("Average years of education by decile.png", width = 10, height = 6, dpi = 300)



# Income composition

renda <- Tabela_Geral_Medias %>%
  reframe(decimo_faixa, Renda_media_disp_PC, Renda_Nm_media_disp_PC)

# Transforminf the data in long format (melt)
renda <- melt(
  renda,
  id.vars = "decimo_faixa",                   
  measure.vars = c("Renda_media_disp_PC", 
                   "Renda_Nm_media_disp_PC"),       
  variable.name = "variavel",                       
  value.name = "valor"                              
)

renda$variavel <- dplyr::recode(
  renda$variavel,
  "Renda_media_disp_PC"   = "Disposable Income",
  "Renda_Nm_media_disp_PC"= "Non-monetary Income"
)

renda <- renda %>%
  group_by(decimo_faixa) %>%
  mutate(
    total_decimo = sum(valor, na.rm = TRUE),
    percentual = valor / total_decimo
  ) %>%
  ungroup()

# Criar o gráfico com escala de preenchimento contínua
ggplot(renda, aes(x = factor(decimo_faixa), y = percentual, fill = variavel)
  )+
  geom_col(width = 0.7) +
  geom_text(
    aes(label = scales::percent(percentual, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 7,
    color = "white",
    fontface = "bold"
  ) +
  xlab("Income Decile") +
  ylab("Income composition (%)") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c("Disposable Income" = "#FFAB4D",
      "Non-monetary Income" = "#FF7276"), name = "Type of Income"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.title = element_text(
      size = 16
    ),
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("Income composition by decile.png", width = 10, height = 6, dpi = 300)

rm(renda)



# Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(tidyverse)
# install.packages("remotes")
# remotes::install_github("georgevbsantiago/qsacnpj") - https://github.com/georgevbsantiago/qsacnpj
library(qsacnpj)
library(data.table)

# Importando bases ------------------------------------------------------
path <- paste0(getwd(),"/bases")
lista_arquivos <- list.files(path = path,
                            recursive = TRUE,
                            pattern = "\\.csv",
                            full.names = TRUE)

base <- readr::read_delim(lista_arquivos)


# Ajustando variáveis -----------------------------------------------------
# Ajustando nomes
names(base) <- c("cnpj", "emitente", "num", "dt_emissao", "valor_nf", "dt_registro", "credito", "sit_credito")

# Ajustando datas
base$dt_emissao <- as.Date(base$dt_emissao, format = "%d/%m/%Y")
base$dt_registro <- as.Date(base$dt_registro, format = "%d/%m/%Y")

# Ajustando valores com virgula
base$valor_nf <-  gsub(",", ".", base$valor_nf) %>%
                      as.numeric()
base$credito <-  gsub(",", ".", base$credito) %>%
  as.numeric()


# Enriquecendo a base -----------------------------------------------------

# Fazendo download dos dados dos CNPJ

# down_func <- function(url, file){
#     destino <- paste0(getwd(),"/bases_receita/", file)
#     download.file(url, destino)
# }
# 
# #https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y0.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 01.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y1.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 02.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y2.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 03.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y3.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 04.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y4.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 05.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y5.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 06.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y6.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 07.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y7.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 08.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y8.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 09.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y9.D20514.EMPRECSV.zip", "Dados Abertos CNPJ EMPRESA 10.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y0.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 01.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y1.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 02.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y2.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 03.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y3.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 04.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y4.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 05.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y5.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 06.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y6.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 07.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y7.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 08.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y8.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 09.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y9.D20514.ESTABELE.zip", "Dados Abertos CNPJ ESTABELECIMENTO 10.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y0.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 01.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y1.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 02.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y2.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 03.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y3.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 04.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y4.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 05.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y5.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 06.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y6.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 07.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y7.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 08.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y8.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 09.zip")
# down_func("http://200.152.38.155/CNPJ/K3241.K03200Y9.D20514.SOCIOCSV.zip", "Dados Abertos CNPJ SÓCIO 10.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$W.SIMPLES.CSV.D20514.zip", "Informações sobre o Simples Nacional/MEI.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.CNAECSV.zip", "Tabela de atributo CNAE.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.MOTICSV.zip", "Tabela de motivo da situação cadastral.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.MUNICCSV.zip", "Tabela de atributo Município.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.NATJUCSV.zip", "Tabela de atributo Natureza Jurídica.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.PAISCSV.zip", "Tabela de atributo País.zip")
# down_func("http://200.152.38.155/CNPJ/F.K03200$Z.D20514.QUALSCSV.zip", "Tabela de atributo qualificação dos sócios.zip")


#Importando base de sócios
# path_socios <- paste0(getwd(),"/bases_receita_socios")
# lista_arquivos_socios <- list.files(path = path_socios,
#                              recursive = TRUE,
#                              pattern = "\\.SOCIOCSV",
#                              full.names = TRUE)
# 
# base_socios <- readr::read_delim(lista_arquivos_socios, 
#            delim = ";", escape_double = FALSE, col_names = FALSE, 
#            trim_ws = TRUE)
# 
# names(base_socios) <- c("CNPJ BÁSICO",
#                         "IDENTIFICADOR DE SÓCIO",
#                         "NOME DO SÓCIO (NO CASO PF) OU RAZÃO SOCIAL (NO CASO PJ)",
#                         "CNPJ/CPF DO SÓCIO",
#                         "QUALIFICAÇÃO DO SÓCIO",
#                         "DATA DE ENTRADA SOCIEDADE",
#                         "PAIS",
#                         "REPRESENTANTE LEGAL",
#                         "NOME DO REPRESENTANTE",
#                         "QUALIFICAÇÃO DO REPRESENTANTE LEGAL",
#                         "FAIXA ETÁRIA"
#                         )
# 
# base_socios$`FAIXA ETÁRIA` <- ifelse(base_socios$`FAIXA ETÁRIA` == 1, "0 a 12 anos",
#                                      ifelse(base_socios$`FAIXA ETÁRIA` == 2, "13 a 20 anos",
#                                             ifelse(base_socios$`FAIXA ETÁRIA` == 3, "21 a 30 anos",
#                                                    ifelse(base_socios$`FAIXA ETÁRIA` == 4, "31 a 40 anos",
#                                                           ifelse(base_socios$`FAIXA ETÁRIA` == 5, "41 a 50 anos",
#                                                                  ifelse(base_socios$`FAIXA ETÁRIA` == 6, "51 a 60 anos",
#                                                                         ifelse(base_socios$`FAIXA ETÁRIA` == 7, "61 a 70 anos",
#                                                                                ifelse(base_socios$`FAIXA ETÁRIA` == 8, "71 a 80 anos",
#                                                                                       ifelse(base_socios$`FAIXA ETÁRIA` == 9, "Maiores de 80 anos",
#                                                                                              ifelse(base_socios$`FAIXA ETÁRIA` == 0, "Não se aplica", NA))))))))))
# 
# 
# write.csv(base_socios, "base_receita_preparados/base_socios.csv", row.names = F)

#Importando base de estabelecimentos
# path_estabelecimento <- paste0(getwd(),"/bases_receita_estabelecimento")
# 
# lista_arquivos_estabelecimento <- list.files(path = path_estabelecimento,
#                                     recursive = TRUE,
#                                     pattern = "\\.ESTABELE",
#                                     full.names = TRUE)
# 
# ler_estabelecimento <- function(base,n){
#   
#   base <- readr::read_delim(paste0("G:/My Drive/RStudio/amabilidados/bases_receita_estabelecimento/", base), 
#                                              delim = ";", escape_double = FALSE, col_names = FALSE, 
#                                              trim_ws = TRUE)
#   
#   names(base) <- c("CNPJ BÁSICO","CNPJ ORDEM",
#                                     "CNPJ DV","IDENTIFICADOR MATRIZ/FILIAL",
#                                     "NOME FANTASIA","SITUAÇÃO CADASTRAL",
#                                     "DATA SITUAÇÃO CADASTRAL","MOTIVO SITUAÇÃO CADASTRAL",
#                                     "NOME DA CIDADE NO EXTERIOR","PAIS",
#                                     "DATA DE INÍCIO ATIVIDADE","CNAE FISCAL PRINCIPAL",
#                                     "CNAE FISCAL SECUNDÁRIA","TIPO DE LOGRADOURO",
#                                     "LOGRADOURO","NÚMERO",
#                                     "COMPLEMENTO",
#                                     "BAIRRO","CEP",
#                                     "UF","MUNICÍPIO",
#                                     "DDD 1","TELEFONE 1",
#                                     "DDD 2","TELEFONE 2",
#                                     "DDD DO FAX","FAX",
#                                     "CORREIO ELETRÔNICO","SITUAÇÃO ESPECIAL",
#                                     "DATA DA SITUAÇÃO ESPECIAL") 
#   
#   base$`IDENTIFICADOR MATRIZ/FILIAL` <- ifelse(base$`IDENTIFICADOR MATRIZ/FILIAL` == 1, "MATRIZ",
#                                                                 ifelse(base$`IDENTIFICADOR MATRIZ/FILIAL` == 2, "FILIAL",NA))
#   
#   base$`SITUAÇÃO CADASTRAL` <- ifelse(base$`SITUAÇÃO CADASTRAL` == "01", "NULA",
#                                                        ifelse(base$`SITUAÇÃO CADASTRAL` == "02", "ATIVA",
#                                                               ifelse(base$`SITUAÇÃO CADASTRAL` == "03", "SUSPENSA",
#                                                                      ifelse(base$`SITUAÇÃO CADASTRAL` == "04", "INAPTA",
#                                                                             ifelse(base$`SITUAÇÃO CADASTRAL` == "08", "BAIXADA",NA)))))
#   write.csv(base, paste0("base_receita_preparados/base_estabelecimento", n,".csv"), row.names = F)
# }
# 
# ler_estabelecimento("K3241.K03200Y0.D20514.ESTABELE", 0)
# ler_estabelecimento("K3241.K03200Y1.D20514.ESTABELE", 1)
# ler_estabelecimento("K3241.K03200Y2.D20514.ESTABELE", 2)
# ler_estabelecimento("K3241.K03200Y3.D20514.ESTABELE", 3)
# ler_estabelecimento("K3241.K03200Y4.D20514.ESTABELE", 4)
# ler_estabelecimento("K3241.K03200Y5.D20514.ESTABELE", 5)
# ler_estabelecimento("K3241.K03200Y6.D20514.ESTABELE", 6)
# ler_estabelecimento("K3241.K03200Y7.D20514.ESTABELE", 7)
# ler_estabelecimento("K3241.K03200Y8.D20514.ESTABELE", 8)
# ler_estabelecimento("K3241.K03200Y9.D20514.ESTABELE", 9)


#Importando base de empresas
# path_empresa <- paste0(getwd(),"/bases_receita_empresa")
# lista_arquivos_empresa <- list.files(path = path_empresa,
#                                     recursive = TRUE,
#                                     pattern = "\\.EMPRECSV",
#                                     full.names = TRUE)
# 
# base_empresa <- readr::read_delim(lista_arquivos_empresa, 
#                                  delim = ";", escape_double = FALSE, col_names = FALSE, 
#                                  trim_ws = TRUE)
# 
# names(base_empresa) <- c("CNPJ BÁSICO",
#                          "RAZÃO SOCIAL / NOME EMPRESARIAL",
#                          "NATUREZA JURÍDICA",
#                          "QUALIFICAÇÃO DO RESPONSÁVEL",
#                          "CAPITAL SOCIAL DA EMPRESA",
#                          "PORTE DA EMPRESA",
#                          "ENTE FEDERATIVO RESPONSÁVEL")
# 
# base_empresa$`PORTE DA EMPRESA` <- ifelse(base_empresa$`PORTE DA EMPRESA` == "00", "NÃO INFORMADO", 
#                                           ifelse(base_empresa$`PORTE DA EMPRESA` == "01", "MICRO EMPRESA", 
#                                                  ifelse(base_empresa$`PORTE DA EMPRESA` == "03", "EMPRESA DE PEQUENO PORTE", 
#                                                         ifelse(base_empresa$`PORTE DA EMPRESA` == "05", "DEMAIS", NA ))))
# 
# write.csv(base_empresa, "base_receita_preparados/base_empresa.csv", row.names = F)


#Preparando merge com a base da AMA
base$cnpj <- gsub("-", "", base$cnpj)
base$cnpj <- gsub("/", "", base$cnpj)
base$cnpj <- gsub("\\.", "", base$cnpj)
cnpj_ama <- base$cnpj %>% unique() %>% as.character()
min(base$dt_emissao)


ajustar_estabelecimento <- function(base, cnpj_ama){
  base <- fread(paste0("base_receita_preparados/", base), nThread = 4, colClasses = 'character')
  base$`DATA SITUAÇÃO CADASTRAL` <- as.Date(base$`DATA SITUAÇÃO CADASTRAL`, "%Y%m%d")
  base$`DATA DE INÍCIO ATIVIDADE` <- as.Date(base$`DATA DE INÍCIO ATIVIDADE`, "%Y%m%d") 
  base <- subset(base, UF == "SP")
  base <- subset(base, !(`SITUAÇÃO CADASTRAL` != "ATIVA" & `DATA SITUAÇÃO CADASTRAL` <= "2017-01-01"))
  base$cnpj <- paste0(base$`CNPJ BÁSICO`, base$`CNPJ ORDEM`, base$`CNPJ DV`)
  base_ama <- subset(base, cnpj %in% cnpj_ama)
  base_n_ama <- subset(base, !(cnpj %in% cnpj_ama))
  base <- NULL
  return(list(base_ama,base_n_ama))
}
estab0 <- ajustar_estabelecimento("base_estabelecimento0.csv", cnpj_ama)
gc()
estab1 <- ajustar_estabelecimento("base_estabelecimento1.csv", cnpj_ama)
gc()
estab2 <- ajustar_estabelecimento("base_estabelecimento2.csv", cnpj_ama)
gc()
estab3 <- ajustar_estabelecimento("base_estabelecimento3.csv", cnpj_ama)
gc()
estab4 <- ajustar_estabelecimento("base_estabelecimento4.csv", cnpj_ama)
gc()
estab5 <- ajustar_estabelecimento("base_estabelecimento5.csv", cnpj_ama)
gc()
estab6 <- ajustar_estabelecimento("base_estabelecimento6.csv", cnpj_ama)
gc()
estab7 <- ajustar_estabelecimento("base_estabelecimento7.csv", cnpj_ama)
gc()
estab8 <- ajustar_estabelecimento("base_estabelecimento8.csv", cnpj_ama)
gc()
estab9 <- ajustar_estabelecimento("base_estabelecimento9.csv", cnpj_ama)

estab_ama <- do.call(rbind, list(estab0[[1]], 
                    estab1[[1]],
                    estab2[[1]],
                    estab3[[1]],
                    estab4[[1]],
                    estab5[[1]],
                    estab6[[1]],
                    estab7[[1]],
                    estab8[[1]],
                    estab9[[1]]))

estab_n_ama <- do.call(rbind, list(estab0[[2]], 
                                 estab1[[2]],
                                 estab2[[2]],
                                 estab3[[2]],
                                 estab4[[2]],
                                 estab5[[2]],
                                 estab6[[2]],
                                 estab7[[2]],
                                 estab8[[2]],
                                 estab9[[2]]))

estab0 <- NULL
estab1 <- NULL
estab2 <- NULL
estab3 <- NULL
estab4 <- NULL
estab5 <- NULL
estab6 <- NULL
estab7 <- NULL
estab8 <- NULL
estab9 <- NULL

base_ama <- merge(base, estab_ama, by = "cnpj", all = T)

base_ama <- base_ama %>%
  dplyr::select("cnpj",
                "emitente",
                "num",
                "dt_emissao",
                "valor_nf",
                "dt_registro",
                "credito",
                "sit_credito",
                "identificador" = "IDENTIFICADOR MATRIZ/FILIAL",
                "situacao_cadastral" = "SITUAÇÃO CADASTRAL",
                "dt_situacao_cadastral" = "DATA SITUAÇÃO CADASTRAL",
                "dt_inicio_atividades" = "DATA DE INÍCIO ATIVIDADE",
                "cnae_principal" = "CNAE FISCAL PRINCIPAL",
                "cnae_secundaria" = "CNAE FISCAL SECUNDÁRIA",
                "tipo_logradouro" = "TIPO DE LOGRADOURO",
                "logradouro" = "LOGRADOURO",
                "num_logradouro" = "NÚMERO",
                "complemento" = "COMPLEMENTO", 
                "bairro" = "BAIRRO",
                "cep" = "CEP",
                "uf" = "UF",
                "municipio" = "MUNICÍPIO" 
                )

base_n_ama <- estab_n_ama %>%
  dplyr::select("cnpj",
                "identificador" = "IDENTIFICADOR MATRIZ/FILIAL",
                "situacao_cadastral" = "SITUAÇÃO CADASTRAL",
                "dt_situacao_cadastral" = "DATA SITUAÇÃO CADASTRAL",
                "dt_inicio_atividades" = "DATA DE INÍCIO ATIVIDADE",
                "cnae_principal" = "CNAE FISCAL PRINCIPAL",
                "cnae_secundaria" = "CNAE FISCAL SECUNDÁRIA",
                "tipo_logradouro" = "TIPO DE LOGRADOURO",
                "logradouro" = "LOGRADOURO",
                "num_logradouro" = "NÚMERO",
                "complemento" = "COMPLEMENTO", 
                "bairro" = "BAIRRO",
                "cep" = "CEP",
                "uf" = "UF",
                "municipio" = "MUNICÍPIO" 
  )

write.csv(base_ama, "base_ama.csv", row.names = F)
write.csv(base_n_ama, "base_n_ama.csv", row.names = F)

# empresa <- fread("base_receita_preparados/base_empresa.csv")
# empresa_ama <- merge(estab_ama, empresa, by)
# empresa <- NULL
# gc()
# 
# socio <- fread("base_receita_preparados/base_socios.csv")
# socio_ama <- subset(socio, `CNPJ BÁSICO` %in% cnpj_ama)
# socio <- NULL


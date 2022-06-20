# Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(zoo)
library(tidytext)
library(reshape2)
library(writexl)
library(patchwork)
library(stats)
library(forecast)
library(readxl)
library(tmaptools)
library(readxl)
library(paletteer)
library(gganimate)
library(sf)
library(geobr)

# Importando bases ------------------------------------------------------
base <- read_csv("bases_ajustadas/base_ama.csv")

# Análise Temporal ---------------------------------------------------------------
#Agrupando por mês e por CNPJ
base$mes_ano_emissao <- as.yearmon(base$dt_emissao)
base$ano_emissao <- format(base$mes_ano_emissao, format = "%Y")


# Evolução do valor das notas
valor_nota <- base %>%
  group_by(mes_ano_emissao) %>%
  summarise(valor_nf = sum(valor_nf, na.rm = T))
ggplot(valor_nota, aes(mes_ano_emissao, valor_nf))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Valor da nota R$")

stl(ts(valor_nota$valor_nf,
       start = min(valor_nota$mes_ano_emissao),
       end = max(valor_nota$mes_ano_emissao),
       frequency = 12), s.window="periodic", robust=TRUE) %>% autoplot()

valor_nota_emp <- base %>%
  group_by(cnpj, ano_emissao) %>%
  summarise(valor_nf = sum(valor_nf, na.rm = T))

ggplot(valor_nota_emp, aes(ano_emissao, valor_nf))+
  geom_col()+
  theme_bw()

ggplot(valor_nota_emp, aes(ano_emissao, log(valor_nf)))+
  geom_jitter(alpha =0.3)+
  geom_boxplot()+
  theme_bw()


# Evolução do credito
valor_credito <- base %>%
  group_by(mes_ano_emissao) %>%
  summarise(credito = sum(credito, na.rm = T))
ggplot(valor_credito, aes(mes_ano_emissao, credito))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Valor do crédito R$")

stl(ts(valor_credito$credito,
       start = min(valor_credito$mes_ano_emissao),
       end = max(valor_credito$mes_ano_emissao),
       frequency = 12), s.window="periodic", robust=TRUE) %>% autoplot()


valor_credito_emp <- base %>%
  group_by(cnpj, ano_emissao) %>%
  summarise(credito = sum(credito, na.rm = T))

ggplot(valor_credito_emp, aes(ano_emissao, credito))+
  geom_col()+
  theme_bw()

ggplot(valor_credito_emp, aes(ano_emissao, log(credito)))+
  geom_jitter(alpha =0.3)+
  geom_boxplot()+
  theme_bw()


# Evolução nota vs credito
nota_vs_credito <- merge(valor_nota, valor_credito, by = "mes_ano_emissao")
nota_vs_credito$tx_credito_nota <- nota_vs_credito$credito/nota_vs_credito$valor_nf * 100
ggplot(nota_vs_credito, aes(mes_ano_emissao, tx_credito_nota))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Crédito/Valor da Nota * 100")

stl(ts(valor_credito$credito,
       start = min(valor_credito$mes_ano_emissao),
       end = max(valor_credito$mes_ano_emissao),
       frequency = 12), s.window="periodic", robust=TRUE) %>% autoplot()


# Evolução do número de empresas
num_empresas <- base %>% 
  group_by(mes_ano_emissao)  %>%
  summarise(n_empresas = n_distinct(cnpj))
ggplot(num_empresas, aes(mes_ano_emissao, n_empresas))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Número de empresas")

stl(ts(num_empresas$n_empresas,
       start = min(num_empresas$mes_ano_emissao),
       end = max(num_empresas$mes_ano_emissao),
       frequency = 12), s.window="periodic", robust=TRUE) %>% autoplot()

num_empresas_ano <- base %>% 
  group_by(ano_emissao)  %>%
  summarise(n_empresas = n_distinct(cnpj))

ggplot(num_empresas_ano, aes(ano_emissao, n_empresas))+
  geom_col()+
  theme_bw()

# Evolução nota por empresa
nota_empresa <- merge(valor_nota, num_empresas, by = "mes_ano_emissao")
nota_empresa$valor_nota_empresa <- nota_empresa$valor_nf/nota_empresa$n_empresas * 100
ggplot(nota_empresa, aes(mes_ano_emissao, valor_nota_empresa))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Valor de nota (R$) por empresa")

# Evolução credito por empresa
credito_empresa <- merge(valor_credito, num_empresas, by = "mes_ano_emissao")
credito_empresa$cred_empresa <- credito_empresa$credito/credito_empresa$n_empresas * 100
ggplot(credito_empresa, aes(mes_ano_emissao, cred_empresa))+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("Crédito (R$) por empresa")

stl(ts(credito_empresa$cred_empresa,
       start = min(credito_empresa$mes_ano_emissao),
       end = max(credito_empresa$mes_ano_emissao),
       frequency = 12), s.window="periodic", robust=TRUE) %>% autoplot()


# Análise de CNAES --------------------------------------------------------
base_cnae_prep <- base %>% dplyr::select(cnpj, cnae_principal, cnae_secundaria)
base_cnae_prep <- unique(base_cnae_prep)

for(i in seq_along(base_cnae_prep$cnpj)){
base_cnae_prep$cnae[[i]] <- c(base_cnae_prep$cnae_principal[i], unlist(strsplit(base_cnae_prep$cnae_secundaria[i],",")))
}

base_cnae_prep$cnae_principal <- NULL
base_cnae_prep$cnae_secundaria <- NULL

base <- merge(base, base_cnae_prep, by = "cnpj", all.x = T)


# 2017
base_2017 <- subset(base, ano_emissao == 2017) 
base_2017 <- base_2017 %>% dplyr::select(cnpj, cnae) %>% unique()
cnae_2017 <- table(unlist(base_2017$cnae)) %>% data.frame()
cnae_2017$ano_emissao <- 2017
# 2018
base_2018 <- subset(base, ano_emissao == 2018) 
base_2018 <- base_2018 %>% dplyr::select(cnpj, cnae) %>% unique()
cnae_2018 <- table(unlist(base_2018$cnae)) %>% data.frame()
cnae_2018$ano_emissao <- 2018
# 2019
base_2019 <- subset(base, ano_emissao == 2019) 
base_2019 <- base_2019 %>% dplyr::select(cnpj, cnae) %>% unique()
cnae_2019 <- table(unlist(base_2019$cnae)) %>% data.frame()
cnae_2019$ano_emissao <- 2019
# 2020
base_2020 <- subset(base, ano_emissao == 2020) 
base_2020 <- base_2020 %>% dplyr::select(cnpj, cnae) %>% unique()
cnae_2020 <- table(unlist(base_2020$cnae)) %>% data.frame()
cnae_2020$ano_emissao <- 2020
# 2021
base_2021 <- subset(base, ano_emissao == 2021) 
base_2021 <- base_2021 %>% dplyr::select(cnpj, cnae) %>% unique()
cnae_2021 <- table(unlist(base_2021$cnae)) %>% data.frame()
cnae_2021$ano_emissao <- 2021

base_cnae <- rbind(cnae_2017, cnae_2018, cnae_2019, cnae_2020, cnae_2021)
base_cnae <- base_cnae %>%                                      
  arrange(desc(Freq)) %>% 
  group_by(ano_emissao) %>%
  slice(1:10)
names(base_cnae) <- c("cnae","quantidade", "ano_emissao")   

#Descrição dos cnae - https://concla.ibge.gov.br/classificacoes/download-concla.html

descricao_cnae <- read_excel("CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx", skip = 3)
descricao_cnae <- descricao_cnae[,c(5,6)]
names(descricao_cnae) <- c("cnae", "desc_cnae")
descricao_cnae <- na.omit(descricao_cnae)
descricao_cnae$cnae <- gsub("-","",descricao_cnae$cnae)
descricao_cnae$cnae <- gsub("/","",descricao_cnae$cnae)
base_cnae <- merge(base_cnae, descricao_cnae, by = "cnae", all.x = T)
base_cnae$desc_cnae <- as.factor(base_cnae$desc_cnae)

ggplot(base_cnae, aes(desc_cnae, quantidade, fill = desc_cnae))+
  geom_col()+
  coord_flip()+
  facet_wrap(~ano_emissao, ncol = 5)+
  theme_bw()+
  theme(legend.position="none")+
  xlab("")+
  ylab("")+
  ggtitle("Frequência dos CNAES",
          subtitle = "10 mais frequentes por ano"
  )
write.csv(base_cnae, "top_10_cnae.csv", row.names = F) 

# Análise da localização --------------------------------------------------
#https://www.alexcookson.com/post/2020-10-18-building-an-animation-step-by-step-with-gganimate/ - Boa referência

#Preparando a base
municipio <- read_delim("bases_receita_outros/F.K03200$Z.D20514.MUNICCSV",
                        delim = ";", escape_double = FALSE, col_names = FALSE,
                        trim_ws = TRUE)
names(municipio) <- c("municipio", "ds_municipio")

estado <- read_excel("estados.xlsx")

base <- merge(base, municipio, by = "municipio", all.x = T)
base <- merge(base, estado, by = "uf", all.x = T)

base$address <- paste0(base$tipo_logradouro, " ",base$logradouro, ", ", base$num_logradouro, ", ", base$ds_municipio, ", ", base$estado, ", ", "Brazil")

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

base$address <- rm_accent(base$address) %>% tolower() #removendo caracteres especiais para a geolocalização

base_geo <- unique(base$address) #mantendo endereços únicos

# Rodando geolocalização - Descomentar para conectar à API do OSM e geolocalizar os endereços
# base_geo <- geocode_OSM(base_geo, projection=4326, as.sf=F,as.data.frame = T)
# base_geo <- base_geo[,c(1:3)]
# names(base_geo) <- c("address", "long", "lat")
#write.csv(base_geo, "base_geo.csv", row.names = F)


base_geo <- read_csv("base_geo.csv") #lendo a base salva, para não ter que conectar à API o tempo todo
base_geo$...1 <- NULL
base <- merge(base, base_geo, by = "address", all.x = T)
analise_geo <- base %>% dplyr::select(cnpj,long, lat) %>% unique()
sum(is.na(analise_geo$long))
base_geo_exp <- base %>% dplyr::select(-cnae)
write.csv(base_geo_exp, "base.csv", row.names = F)

geo_ama <- data.frame(
              "Unidade" = c("AMA", "AMA", "AMA", "AMA", "AMA"), 
              "lat" = c("-23.561843055194757","-23.56161885723629", "-23.56582946732317", "-23.836188824082093", "-23.485894483947234"),
              "long" = c("-46.621708019719","-46.62170280468605", "-46.61981503089255", "-46.712785157877214", "-46.72403234438891"))
geo_ama$lat <- as.numeric(geo_ama$lat)
geo_ama$long <- as.numeric(geo_ama$long)
write.csv(geo_ama, "geo_ama.csv", row.names = F)

all_mun_sp <- read_municipality(code_muni=35, year=2018)
all_mun_sp$name_muni <- rm_accent(all_mun_sp$name_muni) %>% toupper()
mapa_sp <- base %>% dplyr::select(ds_municipio, credito, valor_nf, ano_emissao)
mapa_sp <- mapa_sp %>%
  group_by(ds_municipio, ano_emissao) %>%
  summarise(credito = sum(credito, na.rm = T),
            valor_nf = sum(valor_nf, na.rm = T))
municip_ano <- merge(unique(all_mun_sp$name_muni), c(2017, 2018, 2019, 2020, 2021), all = T)
names(municip_ano) <- c("ds_municipio", "ano_emissao")
mapa_sp <- merge(municip_ano, mapa_sp, by = c("ds_municipio", "ano_emissao"), all = T)
mapa_sp[is.na(mapa_sp)] <- 0
mapa_sp <- left_join(all_mun_sp, mapa_sp, by = c("name_muni" = "ds_municipio"))
mapa_sp$ano_emissao <- as.integer(mapa_sp$ano_emissao)

# Mapa das Notas fiscais
nf_gif <- ggplot() +
  geom_sf(data=mapa_sp, aes(fill=log(valor_nf)), color= "grey", size=.15)+
  paletteer::scale_fill_paletteer_c(palette = "viridis::plasma",
                       name="Log R$", na.value="white")+
  theme_minimal()+
  theme(plot.title = element_text(size=22))+
  labs(title = "Valor das Notas Fiscais", subtitle = "Year: {frame_time}", 
       caption='Fonte: Amabilidados', size=8) +
  transition_time(ano_emissao)+ 
  enter_fade() + 
  exit_shrink()

animate(nf_gif, height = 500, width = 800)
anim_save("nota_fiscal_mapa.gif")

# Mapa do crédito
credito_gif <- ggplot() +
  geom_sf(data=mapa_sp, aes(fill=log(credito)), color= "grey", size=.15)+
  paletteer::scale_fill_paletteer_c(palette = "viridis::plasma",
                                    name="Log R$", na.value="white")+
  theme_minimal()+
  theme(plot.title = element_text(size=22))+
  labs(title = "Valor do Crédito", subtitle = "Year: {frame_time}",
       caption='Fonte: Amabilidados', size=8) +
  transition_time(ano_emissao)+ 
  view_step(pause_length = 1, step_length = 20, nstep = 5)

animate(credito_gif, height = 500, width = 800, start_pause = 15, end_pause = 15, nframes = 300)
anim_save("credito_gifl_mapa.gif")

# Mapa das empresas
geo_empresas <- base %>%
  group_by(ano_emissao, ds_municipio) %>%
  summarise(n_empresas = n_distinct(cnpj))
geo_empresas <- merge(municip_ano, geo_empresas, by = c("ds_municipio", "ano_emissao"), all = T)
geo_empresas[is.na(geo_empresas)] <- 0
geo_empresas <- left_join(all_mun_sp, geo_empresas, by = c("name_muni" = "ds_municipio"))
geo_empresas$ano_emissao <- as.integer(geo_empresas$ano_emissao)

empresas_gif <- ggplot()+
  geom_sf(data=geo_empresas, aes(fill=log(n_empresas)), color= "grey", size=.15)+
  paletteer::scale_fill_paletteer_c(palette = "viridis::plasma",
                                    name="Log Número de Empresas", na.value="white")+
  # geom_point(data = geo_ama, aes(long, lat), size = 3, color = "darkgreen")+                                    
  # geom_text(data = geo_ama, aes(long[1], lat[1], label = Unidade), hjust = -1, vjust = 4, color = "darkgreen", size = 5)+ 
  theme_minimal()+
  theme(plot.title = element_text(size=22), plot.subtitle = element_text(size=18))+
  labs(title = "Número de Empresas", subtitle = "Ano: {frame_time}",
       caption='Fonte: Amabilidados', size=8) +
  transition_time(ano_emissao)+ 
  view_step(pause_length = 1, step_length = 20, nstep = 5)

animate(empresas_gif, height = 600, width = 900, start_pause = 15, end_pause = 15, nframes = 150)
anim_save("empresas_gif.gif")






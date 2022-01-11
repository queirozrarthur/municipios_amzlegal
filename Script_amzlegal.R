## Dados de Área Preservada e Emissão de Gases Poluentes

# Definir diretório de trabalho
setwd('C:\\Users\\queir\\OneDrive\\Documents\\Academia\\Amazonia MADE\\Classificação Municípios\\municipios_amzlegal')

# Carregar pacotes
library(tidyverse)
library(openxlsx)
library(reshape2)
library(RCurl)
library(FactoMineR)
library(factoextra)
library(cluster)

# Dados de Área Preservada
dados_preservada <- read.xlsx('Cobertura_dados.xlsx')

# Dados de Emissão de Gases
dados_gases <- read.xlsx('Gases_dados.xlsx')  

# Dados de População
dados_pop <- read.xlsx('POP_BASE_2000a2021.xlsx')

df_pop <- dados_pop %>% # Organizar a base de População 
  select(-c(Nível)) %>% 
  rename(geo_code = Cód.,
         city = Município) %>% 
  gather('year', 'pop', 3:21) %>% 
  select(-c(city)) %>% 
  mutate(geo_code = as.character(geo_code),
         year = as.character(year)) 

# Dados de Latitude e Longitude dos Municípios
dados_latlong <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv",
                         encoding = 'UTF-8')

lat_long <- dados_latlong %>% # Organizar a base de Latitude e Longitude
  select(codigo_ibge, nome, latitude, longitude) %>% 
  rename(geo_code = codigo_ibge) %>% 
  mutate(geo_code = as.character(geo_code))

# Organizar os dados de Área Preservada
df_preservada <- dados_preservada %>%
  select(state, city, geo_code, level_1, paste(1985:2020, sep = '')) %>% 
  gather('year', 'area', 5:40) %>% 
  filter(area != is.na(area)) %>% 
  group_by(geo_code, year) %>% 
  mutate(area_total = sum(area)) %>% 
  ungroup() %>% 
  filter(level_1 == '1. Forest' |
           level_1 == '2. Non Forest Natural Formation') %>% 
  group_by(geo_code, year) %>% 
  mutate(area_preservada = sum(area)) %>% 
  ungroup() %>%
  mutate(prop_preservada = (area_preservada/area_total)*100) %>% 
  select(c(state, city, geo_code, year, area_total, area_preservada, prop_preservada)) %>% 
  unique() %>% 
  filter(state == 'AC' |
           state == 'AM' |
           state == 'RR' |
           state == 'AP' |
           state == 'PA' |
           state == 'MA' |
           state == 'TO' |
           state == 'RO' |
           state == 'MT') 

write.csv2(df_preservada, 'AreaPreservada_1985a2020.csv', 
           row.names = F)

# Organizar os dados de Emissão de Gases
df_gases <- dados_gases %>% 
  gather('year', 'emission', 3:21) %>% 
  mutate(geo_code = as.character(geo_code),
         year = as.character(year)) %>% 
  left_join(df_pop, by = c('geo_code', 'year')) %>% 
  filter(year == 2012 |
           year == 2013 |
           year == 2014 | 
           year == 2015 |
           year == 2016 |
           year == 2017 |
           year == 2018) %>% 
  mutate(emission = as.numeric(emission),
         pop = as.numeric(pop)) %>% 
  mutate(emission_pc = emission/pop) %>% 
  select(geo_code, uf, year, emission, pop, emission_pc) %>% 
  left_join(lat_long, by = ('geo_code')) %>% 
  rename(city = nome,
         state = uf) %>% 
  filter(state == 'AC' |
           state == 'AM' |
           state == 'RR' |
           state == 'AP' |
           state == 'PA' |
           state == 'MA' |
           state == 'TO' |
           state == 'RO' |
           state == 'MT') %>% 
  select(geo_code, state, city, year, everything())

write.csv2(df_gases, 'EmissaoPerCapita_2012a2018.csv', 
           row.names = F)

## Organizar a base para realizar o agrupamento (Análise de Clusters)

# Área Preservada (pegar os dados de 2020 e a diferença com 1985)
df_preservada_2 <- df_preservada %>% 
  filter(year == 1985 |
           year == 2020) %>%
  select(state, city, geo_code, year, prop_preservada) %>% 
  spread(year, prop_preservada) %>%
  mutate(dif_preservada = `2020` - `1985`) %>% 
  select(-c(`1985`))

# Gases (pegar os dados mais recentes)
df_gases_2 <- df_gases %>% 
  filter(year==2018) %>% 
  select(geo_code, emission_pc)

# Juntar as duas bases e organizar para a análise
df_cluster <- df_preservada_2 %>% 
  left_join(df_gases_2, by = c('geo_code')) %>% 
  select(-c(state, city)) %>% 
  column_to_rownames('geo_code')

# Comando para gerar Cluster Aglomerativo/Hierárquico
agclust <- agnes(df_cluster, stand=T, metric='euclidean', method='ward')
ag3 <- cutree(agclust, k = 3)
summary(agclust)

# Avaliar os resultados
aggregate(df_cluster, by=list(cluster=ag3), mean)
aggregate(df_cluster, by=list(cluster=ag3), sd)

# Pegar os resultados
cluster_results <- cbind(rownames(df_cluster), cutree(agclust, 3))

df_cluster_2 <- df_cluster %>% 
  rownames_to_column('geo_code')

tab_cluster <- cluster_results %>%
  as.data.frame() %>% 
  rename(geo_code = V1,
         cluster = V2) %>% 
  left_join(df_cluster_2, by = c('geo_code')) %>%
  left_join(lat_long, by = c('geo_code')) %>% 
  rename('Participação 2020 (%)'= '2020',
         'Emissão Per Capita' = emission_pc,
         'Diferença (1985)' = dif_preservada,
         'Município' = nome,
         'Cluster' = cluster) %>% 
  select(geo_code, Município, Cluster, everything())

write.csv2(tab_cluster, 'ClusterAnalysis.csv', 
           row.names = F)


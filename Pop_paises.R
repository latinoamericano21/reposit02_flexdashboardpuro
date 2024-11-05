# 1.Instalar os pacotes "httr" e "jsonlite"

if(!require(httr)){install.packages("httr")};library("httr")
if(!require(jsonlite)){install.packages("jsonlite")};library("jsonlite")
if(!require(RCurl)){install.packages("RCurl")};library("RCurl")
if(!require(dplyr)){install.packages("dplyr")};library("dplyr")
if(!require(tidyverse)){install.packages("tidyverse")};library("tidyverse")
if(!require(magrittr)){install.packages("magrittr")};library("magrittr")
if(!require(ggplot2)){install.packages("ggplot2")};library("ggplot2")
if(!require(scales)){install.packages("scales")};library("scales")

# # 2. Fazer uma solicitação "GET" [adaptado para acessar dados do IBGE/SIDRA]
# url_da_base_de_dados2 <- "https://statistics.cepal.org/portal/cepalstat/open-data.html?lang=es#/Indicator/get_cepalstat_api_v1_indicator__indicator_id__areas"
# 
# pop_teste <-  GET(url_da_base_de_dados2)
# head(pop_teste)
# 
# ### ERROS daqui para baixo
# pop_teste_text <- content(pop_teste, "text")
# #pop_teste_text
# 
# pop_teste_json <- fromJSON(pop_teste_text, flatten = TRUE)
# # pop2022_json
# # 
# pop_teste_df <- as.data.frame(pop_teste_json)
# # View(pop2022_df)


#####     #####     #####
# 2B. Fazer uma solicitação "GET" [

# Declares the base url for calling the API
url_base <- "https://population.un.org/dataportalapi/api/v1"

# busca todos os indicadores em formato de string
indicadores <- getURL(paste0(url_base,"/indicators")) # concatena url_base e adendo ao endereço
indicadores <- fromJSON(indicadores, flatten = TRUE)

# busca todas as localizações em formato de string
localizacoes <- getURL(paste0(url_base,"/locations")) # concatena url_base e adendo ao endereço
localizacoes <- fromJSON(localizacoes, flatten = TRUE)

# token de acesso à API recebido por e-mail
headers = c(
  "Authorization" = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1bmlxdWVfbmFtZSI6ImplZmVzZ0B5YWhvby5jb20uYnIiLCJuYmYiOjE3MzA0Nzc3NDUsImV4cCI6MTc2MjAxMzc0NSwiaWF0IjoxNzMwNDc3NzQ1LCJpc3MiOiJkb3RuZXQtdXNlci1qd3RzIiwiYXVkIjoiZGF0YS1wb3J0YWwtYXBpIn0.lC6dq3irOXpN1Jpulzjhx4yLGJTP68GcACY90fNqUA0"
)

# Fazer a requisição dos dados:

## Teste de requisição
# res <- getURL("https://population.un.org/dataportalapi/api/v1/data/indicators/46/locations/900/start/2020/end/2025?pagingInHeader=false&format=json", .opts=list(httpheader = headers, followlocation = TRUE))
#res <- fromJSON(res, flatten = TRUE)

# Informar abaixo os indicadores e as localizacoes desejadas para requisitar:
indic_selec <- 49
localiz_selec <- "32,68,152"
ano_inic <- 2022
ano_final <- 2022

# Update the relative path to search for data on a specific indicator, location, and for specific years
target <- paste0(url_base, "/data/indicators/",indic_selec,"/locations/", localiz_selec,"/start/",ano_inic,"/end/", ano_final,"?pagingInHeader=false&format=json")

#pop_teste_bd3 <-  GET(target)
pop_paises_bd3 <- getURL(target, .opts=list(httpheader = headers, followlocation = TRUE))

pop_paises_json_bd3 <- fromJSON(pop_paises_bd3, flatten = TRUE)

pop_paises_df <- as.data.frame(pop_paises_json_bd3$data)

# # Get the other pages with data
# while (!is.null(pop2022_df$nextPage)){
# 
#   pop_teste_json_bd3 <- fromJSON(pop_teste_json_bd3$nextPage)
#   pop_teste_df <- rbind(pop_teste_df, pop_teste_json_bd3$data)
# }

# 3. Selecionar dados

pop_paises_selec <- pop_paises_df %>% filter(sex == "Both sexes")
pop_paises_selec <- pop_paises_selec %>% select(
  "location",
  "value"
)
colnames(pop_paises_selec)[1] <- "Pais"
colnames(pop_paises_selec)[2] <- "Populacao"


# 4. Trazendo os dados das UFs do Brasil

ufs_e_paises <- pop2022_trat %>% select("UF","Valor")
colnames(ufs_e_paises)[1] <- "Local"
colnames(ufs_e_paises)[2] <- "Populacao"
ufs_e_paises <- mutate(ufs_e_paises, UF_ou_Pais = "UF")

colnames(pop_paises_selec)[1] <- "Local"
pop_paises_selec <- mutate(pop_paises_selec, UF_ou_Pais = "Paises")

ufs_e_paises <- bind_rows(ufs_e_paises, pop_paises_selec)


# 4b. ordenar dados
ufs_e_paises <- arrange(ufs_e_paises, Populacao) # reorganiza o dtframe ordem cresc de Valor
ufs_e_paises$nro_ord_cresc <- seq(ufs_e_paises$Populacao) # cria/altera col com número na sequência crescente
ufs_e_paises <- arrange(ufs_e_paises, desc(Populacao)) # reorganiza o dtframe ordem decresc de Valor
ufs_e_paises$nro_ord_decresc <- seq(ufs_e_paises$Populacao) # cria/altera col com número na sequência decrescente


# 5. Gerar gráfico

### Gráfico de barras ###

x11(width = 5, height = 8) # define largura e altura do gráfico em polegadas
p <- ufs_e_paises %>%
  ggplot(aes(reorder(x = Local, nro_ord_cresc),       # especifica variável eixo x
             y= Populacao,
             #fill = Populacao,
             fill = UF_ou_Pais,
  ))    # especifica variável eixo y
p <- p + geom_bar(stat = "identity",
                  #fill = "lightblue", # cor do preenchimento das colunas
                  colour = "slateblue",
                  width = 0.5
)   # especifica formato do gráfico
p <- p + labs(x = "UFs do Brasil e Países",   # rótulo do eixo x
              y = "População",
              title = "População por Unidade da Federação",
              subtitle = "Ano 2022"
)   # rótulo do eixo y
p <- p + geom_text(aes(label = format(Populacao, big.mark = ".", decimal.mark = ",")),  # insere rótulos sobre as colunas
                   hjust = -0.2, # posiciona os rótulos das colunas
                   size = 3, # tamanho da fonte dos rótulos das colunas
                   colour = "black",
                   family = "Latin Modern Sans"
) # cor dos rótulos das colunas
#p <- p + scale_y_continuous(labels = label_number())   #eliminar a notação científica no eixo y
#p <- p + ylim(c(0,65000000))
p <- p + scale_y_continuous(limits = c(0,55000000), labels = label_comma(big.mark = ".", decimal.mark = ","))
# format(Valor, big.mark = ".", decimal.mark = ",", scientific = FALSE))
p <- p + theme_light()   # modifica o tema do gráfico
p <- p + theme(
  text = element_text(family = "Latin Modern Sans"),   # família da fonte geral
  plot.background = element_rect(fill = "gray90"),   # cor fundo nas margens
  plot.title = element_text(hjust=0.5), # centralizar o título
  plot.subtitle = element_text(hjust = 0.5), # centralizar o subtítulo
  axis.text = element_text(
    size = 10,   # tamanho da fonte dos rótulos dos eixos
    color = "#222222") #,
  # angle = 45)
) 
p <- p + coord_flip() # troca os eixos horizontal e vertical
p

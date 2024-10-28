# 1.Instalar os pacotes "httr" e "jsonlite"
#install.packages("httr")
#install.packages("jsonlite")
#library("httr")   #carrega biblioteca
#library("jsonlite")   #carrega biblioteca
if(!require(httr)){install.packages("httr")};library("httr")
if(!require(jsonlite)){install.packages("jsonlite")};library("jsonlite")
if(!require(dplyr)){install.packages("dplyr")};library("dplyr")
if(!require(ggplot2)){install.packages("ggplot2")};library("ggplot2")
if(!require(tidyverse)){install.packages("tidyverse")};library("tidyverse")
if(!require(scales)){install.packages("scales")};library("scales")
#if(!require(ggthemr)){install.packages("ggthemr")};library("ggthemr")
#####

# 2. Fazer uma solicitação "GET" [adaptado para acessar dados do IBGE/SIDRA]
url_da_base_de_dados <- "https://apisidra.ibge.gov.br/values/t/1209/n1/1/n3/all/v/allxp/p/2022/h/y"

pop2022 <-  GET(url_da_base_de_dados)
pop2022

pop2022_text <- content(pop2022, "text")
pop2022_text

pop2022_json <- fromJSON(pop2022_text, flatten = TRUE)
pop2022_json

pop2022_df <- as.data.frame(pop2022_json)
View(pop2022_df)

# 2.a - usar a linha 1 como cabeçalho do dataframe
nomesvar <- as.vector(t(pop2022_df[1,])) # gera um vetor igual a 1ª linha
colnames(pop2022_df) <- nomesvar  # rename também funciona?
pop2022_df <- pop2022_df[-1,] # exclui a 1ª linha
View(pop2022_df)

# outro modo
# nomesvar <- pop2022_df |> slice_head(n=1)
# nomesvar <- as.vector(t(unname(nomesvar)))
#####

# 3. Selecionar dados
pop2022_selec <- pop2022_df|>select(
  "Brasil e Unidade da Federação",
  "Valor"
  )
colnames(pop2022_selec)[1] <- "UF"
pop2022_selec <- filter(pop2022_selec, UF != "Brasil")
pop2022_selec$Valor <- as.integer(pop2022_selec$Valor)   # transforma a coluna de dados de string para inteiros
View(pop2022_selec)
#####

# 4. Tratar e ordenar dados
pop2022_trat <- pop2022_selec # gera cópía do dtfrm com outro nome
pop2022_trat <- arrange(pop2022_trat, Valor) # reorganiza o dtframe ordem cresc de Valor
pop2022_trat$nro_ord_cresc <- seq(pop2022_trat$Valor) # cria/altera col com número na sequência crescente
pop2022_trat <- arrange(pop2022_trat, desc(Valor)) # reorganiza o dtframe ordem decresc de Valor
pop2022_trat$nro_ord_decresc <- seq(pop2022_trat$Valor) # cria/altera col com número na sequência decrescente
View(pop2022_trat)

# 5. Gerar gráfico

### Gráfico de barras ###

p <- pop2022_trat %>%
  ggplot(aes(reorder(x = UF, nro_ord_cresc),       # especifica variável eixo x
             y= Valor,
             fill = Valor,
             ))    # especifica variável eixo y
p <- p + geom_bar(stat = "identity",
                  fill = "lightblue", # cor do preenchimento das colunas
                  colour = "slateblue",
                  width = 0.5
                  )   # especifica formato do gráfico
p <- p + labs(x = "Unidade da Federação",   # rótulo do eixo x
              y = "População",
              title = "População por Unidade da Federação",
              subtitle = "Censo Demográfico 2022"
              )   # rótulo do eixo y
p <- p + geom_text(aes(label = format(Valor, big.mark = ".", decimal.mark = ",")),  # insere rótulos sobre as colunas
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

#g01 <- g01 + scale_fill_brewer() #
#g01 <- g01 + scale_fill_viridis_b(option = "D")
#g01 <-  g01 + scale_fill_continuous(type = "viridis")
#g01 <- g01 + scale_x_reverse() #
#g01 <- g01 + scale_colour_brewer(aesthetics = "colour")
#g01 <- g01 + scale_y_continuous()
#g01 <- g01 + scale_fill_manual(values = alpha(c("blue", "red"), .3))

###   #####   ###

  










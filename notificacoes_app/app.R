#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(Metrics)

######################################################################################
# ATENÇÃO! Limiares epidêmicos colocar manualmente de:  https://docs.google.com/spreadsheets/d/1q_YxDpyzQjB65-X9ijYGKUp090w7axOH8hokb88zyHc/edit#gid=0
#####################################################################################
## Cores diponíveis para o shiny: https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
## publicar nuvem: https://cursos.alura.com.br/course/programacao-r-dashboard-online-shiny/task/85057

## para publicar
## rsconnect::setAccountInfo(name='srag',
#token='2E295F5C2E5CCB54BBFCA32A034725AB',
#secret='8lkxjzRt8guLVvO/Am97KOT3W9gwtt8TfhDJGFpZ')
# URL: https://srag.shinyapps.io/notificacoes_app/
## icons: https://getbootstrap.com/docs/3.4/components/#glyphicons
dados <- fread('www/notificacoes_geral.csv',encoding = 'UTF-8')
dados$ano <- substr(dados$data,1,4)
dados <- dados[order(ano),]
dados_ano_atual <- fread('www/ano_atual_geral.csv',encoding = 'UTF-8')
################################################################################################
cabecalho <- dashboardHeader(title = "Predição de Notificações de SRAG por Região", titleWidth = "40%")
barra_lateral <- dashboardSidebar(width = "160px", 
                                  tags$head(
                                      tags$style(HTML(".main-sidebar {background-color:  #adb3ba !important;}")),
                                      tags$style(HTML(".skin-blue .sidebar a { background-color:grey; color: #000000; }"))
                                  ),
                                  
                                      sidebarMenu(
                                        menuItem('Ano atual',
                                                   tabName = "anoAtual", ## é um ID
                                                   icon = icon('dashboard',
                                                               colors = 'blue'           
                                                   )),
                                      menuItem('Avaliação modelos',
                                               tabName = "dashboard", ## é um ID
                                               icon = icon('dashboard',
                                                colors = 'blue'           
                                                           )),
                                      menuItem('Informações',
                                               tabName = 'infos',
                                               icon = icon('info-circle')),
                                      HTML("<br><br>"),
                                      HTML('<center><img src="logo_fiocruz.png" width=90%"></center>'),
                                      HTML("<br>"),
                                      HTML('<center><img src="logo_ufcspa.jpg" width=50%"></center>')
                                  ))
painel_principal <- dashboardBody(
    tags$head(tags$style(HTML(".info-box, .info-box-icon, .small-box{height: 90px}"))),
    #tags$head(tags$style(HTML('.info-box {min-height: 25px;} .info-box-icon {height: 25px; line-height: 25px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;height: 25px;}'))),
    tabItems(
        tabItem(tabName = 'infos',
                column(width = 12,
                       h1("Informações"),
                fluidRow(
                    infoBox(title = 'Sobre', width = 6, icon = icon('glyphicon glyphicon-info-sign',lib = "glyphicon"),
                    subtitle = 'Esta aplicação foi desenvolvida como sub-produto do mestrado de Amauri Duarte da Silva, com a orientação dos professores:
                    Dra. Ana Gorini da Veiga e Dr. Marcelo Ferreira da Costa Gomes')
                ),
                fluidRow(
                    infoBox(title = 'Contatos', icon = icon('envelope-square'),
                            subtitle = HTML(paste("e-mail para contato:",br(),"amauri@ufcspa.edu.br"))),
                ),
                )
        ),
        tabItem(tabName = 'anoAtual',
                h1("Predição para o ano atual"),
                fluidRow(
                    column(width = 12,

                            column(width = 4,
                                   box(width = '100%',
                                       selectInput(inputId = "select_regiao_atual",
                                                   label = ("Região alvo:"),
                                                   choices = c(unique(dados$regiao))
                                       )
                                   )
                            ),
     
                        
                    )
                    ),
                fluidRow(
                    column(width = 8,
                           box(width = '100%', title = NULL,
                               plotlyOutput(outputId = 'notificacoes_ano_atual')
                           )
                    ),
                    
                  
                    column(width = 4,
                           fluidRow(
                                infoBox(title = 'Ano', value = dados_ano_atual$ano[1]
                                    ,color = 'yellow',
                                   width = 12,
                                    icon = icon("calendar-alt"))
                                ),
                           fluidRow(
                                  infoBox(title = HTML(paste("Período utilizado", "no treino do modelo", sep="<br/>")), value = paste(dados_ano_atual$ano_inicio[1]," - ",dados_ano_atual$ano_fim[1])
                                       ,color = 'navy',
                                       width = 12,
                                       icon = icon("calendar-alt"))
                           )
                           ,
                           fluidRow(
                           infoBoxOutput(width = 12, outputId = "total_notificacoes")
                           ),
                           h2("Métricas de treino"),
                           fluidRow(
                               valueBoxOutput(width = 6, outputId = "r2"),
                               valueBoxOutput(width = 6, outputId = "rmse"),
                           ),
                           fluidRow(
                               valueBoxOutput(width = 6, outputId = "mape"),
                               valueBoxOutput(width = 6, outputId = "mae")
                           ),

                    )
                )
                ),
        tabItem(tabName = 'dashboard',
                fluidRow(
                    column(width = 12,
                           h1("Análise do modelo para anos anteriores"),
                               column(width = 4,
                                      box(width = '100%',
                                          selectInput(inputId = "select_regiao",
                                                      label = ("Região para avaliar:"),
                                                      choices = c(unique(dados$regiao))
                                          )
                                      )
                               ),
                               column(width = 4,
                                      box(width = '100%',
                                          selectInput(inputId = "select_ano",
                                                      label = ("Ano para predição:"),
                                                      choices = c(unique(dados$ano))
                                          )
                                      )
                               ),
                               column(width = 4,
                                      box(width = '100%',
                                          selectInput(inputId = "select_ano_anterior",
                                                      label = ("Anos Anteriores:"),
                                                      choices = c(unique(dados$ano))
                                          )
                                      )
                               ),
                           
                           
                    )
                ), ## FINAL PRIMEIRA LINHA
                
                
                fluidRow(
                    
                    column(width = 6,
                           box(width = '100%', title = NULL,
                               plotlyOutput(outputId = 'notificacoes')
                           )
                    ),
                    column(width = 6,
                           box(width = '100%', title = NULL,
                               plotlyOutput(outputId = 'notificacoes_ano_anterior')
                           )
                    )
                ),
                fluidRow(
                    column(width = 8,
                           valueBoxOutput(width = 2, outputId = "r2_anterior"),
                           valueBoxOutput(width = 2, outputId = "pearson_anterior"),
                           valueBoxOutput(width = 2, outputId = "rmse_anterior"),
                           infoBoxOutput(width = 5, outputId = "explicacao_media")
                    )
                ),
         )
        
    )
    
) ## fim tabitem

ui <-dashboardPage(
    header = cabecalho,
    sidebar = barra_lateral,
    body = painel_principal
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dados_selecionados <-reactive({
        dados_filtro_regiao <- dados %>% filter(regiao == input$select_regiao)
        dados_filtro <- {dados_filtro_regiao %>% filter(ano == input$select_ano)}
        ############ transforma em semana de 7 dias
        semana <- 1
        count <- 1
        for(i in 1:nrow(dados_filtro)){
            dados_filtro $semana[i] <- semana
            count <- count+1
            if (count > 7){
                semana = semana+1
                count = 1
            }
        }
        # agrupa por semana
        dados_filtro <- dados_filtro  %>% 
            group_by(semana,regiao, ano_inicio, ano_fim) %>%                            # multiple group columns
            summarise(predito_semana = sum(predito), real_semana = sum(real), media_double = sum(media_notificacoes_dia))
   
    })

    dados_selecionados_ano_anterior <-reactive({
        dados_filtro_regiao <- dados %>% filter(regiao == input$select_regiao)
        dados_filtro <- {dados_filtro_regiao %>% filter(ano == input$select_ano_anterior)}
        ############ transforma em semana de 7 dias
        semana <- 1
        count <- 1
        for(i in 1:nrow(dados_filtro)){
            dados_filtro $semana[i] <- semana
            count <- count+1
            if (count > 7){
                semana = semana+1
                count = 1
            }
        }
        # agrupa por semana
        dados_filtro<- dados_filtro  %>% 
            group_by(semana,regiao) %>%                            # multiple group columns
            summarise(predito_semana = sum(predito), real_semana = sum(real))
    })
    ############## ANO ATUAL
    # cria coluna data (data primeiros sintomas)

    dados_selecionados_ano_atual <-reactive({
        dados_regiao <- dados_ano_atual %>%
            filter(regiao == input$select_regiao_atual)
        ano_data <- dados_regiao$ano[1]
        ano_data <- paste("01/01/",ano_data)
        ano_data <- gsub(" ", "", ano_data)
        ano_data <- as.Date(ano_data, format = "%m/%d/%Y")
        for(i in 1:nrow(dados_regiao)){
            dados_regiao$data[i] <- as.character(ano_data)
            ano_data <- ano_data+1
        }
        semana <- 1
        count <- 1
        for(i in 1:nrow(dados_regiao)){
            dados_regiao$semana[i] <- semana
            count <- count+1
            if (count > 7){
                semana = semana+1
                count = 1
                
            }
        }
        dados_regiao <- dados_regiao%>% 
            group_by(semana,regiao, limiar_pre_epidemico, intensidade_alta, intensidade_muito_alta,rmse,r2, mae, mape) %>%                            # multiple group columns
            summarise(predito = sum(notificacoes_dia))
    })

######################## fim TRATA DADOS ########################################   
    
################ GRÁFICOS ####################################
library(stringr)
    output$notificacoes <- renderPlotly({
        real <- dados_selecionados()$real_semana
        semana<- dados_selecionados()$semana
        predito <- dados_selecionados()$predito_semana
        media <- as.integer(dados_selecionados()$media_double) # arredonda media --> teve que criar variável com nome diferente
        ggplotly(
        ggplot()+
        geom_line(data=dados_selecionados(),aes(y=real,x= semana,colour="Real"),size=0.5 )+
        geom_line(data=dados_selecionados(),aes(y=predito,x= semana,colour="Predito"),size=0.5) +
            geom_line(data=dados_selecionados(),aes(y=media,x= semana,colour="Média"),size=0.5) +
        scale_color_manual(name = "Notificações", values = c("Real" = "darkblue", "Predito" = "red", "Média" = "orange"))+
        scale_x_continuous(expand = c(0, 0), limits = c(1,NA), breaks = seq(1, 53, 4)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
        ggtitle(paste('Região',str_to_title(input$select_regiao), " -  Predito X Realizado", "(", input$select_ano, ")"))+
        xlab('Semana não Epidemiológica') +
        ylab('Notificações')
        )
    })
    output$notificacoes_ano_anterior <- renderPlotly({
        real <-dados_selecionados_ano_anterior()$real_semana
        semana <- dados_selecionados_ano_anterior()$semana
        ggplotly(
            ggplot()+
                geom_line(data=dados_selecionados_ano_anterior(),aes(y=real,x= semana,colour="Real"),size=0.5 )+
                scale_color_manual(name = "Notificações", values = c("Real" = "darkblue"))+
                scale_x_continuous(expand = c(0, 0), limits = c(1,NA), breaks = seq(1, 53, 4)) + 
                scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
                ggtitle(paste('Região',str_to_title(input$select_regiao), " - Realizado", " (", input$select_ano_anterior,")"))+
                xlab('Semana não Epidemiológica') +
                ylab('Notificações')
        )
    })
    output$notificacoes_ano_atual <- renderPlotly({
        dados_regiao <- dados_selecionados_ano_atual()
        predito <-dados_regiao$predito
        semana <- dados_regiao$semana
        ggplotly(
            ggplot()+
                geom_line(data=dados_regiao,aes(y=predito,x= semana, colour="Predito"),size=0.5  )+
                geom_line(data=dados_regiao,aes(y= limiar_pre_epidemico,x= semana, colour="Limiar pré-epidêmico"),size=0.5, linetype = "dashed"  )+
                geom_line(data=dados_regiao,aes(y= intensidade_alta,x= semana, colour="Intensidade Alta"),size=0.5, linetype = "dashed"  )+
                geom_line(data=dados_regiao,aes(y= intensidade_muito_alta,x= semana, colour="Intensidade Muito Alta"),size=0.5, linetype = "dashed"  )+
                scale_color_manual(name = "Notificações", values = c("Predito" = "darkblue", "Limiar pré-epidêmico" = "#246e21", "Intensidade Alta" = "orange", "Intensidade Muito Alta" = "red" ))+
                scale_x_continuous(breaks = seq(1, 53, 4)) + 
                ggtitle(paste('Região',str_to_title(input$select_regiao_atual)))+
                xlab('Semana não Epidemiológica') +
                ylab('Notificações')
        )
    })
    #=================================== métricas
    # ========================== Anos anteriores
    output$r2_anterior <- renderValueBox({
        r2 <- cor(dados_selecionados()$real_semana, dados_selecionados()$predito_semana) ^ 2
        valueBox(value = format(round(r2, 2), nsmall = 2),
                 subtitle = "R²", icon = icon("chart-bar"), color = "light-blue"
        )
    })
    output$pearson_anterior <- renderValueBox({
        pearson <- cor(dados_selecionados()$real_semana, dados_selecionados()$predito_semana, method = c("pearson"))
        valueBox(value = format(round(pearson, 2), nsmall = 2),
                 subtitle = "Pearson", icon = icon("chart-bar"), color = "light-blue"
        )
    })
    output$explicacao_media <-  renderInfoBox({
        infoBox(title = 'OBSERVAÇÃO', icon = icon('glyphicon glyphicon-hand-right',lib = "glyphicon"), color = 'yellow',
        subtitle = paste('Média calculada para as notificações entre os anos de ',dados_selecionados()$ano_inicio[1]," e ",dados_selecionados()$ano_fim[1]))
    })

    output$rmse_anterior <- renderValueBox({
        rmse<- rmse(dados_selecionados()$real_semana, dados_selecionados()$predito_semana)
        valueBox(value = format(round(rmse, 2), nsmall = 2),
                 subtitle = "RMSE", icon = icon("chart-bar"), color = "light-blue"
        )
    })
    # ====================== ano atual
    output$rmse <- renderValueBox({
        valueBox(value = format(round(dados_selecionados_ano_atual()$rmse[1], 2), nsmall = 2),
                 subtitle = "RMSE", icon = icon("chart-bar"), color = "blue"
        )
    })
    output$r2 <- renderValueBox({
        valueBox(value = format(round(dados_selecionados_ano_atual()$r2[1], 2), nsmall = 2),
                 subtitle = "R²", icon = icon("chart-bar"), color = "blue"
        )
    })
    output$mae <- renderValueBox({
        valueBox(value = format(round(dados_selecionados_ano_atual()$mae[1], 2), nsmall = 2),
                 subtitle = "MAE", icon = icon("chart-bar"), color = "light-blue"
        )
    })
    output$mape<- renderValueBox({
        valueBox(value = format(round(dados_selecionados_ano_atual()$mape[1], 2), nsmall = 2),
                 subtitle = "MAPE", icon = icon("chart-bar"), color = "light-blue"
        )
    })
    output$total_notificacoes<- renderValueBox({
        infoBox(title = HTML(paste("Total de notificações", "previstas no ano", sep="<br/>")), value = sum(dados_selecionados_ano_atual()$predito),
                color = 'green',
                width = 8,
                icon = icon("calculator"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

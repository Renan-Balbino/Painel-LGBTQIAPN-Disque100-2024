library(shiny)

# Vetor de cores que você pode modificar
cores <- c("#A8333D", "#D15C2A", "#D7A72A", "#2B8C5F", "#1D6996", "#6B4C9A", 
           "#5E4B8B", "#B04A77", "#9A3F6A", "#1D6996", "#2A635D", "#3C3C3C", 
           "#2E3E50")

uicor <- fluidPage(
  titlePanel("Visualizador de Paleta de Cores"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("nova_cor", "Adicionar nova cor (hexadecimal):", ""),
      actionButton("adicionar", "Adicionar"),
      br(), br(),
      checkboxInput("texto_claro", "Texto branco sobre fundo?", TRUE)
    ),
    
    mainPanel(
      uiOutput("paleta")
    )
  )
)

servercor <- function(input, output, session) {
  # Paleta reativa
  paleta_cores <- reactiveVal(cores)
  
  observeEvent(input$adicionar, {
    nova <- input$nova_cor
    if (grepl("^#([A-Fa-f0-9]{6})$", nova)) {
      atual <- paleta_cores()
      paleta_cores(unique(c(atual, nova)))
    }
  })
  
  output$paleta <- renderUI({
    estilo_texto <- if (input$texto_claro) "color: white;" else "color: black;"
    divs <- lapply(paleta_cores(), function(cor) {
      div(
        style = paste0("background-color: ", cor, 
                       "; ", estilo_texto, 
                       " padding: 20px; margin-bottom: 10px; border-radius: 8px;"),
        strong(cor)
      )
    })
    do.call(tagList, divs)
  })
}

shinyApp(uicor, servercor)


# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)


library(shiny)
library(ggiraph)
library(ggplot2)

ui2 <- fluidPage(
  actionButton("mostrar_grafico", "Expandir Gráfico")
)

server2 <- function(input, output, session) {
  
  output$grafico_modal <- renderUI({
    girafeOutput("grafico_expandido", width = "100%", height = "500px")
  })
  
  output$grafico_expandido <- renderGirafe({
    gg <- ggplot(mtcars, aes(x = wt, y = mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(size = 4, color = "#B04A77") +
      theme_minimal()
    
    girafe(ggobj = gg, options = OptionsGirafe)
  })
  
  observeEvent(input$mostrar_grafico, {
    showModal(
      modalDialog(
        title = "Gráfico Interativo",
        girafeOutput("grafico_expandido"),
        size = "l",
        easyClose = TRUE, 
        fade = TRUE
      ), session
    )
  })
}

shinyApp(ui2, server2)



dates <- seq(as.Date("2018-01-01"), as.Date("2018-05-01"), by="days")
point_duration = rnorm(n=length(dates), mean=6, sd=1)
point_duration_bench = rnorm(n=length(dates), mean=5, sd=2)
df <- data.frame(dates, point_duration, point_duration_bench)
df$week <- strftime(df$dates, format = "%V")
df$month <- strftime(df$dates, format = "%m")

current_day = Sys.Date()
current_week = strftime(current_day, format = "%V")
current_month = strftime(current_day, format = "%m")


library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)

UI <- fluidPage(
  actionButton("month","Show last week"),
  plotOutput("line_graph"),
  bsModal("modalExample", 
          "Your plot", 
          "month", # <----set the observer to the right button
          size = "large",
          girafeOutput("PlotOutputRegiao", width = "100%", height = "auto"), 
          downloadButton('downloadPlot', 'Download'))
  
)

Server <- function(input, output) {
  
  # Dados reativos e filtrados:
  ReativoRegiao <- reactive({ DenunciaRegiao %>% filter(Individuo %in% input$selectInputRegiao) })
  
  
  # Grafico reativo:
  GrafRegiao <- reactive({
    
    ReativoRegiao() %>% filter(`Orientação Sexual` %in% input$CheckBoxRegiao) %>% 
      
      ungroup() %>% group_by(Regiao) %>% 
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>% 
      
      GDenunciaRegiao() })
  
  
  # Plotar grafico:
  output$PlotOutputRegiao <- renderGirafe({ 
    
    girafe(ggobj = GrafRegiao(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
}


shinyApp(ui = UI, server = Server)






library(shiny)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)

ui <- fluidPage(
  actionButton("btn", "Mostrar gráfico interativo")
)

server <- function(input, output, session) {
  output$grafico_interativo <- renderGirafe({
    gg <- ggplot(mtcars, aes(x = wt, y = mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(size = 4, color = "#B04A77") +
      theme_minimal()
    
    girafe(ggobj = gg)
  })
  
  observeEvent(input$btn, {
    showModal(modalDialog(
      title = "Gráfico Interativo",
      girafeOutput("grafico_interativo", height = "500px"),
      size = "l",
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)




library(shiny)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)

ui <- fluidPage(
  tags$style(HTML("
  .girafe_tooltip {
    z-index: 9999 !important;
    pointer-events: none;
  }
")),
  
  tags$h2("Exemplo com gráfico interativo ggiraph"),
  actionButton("btn", "Mostrar gráfico interativo")
)

server <- function(input, output, session) {
  
  output$grafico_interativo <- renderGirafe({
    gg <- ggplot(mtcars, aes(x = wt, y = mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(size = 4, color = "#B04A77") +
      theme_minimal()
    
    girafe(ggobj = gg, width_svg = 8, height_svg = 6)
  })
  
  observeEvent(input$btn, {
    showModal(modalDialog(
      girafeOutput("grafico_interativo", width = "100%", height = "500px"),
      title = "Gráfico Interativo",
      size = "l",
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)



library(shiny)
library(ggplot2)
library(ggiraph)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #overlay {
        display: none;
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background-color: rgba(0, 0, 0, 0.7);
        z-index: 9999;
      }
      #modalContent {
        position: absolute;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%);
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 0 10px #000;
        max-width: 90%;
        max-height: 90%;
        overflow: auto;
      }
    "))
  ),
  
  actionButton("show", "Mostrar Gráfico Interativo"),
  
  div(
    id = "overlay",
    div(
      id = "modalContent",
      girafeOutput("grafico_interativo", width = "100%", height = "500px"),
      br(),
      actionButton("close", "Fechar")
    )
  )
)

server <- function(input, output, session) {
  output$grafico_interativo <- renderGirafe({
    gg <- ggplot(mtcars, aes(wt, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#B04A77", size = 4) +
      theme_minimal()
    
    girafe(ggobj = gg)
  })
  
  observeEvent(input$show, {
    shinyjs::show("overlay")
  })
  
  observeEvent(input$close, {
    shinyjs::hide("overlay")
  })
}

shinyApp(ui, server)





library(shiny)
library(ggplot2)
library(ggiraph)
library(bslib)

ui2 <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("Painel com Gráficos Interativos"),
  
  # Botões para expandir gráfico 1 ou 2
  fluidRow(
    column(6, actionButton("expandir1", "Expandir gráfico 1")),
    column(6, actionButton("expandir2", "Expandir gráfico 2"))
  ),
  
  # Layout com os dois gráficos (mostra quando nenhum está expandido)
  conditionalPanel(
    condition = "input.expandir1 % 2 === 0 && input.expandir2 % 2 === 0",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Gráfico 1 - MPG vs WT"),
        girafeOutput("grafico1", height = "300px")
      ),
      card(
        card_header("Gráfico 2 - MPG vs HP"),
        girafeOutput("grafico2", height = "300px")
      )
    )
  ),
  
  # Layout com gráfico 1 expandido
  conditionalPanel(
    condition = "input.expandir1 % 2 === 1 && input.expandir2 % 2 === 0",
    card(
      card_header("Gráfico 1 Expandido"),
      girafeOutput("grafico_expandido1", height = "600px")
    )
  ),
  
  # Layout com gráfico 2 expandido
  conditionalPanel(
    condition = "input.expandir2 % 2 === 1 && input.expandir1 % 2 === 0",
    card(
      card_header("Gráfico 2 Expandido"),
      girafeOutput("grafico_expandido2", height = "600px")
    )
  )
)

server2 <- function(input, output, session) {
  
  # Gráfico 1
  output$grafico1 <- renderGirafe({
    g1 <- ggplot(mtcars, aes(wt, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#B04A77", size = 4) +
      theme_minimal()
    girafe(ggobj = g1)
  })
  
  # Gráfico 2
  output$grafico2 <- renderGirafe({
    g2 <- ggplot(mtcars, aes(hp, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#1D6996", size = 4) +
      theme_minimal()
    girafe(ggobj = g2)
  })
  
  # Gráfico 1 expandido
  output$grafico_expandido1 <- renderGirafe({
    g1_exp <- ggplot(mtcars, aes(wt, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#B04A77", size = 6) +
      labs(title = "Gráfico Expandido - MPG vs WT") +
      theme_minimal(base_size = 15)
    girafe(ggobj = g1_exp)
  })
  
  # Gráfico 2 expandido
  output$grafico_expandido2 <- renderGirafe({
    g2_exp <- ggplot(mtcars, aes(hp, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#1D6996", size = 6) +
      labs(title = "Gráfico Expandido - MPG vs HP") +
      theme_minimal(base_size = 15)
    girafe(ggobj = g2_exp)
  })
}

shinyApp(ui2, server2)






ui3 <- page_navbar(
  
  
  nav_panel(title = "Painel", 
            
            page_fluid(
              
                layout_columns(col_widths = c(6, 6), 
                               
                               # Regiao ----
                               cardRegiao, 
                               
                               
                               # Mes ----
                               cardMes), 
              
            ), 
            
  ) 
  
)

shinyApp(ui3, server3)



server3 <- function(input, output, session) {
  
  # Denuncias por Regiao e Orientacao sexual -------------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoRegiao <- reactive({ DenunciaRegiao %>% dplyr::filter(Individuo %in% input$selectInputRegiao) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputRegiao, {
    
    opcoes <- sort(unique(ReativoRegiao()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session, 
                             inputId = "CheckBoxRegiao", 
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafRegiao <- reactive({ 
    
    if (input$selectInputRegiao == "Vítima") {
      "Percentual de denúncias por região geográfica e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por região geográfica e pela orientação sexual"} })
  
  
  # Titulo no card:
  output$TituloPlotRegiao <- renderUI({ TitleGrafRegiao() })

    
  # Grafico reativo:
  GrafRegiao <- reactive({
    
    ReativoRegiao() %>% dplyr::filter(`Orientação Sexual` %in% input$CheckBoxRegiao) %>% 
      
      ungroup() %>% group_by(Regiao) %>% 
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>% 
      
      GDenunciaRegiao() })
  
  
  # Plotar grafico:
  output$PlotOutputRegiao <- renderGirafe({ 
    
    girafe(ggobj = GrafRegiao(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonRegiao <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafRegiao(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafRegiao()) })
  
  
  # Denuncias por Mes e Orientacao sexual ----------------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoMes <- reactive({ DenunciaMes %>% dplyr::filter(Individuo %in% input$selectInputMes) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputMes, {
    
    opcoes <- sort(unique(ReativoMes()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session,
                             inputId = "CheckBoxMes",
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafMes <- reactive({ 
    
    if (input$selectInputMes == "Vítima") {
      "Percentual de denúncias por mês e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por mês e pela orientação sexual"} })
  
  
  # Titulo no card:
  output$TituloPlotMes <- renderUI({ TitleGrafMes() })
  
  
  # Grafico reativo:
  GrafMes <- reactive({
    
    ReativoMes() %>% dplyr::filter(`Orientação Sexual` %in% input$CheckBoxMes) %>%
      
      ungroup() %>% group_by(Mes) %>%
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>%
      
      GDenunciaMes() })
  
  # Plotar grafico:
  output$PlotOutputMes <- renderGirafe({ 
    
    girafe(ggobj = GrafMes(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonMes <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafMes(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafMes()) })
}

shinyApp(ui3, server3)






library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  actionButton("mostrar", "Mostrar painel"),
  actionButton("esconder", "Esconder painel"),
  
  div(id = "painel_oculto",
      h3("Painel que aparece e desaparece"),
      p("Conteúdo aqui.")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$mostrar, {
    shinyjs::show("painel_oculto")
  })
  
  observeEvent(input$esconder, {
    shinyjs::hide("painel_oculto")
  })
  
  # Para iniciar escondido
  shinyjs::hide("painel_oculto")
}

shinyApp(ui, server)





library(shiny)
library(ggplot2)
library(ggiraph)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Painel com Gráficos Interativos"),
  uiOutput("painel_graficos")
)

server <- function(input, output, session) {
  estado <- reactiveValues(expandido = NULL)
  
  # Botões de expandir
  observeEvent(input$expandir1, {
    estado$expandido <- if (identical(estado$expandido, "g1")) NULL else "g1"
  })
  
  observeEvent(input$expandir2, {
    estado$expandido <- if (identical(estado$expandido, "g2")) NULL else "g2"
  })
  
  # Botão para voltar da visualização expandida
  observeEvent(input$voltar, {
    estado$expandido <- NULL
  })
  
  # Painel dinâmico
  output$painel_graficos <- renderUI({
    if (is.null(estado$expandido)) {
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Gráfico 1 - MPG vs WT"),
              actionButton("expandir1", "Expandir", class = "btn-sm")
            )
          ),
          girafeOutput("grafico1", height = "300px")
        ),
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Gráfico 2 - MPG vs HP"),
              actionButton("expandir2", "Expandir", class = "btn-sm")
            )
          ),
          girafeOutput("grafico2", height = "300px")
        )
      )
    } else if (estado$expandido == "g1") {
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span("Gráfico 1 Expandido"),
            actionButton("voltar", "Voltar", class = "btn-sm btn-secondary")
          )
        ),
        girafeOutput("grafico_expandido1", height = "600px")
      )
    } else if (estado$expandido == "g2") {
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span("Gráfico 2 Expandido"),
            actionButton("voltar", "Voltar", class = "btn-sm btn-secondary")
          )
        ),
        girafeOutput("grafico_expandido2", height = "600px")
      )
    }
  })
  
  # Gráficos
  output$grafico1 <- renderGirafe({
    g <- ggplot(mtcars, aes(wt, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#B04A77", size = 4) +
      theme_minimal()
    girafe(ggobj = g)
  })
  
  output$grafico2 <- renderGirafe({
    g <- ggplot(mtcars, aes(hp, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#1D6996", size = 4) +
      theme_minimal()
    girafe(ggobj = g)
  })
  
  output$grafico_expandido1 <- renderGirafe({
    g <- ggplot(mtcars, aes(wt, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#B04A77", size = 6) +
      labs(title = "Gráfico Expandido - MPG vs WT") +
      theme_minimal(base_size = 15)
    girafe(ggobj = g)
  })
  
  output$grafico_expandido2 <- renderGirafe({
    g <- ggplot(mtcars, aes(hp, mpg, tooltip = rownames(mtcars))) +
      geom_point_interactive(color = "#1D6996", size = 6) +
      labs(title = "Gráfico Expandido - MPG vs HP") +
      theme_minimal(base_size = 15)
    girafe(ggobj = g)
  })
}

shinyApp(ui, server)







library(shiny)
library(ggplot2)
library(ggiraph)
library(bslib)

# Função para gerar um card com botão de expandir
card_grafico <- function(id, titulo, botao_id, grafico_id) {
  card(
    card_header(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span(titulo),
        actionButton(botao_id, label = NULL, icon = icon("circle-plus"), class = "btn-sm")
      )
    ),
    girafeOutput(grafico_id, height = "300px")
  )
}




ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Painel com Gráficos Interativos"),
  uiOutput("Painel")
)



server <- function(input, output, session) {
  
  estado <- reactiveValues(expandido = NULL)
  
  # Observadores para expandir/recolher ------------------------------------------------------------------------------------
  
  observeEvent(input$actionButtonRegiao, {
    estado$expandido <- if (identical(estado$expandido, "Regiao")) NULL else "Regiao"
  })
  
  observeEvent(input$actionButtonMes, {
    estado$expandido <- if (identical(estado$expandido, "Mes")) NULL else "Mes"
  })
  
  observeEvent(input$actionButtonCorRaca, {
    estado$expandido <- if (identical(estado$expandido, "CorRaca")) NULL else "CorRaca"
  })
  
  observeEvent(input$actionButtonFaixaEtaria, {
    estado$expandido <- if (identical(estado$expandido, "FaixaEtaria")) NULL else "FaixaEtaria"
  })
  
  observeEvent(input$voltar, {
    estado$expandido <- NULL
  })
  
  
  # Painel dinamico --------------------------------------------------------------------------------------------------------
  
  output$Painel <- renderUI({
    
    if (is.null(estado$expandido)) {
      
      layout_columns(
        
        col_widths = c(6, 6),
        cardRegiao("770px"), 
        cardMes("770px"), 
        cardCorRaca("770px"), 
        cardFaixaEtaria("770px"))
      
    } else if (estado$expandido == "Regiao") { cardRegiao("1080px")
      
    } else if (estado$expandido == "Mes") { cardMes("1080px")
      
    } else if (estado$expandido == "CorRaca") { cardCorRaca("1080px")
      
    } else if (estado$expandido == "FaixaEtaria") { cardFaixaEtaria("1080px")
    }
  })
  
  
  # Denuncias por Regiao e Orientacao sexual -------------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoRegiao <- reactive({ DenunciaRegiao %>% 
      
      dplyr::filter(Individuo %in% input$selectInputRegiao) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputRegiao, {
    
    opcoes <- sort(unique(ReativoRegiao()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session, 
                             inputId = "CheckBoxRegiao", 
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafRegiao <- reactive({ 
    
    if (input$selectInputRegiao == "Vítima") {
      "Percentual de denúncias por região geográfica e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por região geográfica e pela orientação sexual"} })
  
  
  # Titulo no card:
  output$TituloPlotRegiao <- renderUI({ TitleGrafRegiao() })
  
  
  # Grafico reativo:
  GrafRegiao <- reactive({
    
    ReativoRegiao() %>% dplyr::filter(`Orientação Sexual` %in% 
                                        input$CheckBoxRegiao) %>% 
      
      ungroup() %>% group_by(Regiao) %>% 
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>% 
      
      GDenunciaRegiao() })
  
  
  # Plotar grafico:
  output$PlotOutputRegiao <- renderGirafe({ 
    
    girafe(ggobj = GrafRegiao(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonRegiao <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafRegiao(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafRegiao()) })
  
  
  # Denuncias por Mes e Orientacao sexual ----------------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoMes <- reactive({ DenunciaMes %>% dplyr::filter(Individuo %in% input$selectInputMes) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputMes, {
    
    opcoes <- sort(unique(ReativoMes()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session,
                             inputId = "CheckBoxMes",
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafMes <- reactive({ 
    
    if (input$selectInputMes == "Vítima") {
      "Percentual de denúncias por mês e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por mês e pela orientação sexual"} })
  
  
  # Titulo no card:
  output$TituloPlotMes <- renderUI({ TitleGrafMes() })
  
  
  # Grafico reativo:
  GrafMes <- reactive({
    
    ReativoMes() %>% dplyr::filter(`Orientação Sexual` %in% input$CheckBoxMes) %>%
      
      ungroup() %>% group_by(Mes) %>%
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>%
      
      GDenunciaMes() })
  
  # Plotar grafico:
  output$PlotOutputMes <- renderGirafe({ 
    
    girafe(ggobj = GrafMes(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonMes <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafMes(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafMes()) })
  
  
  # Denuncias por Cor/Raca e Orientacao sexual -----------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoCorRaca <- reactive({ DenunciaCorRaca %>% 
      
      dplyr::filter(Individuo %in% input$selectInputCorRaca) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputCorRaca, {
    
    opcoes <- sort(unique(ReativoCorRaca()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session,
                             inputId = "CheckBoxCorRaca",
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafCorRaca <- reactive({ 
    if (input$selectInputCorRaca == "Vítima") {
      "Percentual de denúncias por raça/cor declarada e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por raça/cor e pela orientação sexual"} })
  
  
  # Titulo no card:
  output$TituloPlotCorRaca <- renderUI({ TitleGrafCorRaca() })
  
  
  # Grafico reativo:
  GrafCorRaca <- reactive({
    
    ReativoCorRaca() %>% dplyr::filter(`Orientação Sexual` %in% input$CheckBoxCorRaca) %>%
      
      ungroup() %>% group_by(`Cor/Raça`) %>%
      
      mutate(Percentual = round(100 * Total / sum(Total), 2)) %>%
      
      GDenunciaCorRaca() })
  
  
  # Plotar grafico:
  output$PlotOutputCorRaca <- renderGirafe({ 
    
    girafe(ggobj = GrafCorRaca(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonCorRaca <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafCorRaca(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafCorRaca()) })
  
  
  # Denuncias por Faixa Etaria e Orientacao sexual -------------------------------------------------------------------------
  
  # Dados reativos e filtrados:
  ReativoFaixaEtaria <- reactive({ DenunciaFaixaEtaria %>% 
      
      dplyr::filter(Individuo %in% input$selectInputFaixaEtaria) })
  
  
  # Opcoes reativas das caixas de selecao:
  observeEvent(input$selectInputFaixaEtaria, {
    
    opcoes <- sort(unique(ReativoFaixaEtaria()$`Orientação Sexual`))
    
    updateCheckboxGroupInput(session,
                             inputId = "CheckBoxFaixaEtaria",
                             choices = opcoes, selected = opcoes) })
  
  
  # Titulo reativo:
  TitleGrafFaixaEtaria <- reactive({ 
    if (input$selectInputFaixaEtaria == "Vítima") {
      "Percentual de denúncias por faixa etária e pela orientação sexual das vítimas LGBTQIA+"
    } else {"Percentual de suspeitos por faixa etária e pela orientação sexual"}
  })
  
  
  # Titulo no card:
  output$TituloPlotFaixaEtaria <- renderUI({ TitleGrafFaixaEtaria() })
  
  
  # Grafico reativo:
  GrafFaixaEtaria <- reactive({
    
    # Percentual por Faixa Etaria (total de cada faixa):
    PercentualFaixaEtaria <- ReativoFaixaEtaria() %>%
      
      group_by(`Faixa Etária`) %>%
      
      summarise(TotalFaixa = sum(Total)) %>%
      
      mutate(Percentual = round(100 * TotalFaixa / sum(TotalFaixa), 2))
    
    
    ReativoFaixaEtaria() %>% dplyr::filter(`Orientação Sexual` %in% 
                                             input$CheckBoxFaixaEtaria) %>%
      
      GDenunciaFaixaEtaria(., PercentualFaixaEtaria) })
  
  
  # Plotar grafico:
  output$PlotOutputFaixaEtaria <- renderGirafe({ 
    
    girafe(ggobj = GrafFaixaEtaria(), 
           options = OptionsGirafe, 
           height_svg = 6, width_svg = 10.67) })
  
  
  # Baixar grafico:
  output$downloadButtonFaixaEtaria <- downloadHandler(
    
    filename = function() {
      
      paste0(TitleGrafFaixaEtaria(), ".png")
      
    }, content = function(file) { Salv.Graf(file, 2, GrafFaixaEtaria()) })
  
}

shinyApp(ui, server)

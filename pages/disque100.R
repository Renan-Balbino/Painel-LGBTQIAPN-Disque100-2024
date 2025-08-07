pagina1 <- function(){
  
  nav_panel(
    title = "Disque 100", 
    
    style = "color: black;",
    
    HTML(rep("<br>"), 4), 
    
    
    tagList(
      
      div(
        style = "max-width: 1080px; margin: 0 auto; padding: 20px;",
        
        # O que e?
        h5(
          div(style = "background-color: #6B4C9A; color: white; padding: 8px; text-align: left; border-radius: 5px; margin-bottom: 20px;", 
              bs_icon("telephone-fill", class = "me-2"), 
              span("Disque 100 – O que é?"))
        ),
        div(style = "font-size: 17px;", 
            p(
              "O Disque Direitos Humanos – Disque 100 é um serviço público gratuito do ",
              strong("Ministério dos Direitos Humanos e da Cidadania"),
              ". Ele recebe denúncias de violações de direitos humanos, especialmente contra pessoas em situação de vulnerabilidade, como:"),
            tags$ul(
              tags$li("Crianças e adolescentes"),
              tags$li("Idosos"),
              tags$li("Pessoas com deficiência"),
              tags$li("População LGBTQIA+"),
              tags$li("Pessoas em situação de rua"),
              tags$li("Indígenas, quilombolas, ciganos, entre outros")
            )),
        
        br(),
        
        # Funcoes do servico
        h5(
          div(style = "background-color: #6B4C9A; color: white; padding: 8px; text-align: left; border-radius: 5px; margin-bottom: 20px;", 
              bs_icon("shield-lock-fill", class = "me-2"),
              span("Funções do serviço"))
        ),
        div(style = "font-size: 17px;", 
            tags$ul(
              tags$li("Receber, analisar e encaminhar denúncias"),
              tags$li("Acionar autoridades competentes em casos graves e urgentes"),
              tags$li("Orientar e informar sobre direitos, programas e serviços públicos")
            )),
        
        br(), 
        
        # Como denunciar?
        h5(
          div(style = "background-color: #6B4C9A; color: white; padding: 8px; text-align: left; border-radius: 5px; margin-bottom: 20px;", 
              bs_icon("chat-dots-fill", class = "me-2"),
              span("Como denunciar?"))
        ),
        div(style = "font-size: 17px;", 
            p("O serviço funciona 24 horas por dia, todos os dias da semana, inclusive feriados. ",
              "Qualquer pessoa pode denunciar:"),
            strong("Canais disponíveis:"),
            tags$ul(
              tags$li(bs_icon("telephone", class = "me-2"), "Disque 100 (ligação gratuita de qualquer telefone)"),
              tags$li(bs_icon("whatsapp", class = "me-2"), "+55 61 99611-0100"),
              tags$li(
                bs_icon("globe", class = "me-2"),
                a("Site do Ministério", href = "https://www.gov.br/mdh/pt-br/ondh", target = "_blank"),
                "com chat e aqui", 
                a("videochamada em Libras", href = "https://atendelibras.mdh.gov.br/acesso", target = "_blank")
              ),
              tags$li(bs_icon("envelope", class = "me-2"), 
                      a("ouvidoria@mdh.gov.br", href = "mailto:ouvidoria@mdh.gov.br")),
              tags$li(bs_icon("send", class = "me-2"), 'Telegram: Buscar por "Direitoshumanosbrasil"'),
              tags$li(bs_icon("phone", class = "me-2"), strong('"Sabe"'), ": aplicativo informativo e educativo"),
              tags$li(bs_icon("geo-alt", class = "me-2"), "Presencial: Brasília/DF, na Ouvidoria Nacional")
            )),
        
        br(), 
        
        # Informacoes adicionais
        h5(
          div(style = "background-color: #6B4C9A; color: white; padding: 8px; text-align: left; border-radius: 5px; margin-bottom: 20px;", 
              bs_icon("info-circle-fill", class = "me-2"),
              span("Para mais informações"))
        ),
        div(style = "font-size: 17px;", 
            p(
              "Acesse o portal oficial em: ",
              a("https://www.gov.br/pt-br/servicos/denunciar-violacao-de-direitos-humanos",
                href = "https://www.gov.br/pt-br/servicos/denunciar-violacao-de-direitos-humanos",
                target = "_blank")
            ))
      )
      
    ), 
    
    # Colab
    
    )
}



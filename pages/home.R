painelUI <- function(){
  
  
  nav_panel(title = "Painel", 
            
            # id = "painel",
            tags$style(HTML("body { background-color: #F4F0F8 !important; }")), 
            
            
            HTML(rep("<br>"), 4), 
            
            
            # page_fluid(
              uiOutput("Painel"),
            # ),
            
            
            # Colab
  )
  
}


ui <- page_navbar(
  
  id = "pagina", 
  
  theme = myTema, 
  
  # fillable_mobile = TRUE, 
  
  navbar_options = navbar_options(theme = "dark", 
                                  bg = "#5E213F", position = "fixed-top"), 
  
  header = 
    tags$head(
      barraNavPag, 
      corpoPag, 
      rolagemPag, 
      tooltip.person), 
  
  
  nav_panel(title = "Painel", 
            
            
            fillPage(uiOutput("Painel")),
            
            
            Colab
            
            ), 
  

    nav_panel(title = "Disque 100", 
              
              fillPage(tagDisque100), 
              
              Colab
  ),

  
  
  nav_panel(title = "Sobre",

            tagSobre, 
            
            Colab
            ),
  
  
  nav_spacer(),
  
  
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank"),
    # nav_item(link_posit)
  ),
  
  
  
  
)

shinyApp(ui, server)
#
# ----------
# shinyApp(ui, function(input, output, session){ })

# runGadget(ui, function(input, output, session){ }, viewer = dialogViewer("", width = 1000, height = 610))

# runGadget(ui, server, viewer = dialogViewer("", width = 1000, height = 610))

# shinyApp(ui, function(input, output, session){ })

# bootswatch_themes(version = version_default(), full_path = FALSE)

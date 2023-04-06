fluidPage(
  
  # Application title
  titlePanel(h1("BiblioXploror",
                style="text-align: center; color: red;")),
  
  fluidRow(
    column(8, align="center", offset = 2,
           textInput(inputId="words", "Ecrivez un mot clé"),
           actionButton("submit_research", "Rechercher"),
           tags$style(type="text/css", "#words { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
           tags$style(type="text/css", "#submit_research {margin-bottom:50px;}")
    )
  ),
  
  tabsetPanel(
    tabPanel(title="Informations générales",
             uiOutput("controle_annee"),
             textOutput("pres_evol"),
             tags$style(type="text/css", "#pres_evol {margin-bottom:20px;}"),
             plotOutput("plot_evol_nbr_art_tps"),
             tags$style(type="text/css", "#plot_evol_nbr_art_tps {margin-bottom:30px;}"),
             tags$style(type="text/css", "#pres_pays {margin-bottom:20px;}"),
             textOutput("pres_pays"),
             plotOutput("plot_prod_pays"),
             tags$style(type="text/css", "#plot_prod_pays {margin-bottom:30px;}"),
             tags$style(type="text/css", "#pres_authors {margin-bottom:20px;}"),
             textOutput("pres_authors"),
             plotOutput("plot_authors"),
             tags$style(type="text/css", "#plot_authors {margin-bottom:30px;}"),
             tags$style(type="text/css", "#pres_source {margin-bottom:20px;}"),
             textOutput("pres_source"),
             tags$style(type="text/css", "#plot_prod_source {margin-bottom:30px;}"),
             plotOutput("plot_prod_source")
  
             
    ),
    tabPanel(title="Système de recommandation",
             
             textOutput("poids_variables_texte"),
             tags$style(type="text/css", "#poids_variables_texte {margin-bottom:20px;}"),
             DTOutput("poids_var"),
             fluidRow(column(8, align="center", offset = 2,
             uiOutput("refresh_wpca"))),
             tags$style(type="text/css", "#poids_var {margin-bottom:60px;}"),
             textOutput("results_texte"),
             tags$style(type="text/css", "#results_texte {margin-top:30px;}"),
             tags$style(type="text/css", "#results_texte {margin-bottom:20px;}"),
             DTOutput("myTable")),
    
    tabPanel(title="Graphiques sur l'ACP",
             uiOutput("choix_axes"),
             
             
             plotlyOutput("plot_scatter_WPCA"),
             tags$style(type="text/css", "#plot_scatter_WPCA {margin-bottom:50px;}"),
             plotOutput("plot_cor_WPCA")
    )
    
    
  )
)













# fluidPage(
#   
#   # Application title
#   titlePanel("Titre de notre appli"),
#   
#   fluidRow(
#       column(8, align="center", offset = 2,
#              textInput(inputId="words", "Ecrivez ici un mot clé"),
#              actionButton("submit_research", "research"),
#              tags$style(type="text/css", "#words { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
#       ),
#     ),
#   
#   DTOutput("poids_var"),
#   DTOutput("myTable"),
#   uiOutput("refresh_wpca"),
#   uiOutput("choix_axes"),
#   
#   
#   plotlyOutput("plot_scatter_WPCA"),
#   plotOutput("plot_cor_WPCA")
#   
# )



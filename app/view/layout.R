
box::use(
  app/logic/ui_globals[ExplorePageContents,footerHTML,DownloadPageContents],
  app/logic/utils,
  app/logic/data[deg_contrasts],
)

box::use(
  shiny[...], 
  htmlwidgets[...], 
  bslib[bs_theme], 
  htmltools[HTML, includeHTML]
)

#'@export
ui <- function(id){
  ns <- NS(id)
  tagList(
    
    navbarPage(
      title = "LiverDB",
      id = "liverdb",
      theme = bslib::bs_theme(bootswatch = "lumen"),
      tabPanel(
        title = "Home",
        id = "home-tab",
        value = "aboutTab",
        icon = icon("home"),
        fluidPage(br(), includeHTML("www/home.html"))
      ),
      tabPanel(
        title = "Explore",
        id = "explore-tab",
        icon = icon('table'),
        ExplorePageContents(deg_contrasts) #ExplorePageContents(results)
      ),
      tabPanel(
        title = "Download",
        id = "download-tab",
        icon = icon('download'),
        DownloadPageContents()
      ),
      tabPanel(
        title = "Documentation",
        id = "docs-tab",
        icon = icon('file-alt'),
        tags$iframe(
          src = './documentation.html',
          width = '100%', height = '800px',
          frameborder = 0,
          scrolling = 'auto'
        )
      )
    ), 
    tags$footer(HTML(footerHTML()))
  )
}

#'@export
server <- function(id) {
      moduleServer(
        id, 
       
        function(input, output, session) {
          print("server part works!")
        })
}

box::use(
  shiny[NS,tabPanel,icon,fluidPage,fluidRow,column,selectInput,dataTableOutput,
        moduleServer,renderTable,h2,h3,hr,p]
)

box::use(
  app/logic/b_explore_utils[ExplorePageContents]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Explore",
    id = "explore-tab",
    icon = icon('table'),
    
    column(4,
           
           fluidRow( h2("Explore results"),hr(),
             column(6,
                   #GSE drop down
                   (selectInput("dropdown1","Study",
                                choices = c("GSE112057","GSE123141","GSE83687"))),
                   p("view study [link] on GEO."),
                    ),
             column(6, 
                   #contrasts drop down
                   (selectInput("dropdown2","Contrast (Numerator vs. Denominator)",
                                choices = c("Control","Etc")))
                    ), hr(), h3("Results Table")
             
      )
    ), #end of left column here
  column(8,
         
      p("ExplorePageContents function will go here -- needs deg_contrasts data")   
         
      ) #end of right column here
  ) 
}


#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    output$table1 <- renderTable({
     #table
    })

  })  
}


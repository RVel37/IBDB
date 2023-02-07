#memory
box::use(
  shiny[observe,moduleServer, invalidateLater,renderUI],
  lobstr[mem_used]
)

print("Mem used at start:")
print(lobstr::mem_used() / 1e6)

#'@export
mem_server <- function(id){
  moduleServer(
    id, function(input, output, session) {
 
      # Print memory usage to console every 2000ms
      observe({
        invalidateLater(2000, session)
        print(lobstr::mem_used() / 1e6)
      },
      priority = 1000)
})}

#'@export
link_server <- function(id){
  moduleServer(
    id, function(input, output, session) {
#link to study
output$studyLink <- renderUI({
  study <- input$selectStudy
  
  helpText(
    "View study ",
    a(study, href = paste0(GEO_BASE, study), target = "_blank"),
    " on GEO."
  )}
)}
)}


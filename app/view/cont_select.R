box::use(
  dplyr[filter, pull],
  shiny[selectInput,moduleServer,renderUI],
)

box::use(
  app/logic/data[deg_contrasts],
  app/view/memory[link_server]
)

#'@export
server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {


# reactive contrast selector
output$UIselectContrast <- renderUI({
  study <- input$selectStudy
  cont_labels <- deg_contrasts %>%
    dplyr::filter(study_id == study) %>% 
    unite("contrast", c("numerator", "denominator"), sep = " vs. ") %>% 
    pull(contrast)

selectInput(
    inputId = "selectContrast",
    label = "Contrast (Numerator vs. Denominator)",
    selected = cont_labels[1],
    choices = cont_labels)
  })
})
}

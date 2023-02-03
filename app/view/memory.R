#memory
box::use(
  shiny[observe,moduleServer, invalidateLater],
  lobstr[mem_used]
)

print("Mem used at start:")
print(lobstr::mem_used() / 1e6)

#'@export
server <- function(id){
  moduleServer(
    id, function(input, output, session) {
 
      # Print memory usage to console every 2000ms
      observe({
        invalidateLater(2000, session)
        print(lobstr::mem_used() / 1e6)
      },
      priority = 1000)
})}
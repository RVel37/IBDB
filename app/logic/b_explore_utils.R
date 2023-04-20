box::use(
  shinipsum[random_ggplot],
  shiny[...],
  prompter[add_prompt]
)

# ------------------------ plotting
#'@export
get_random_ggplot <- function(plot_type) {
  random_ggplot(plot_type)
}

# ------------------------ ? button
#'@export
helpButton <- function(message) {
  return(
    add_prompt(
      ui_element = span(HTML('<i class="fa fa-question-circle"></i>')),
      message = message, position = "right"
    )
  )
}

# ------------------------ Make headers
#'@export
makeHeaders <- function(title, message, fs=1.3) {
  tagList(
    span(span(title, style=paste0("font-size: ", fs, "em;")), helpButton(message))
  )
}

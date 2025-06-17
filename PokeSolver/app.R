library(shiny)

# Example image URL map
image_url_map <- list(
  "A_X" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/1.png",
  "A_Y" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/2.png",
  "A_Z" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/3.png",
  "B_X" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/4.png",
  "B_Y" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/5.png",
  "B_Z" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/6.png",
  "C_X" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/7.png",
  "C_Y" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/8.png",
  "C_Z" = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/9.png"
)

#source("functions.R")

# UI
ui <- fluidPage(
  titlePanel("PokeDoku Solver"),
  
  fluidRow(
    column(2,
           selectInput("h1", "Vertical 1", choices = c("X", "Y", "Z")),
           selectInput("h2", "Vertical 2", choices = c("X", "Y", "Z")),
           selectInput("h3", "Vertical 3", choices = c("X", "Y", "Z"))
    ),
    column(10,
           fluidRow(
             column(4, selectInput("v1", "Horizontal 1", choices = c("A", "B", "C"))),
             column(4, selectInput("v2", "Horizontal 2", choices = c("A", "B", "C"))),
             column(4, selectInput("v3", "Horizontal 3", choices = c("A", "B", "C")))
           ),
           br(),
           fluidRow(
             lapply(1:3, function(i) {
               column(4, 
                      lapply(1:3, function(j) {
                        uiOutput(paste0("img_", i, "_", j))
                      })
               )
             })
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  for (i in 1:3) {
    for (j in 1:3) {
      local({
        ii <- i; jj <- j
        output[[paste0("img_", ii, "_", jj)]] <- renderUI({
          v_val <- input[[paste0("v", ii)]]
          h_val <- input[[paste0("h", jj)]]
          key <- paste0(v_val, "_", h_val)
          url <- image_url_map[[key]]
          if (is.null(url)) url <- "https://via.placeholder.com/200?text=No+Image"
          
          tags$img(src = url, width = "100%", style = "margin-bottom: 10px;")
        })
      })
    }
  }
}

shinyApp(ui, server)

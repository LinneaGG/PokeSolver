library(shiny)

source("../functions.R")

ui <- fluidPage(
  titlePanel("PokeDoku Solver"),
  
  actionButton("go", "Solve!"), 
  
  fluidRow(
    column(2,
           selectInput("hint2_1", "Vertical 1", choices = c("fairy", "flying", "bug")),
           selectInput("hint2_2", "Vertical 2", choices = c("poison", "flying", "bug")),
           selectInput("hint2_3", "Vertical 3", choices = c("poison", "flying", "bug"))
    ),
    column(10,
           fluidRow(
             column(4, selectInput("hint1_1", "Horizontal 1", choices = c("ground", "fire", "bug"))),
             column(4, selectInput("hint1_2", "Horizontal 2", choices = c("grass", "fire", "water"))),
             column(4, selectInput("hint1_3", "Horizontal 3", choices = c("grass", "fire", "water")))
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

server <- function(input, output, session) {
  all_poke_file <- "../all_poke_df_v3.csv"
  
  default_img <- "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/0.png"
  
  # Reactive value to store the grid
  grid_images <- reactiveVal(matrix(default_img, nrow = 3, ncol = 3))
  
  # Update grid on button press
  observeEvent(input$go, {
    new_grid <- matrix(default_img, nrow = 3, ncol = 3)
    
    hint_columns <- c(input$hint2_1, input$hint2_2, input$hint2_3)
    hint_rows <- c(input$hint1_1, input$hint1_2, input$hint1_3)
    
    result <- solve_pokedoku(all_poke_file, hint_columns, hint_rows)
    
    k <- 1
    for (i in 1:3){
      for (j in 1:3){
        if (nrow(result$result[[k]]) > 0 && !is_empty(result$result[[k]]$sprite)) {
          new_grid[i, j] <- result$result[[k]]$sprite[1]
        }
        k <- k + 1
      }
    }
    grid_images(new_grid)
  })
  
  # Render each image slot
  for (i in 1:3) {
    for (j in 1:3) {
      local({
        ii <- i; jj <- j
        output[[paste0("img_", ii, "_", jj)]] <- renderUI({
          img_url <- grid_images()[ii, jj]
          tags$img(src = img_url, width = "200px", style = "margin: 5px;")
        })
      })
    }
  }
}

shinyApp(ui, server)

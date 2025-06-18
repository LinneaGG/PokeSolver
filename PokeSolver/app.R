library(shiny)
source("../functions.R")  # Make sure this works!

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
  
  grid_images <- reactiveVal(matrix(default_img, nrow = 3, ncol = 3))
  sprite_options <- reactiveVal(vector("list", 9))  # Store all sprite options
  
  observeEvent(input$go, {
    new_grid <- matrix(default_img, nrow = 3, ncol = 3)
    new_options <- vector("list", 9)
    
    hint_columns <- c(input$hint2_1, input$hint2_2, input$hint2_3)
    hint_rows <- c(input$hint1_1, input$hint1_2, input$hint1_3)
    result <- solve_pokedoku(all_poke_file, hint_columns, hint_rows)
    
    k <- 1
    for (i in 1:3) {
      for (j in 1:3) {
        if (nrow(result$result[[k]]) > 0) {
          sprites <- result$result[[k]]$sprite
          if (!is.null(sprites) && length(sprites) > 0) {
            new_grid[i, j] <- sprites[1]
            new_options[[k]] <- sprites
          }
        }
        k <- k + 1
      }
    }
    
    grid_images(new_grid)
    sprite_options(new_options)
  })
  
  # Render each cell as a button with an image
  for (i in 1:3) {
    for (j in 1:3) {
      local({
        ii <- i; jj <- j
        output_id <- paste0("img_", ii, "_", jj)
        button_id <- paste0("btn_", ii, "_", jj)
        
        output[[output_id]] <- renderUI({
          actionButton(
            inputId = button_id,
            label = HTML(sprintf(
              '<img src="%s" width="100px" height="100px" style="margin: 5px;">',
              grid_images()[ii, jj]
            )),
            style = "padding: 0; border: none; background: none;"
          )
        })
        
        # Show modal with sprite choices
        observeEvent(input[[button_id]], {
          k <- (ii - 1) * 3 + jj
          options <- sprite_options()[[k]]
          
          if (!is.null(options) && length(options) > 0) {
            showModal(modalDialog(
              title = paste("Select a Pokémon for cell [", ii, ",", jj, "]"),
              easyClose = TRUE,
              footer = NULL,
              fluidRow(
                lapply(seq_along(options), function(idx) {
                  select_id <- paste0("select_", ii, "_", jj, "_", idx)
                  column(2, actionButton(
                    inputId = select_id,
                    label = HTML(sprintf(
                      '<img src="%s" width="80px" height="80px">',
                      options[idx]
                    )),
                    style = "padding: 0; border: none; background: none;"
                  ))
                })
              )
            ))
          }
        })
        
        # Handle selection from modal
        observe({
          k <- (ii - 1) * 3 + jj
          options <- sprite_options()[[k]]
          lapply(seq_along(options), function(idx) {
            select_id <- paste0("select_", ii, "_", jj, "_", idx)
            observeEvent(input[[select_id]], {
              current_grid <- grid_images()
              current_grid[ii, jj] <- options[idx]
              grid_images(current_grid)
              removeModal()
            })
          })
        })
      })
    }
  }
}

shinyApp(ui, server)

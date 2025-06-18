library(shiny)
library(shinythemes)

source("functions.R")
all_poke_file <- "all_poke_df_v4.csv"
all_poke_original <- read.csv(all_poke_file)

hint_options <- c(
  unique(all_poke_original$Type1), 
  unique(all_poke_original$Type2), 
  unique(all_poke_original$Region),
  c("fossil", "legendary", "mythical", "baby", "mega", "gmax", "first_partner", "paradox", "ultra_beast","mono_type", "dual_type")) %>%  
  tolower() %>%  
  unique() %>%  
  sort()

ui <- fluidPage(

  theme = shinytheme("simplex"),
  
  tags$head(
    tags$style(HTML("
      .tooltip-container {
        position: relative;
        display: inline-block;
      }
      .tooltip-container .tooltip-text {
        visibility: hidden;
        background-color: #333;
        color: #fff;
        text-align: center;
        padding: 6px 12px;
        border-radius: 6px;
        position: absolute;
        z-index: 1000;
        top: -15px;
        left: 50%;
        transform: translateX(-50%);
        white-space: nowrap;
        font-size: 16px;
      }
      .tooltip-container:hover .tooltip-text {
        visibility: visible;
      }
    "))
  ),
  
  titlePanel("PokeDoku Solver"),
  
  actionButton("go", "Solve!", class = "btn btn-danger"),

  
  fluidRow(
    # Left column: vertical hints
    column(2,
           div(
            selectInput("hint2_1", "", choices = c("Enter hint" = "", str_to_title(hint_options)), selected = ""),
            style = "margin-bottom: 130px; margin-top: 145px;"
           ), 
           div(
            selectInput("hint2_2", "", choices = c("Enter hint" = "", str_to_title(hint_options)), selected = ""),
            style = "margin-bottom: 130px;"
           ), 
            selectInput("hint2_3", "", choices = c("Enter hint" = "", str_to_title(hint_options)), selected = "")
    ),
    
    # Right side: horizontal hints + image grid
    column(10,
           # Horizontal hints (top row)
           fluidRow(
             column(4, selectInput("hint1_1", "", choices = c("Enter hint" = "", str_to_title(hint_options))), selected = ""),
             column(4, selectInput("hint1_2", "", choices = c("Enter hint" = "", str_to_title(hint_options))), selected = ""),
             column(4, selectInput("hint1_3", "", choices = c("Enter hint" = "", str_to_title(hint_options))), selected = "")
           ),
           br(),
           
           # Grid of outputs (images)
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
  default_img <- "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/0.png"
  
  show_tooltips <- reactiveVal(FALSE)
  
  grid_images <- reactiveVal(matrix(default_img, nrow = 3, ncol = 3))
  sprite_options <- reactiveVal(vector("list", 9))
  sprite_names <- reactiveVal(vector("list", 9))  # Store names for tooltips
  
  observeEvent(input$go, {
    new_grid <- matrix(default_img, nrow = 3, ncol = 3)
    new_options <- vector("list", 9)
    new_names <- vector("list", 9)
    
    hint_columns <- c(input$hint2_1, input$hint2_2, input$hint2_3)
    hint_rows <- c(input$hint1_1, input$hint1_2, input$hint1_3)
    
    show_tooltips(TRUE)
    
    if (any(hint_columns == "") || any(hint_rows == "")) {
      showModal(modalDialog(
        title = "Missing Input",
        "Please select all row and column hints before solving.",
        easyClose = TRUE
      ))
      return()
    }
    
    result <- solve_pokedoku(all_poke_file, hint_columns, hint_rows)
    
    k <- 1
    for (i in 1:3) {
      for (j in 1:3) {
        if (nrow(result$result[[k]]) > 0) {
          sprites <- result$result[[k]]$sprite
          names <- str_to_title(result$result[[k]]$name)
          if (!is.null(sprites) && length(sprites) > 0) {
            new_grid[i, j] <- sprites[1]
            new_options[[k]] <- sprites
            new_names[[k]] <- names
          }
        }
        k <- k + 1
      }
    }
    
    grid_images(new_grid)
    sprite_options(new_options)
    sprite_names(new_names)
  })
  
  for (i in 1:3) {
    for (j in 1:3) {
      local({
        ii <- i; jj <- j
        output_id <- paste0("img_", ii, "_", jj)
        button_id <- paste0("btn_", ii, "_", jj)
        
        output[[output_id]] <- renderUI({
          k <- (ii - 1) * 3 + jj
          sprite <- grid_images()[ii, jj]
          name_list <- sprite_names()[[k]]
          name <- if (!is.null(name_list) && length(name_list) > 0) name_list[1] else "No valid Pokémon"
          
          tooltip_html <- if (show_tooltips() && name != "") {
            sprintf('<div class="tooltip-container">
               <img src="%s" width="180px" height="180px" style="margin: 5px;">
               <div class="tooltip-text">%s</div>
             </div>', sprite, name)
          } else {
            sprintf('<img src="%s" width="180px" height="180px" style="margin: 5px;">', sprite)
          }
          
          actionButton(
            inputId = button_id,
            label = HTML(tooltip_html),
            style = "padding: 0; border: none; background: none;"
          )
        })
        
        
        observeEvent(input[[button_id]], {
          k <- (ii - 1) * 3 + jj
          options <- sprite_options()[[k]]
          names <- sprite_names()[[k]]
          
          if (!is.null(options) && length(options) > 0) {
            showModal(modalDialog(
              title = "Select another Pokémon",
              easyClose = TRUE,
              footer = NULL,
              fluidRow(
                lapply(seq_along(options), function(idx) {
                  select_id <- paste0("select_", ii, "_", jj, "_", idx)
                  name <- if (!is.null(names) && length(names) >= idx) names[idx] else "Unknown"
                  
                  column(2, actionButton(
                    inputId = select_id,
                    label = HTML(sprintf(
                      '<div class="tooltip-container">
                         <img src="%s" width="80px" height="80px">
                         <div class="tooltip-text">%s</div>
                       </div>',
                      options[idx], name
                    )),
                    style = "padding: 0; border: none; background: none;"
                  ))
                })
              )
            ))
          }
        })
        
        observe({
          k <- (ii - 1) * 3 + jj
          options <- sprite_options()[[k]]
          names <- sprite_names()[[k]]
          lapply(seq_along(options), function(idx) {
            select_id <- paste0("select_", ii, "_", jj, "_", idx)
            observeEvent(input[[select_id]], {
              current_grid <- grid_images()
              current_names <- sprite_names()
              
              current_grid[ii, jj] <- options[idx]
              if (!is.null(names) && length(names) >= idx) {
                current_names[[k]][1] <- names[idx]
              }
              
              grid_images(current_grid)
              sprite_names(current_names)
              removeModal()
            })
          })
        })
        
      })
    }
  }
}

shinyApp(ui, server)

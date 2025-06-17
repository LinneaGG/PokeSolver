library(shiny)

source("../functions.R")

# UI
ui <- fluidPage(
  titlePanel("PokeDoku Solver"),
  
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
  for (i in 1:3) {
    for (j in 1:3) {
      local({
        ii <- i; jj <- j
        output[[paste0("img_", i, "_", j)]] <- renderUI({
          # url <- match_pokemon(all_poke_df, input[[paste0(hint1, "_", ii)]], 
          #                      input[[paste0(hint2, "_", ii)]])[1,2] # Pick first pokemon in df
          # 
          # if (is.null(url)) url <- "https://via.placeholder.com/200?text=No+Image"
          
          hint1_input <- input[[paste0("hint1_", ii)]]
          hint2_input <- input[[paste0("hint2_", jj)]]
          
          result <- match_pokemon(all_poke_df, hint1_input, hint2_input)
          
          url <- if (nrow(result) > 0) result$Sprite[1] else "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/0.png"
          

          tags$img(src = url, width = "100%", style = "margin-bottom: 10px;")
        })
      })
    }
  }
}

shinyApp(ui, server)

## LOAD NECESSARY LIBRARIES

library(tidyverse)
library(fastDummies)
library(httr)
library(jsonlite)
library(tibble)
library(purrr)
library(stringr)
library(tidyr)
library(future)

## PRE-DEFINED VARIABLES AND VECTORS

fossil_pokemon <- c(
  "omanyte", "omastar", "kabuto", "kabutops", "aerodactyl", "aerodactyl-mega",
  "lileep", "cradily", "anorith", "armaldo",
  "cranidos", "rampardos", "shieldon", "bastiodon",
  "tirtouga", "carracosta", "archen", "archeops",
  "tyrunt", "tyrantrum", "amaura", "aurorus",
  "dracozolt", "arctozolt", "dracovish", "arctovish")

hisuian_pokemon <- c(
  "voltorb-hisui",
  "electrode-hisui",
  "growlithe-hisui",
  "arcanine-hisui",
  "lilligant-hisui",
  "basculin-white-striped",
  "zorua-hisui",
  "zoroark-hisui",
  "braviary-hisui",
  "samurott-hisui",
  "decidueye-hisui",
  "qwilfish-hisui",
  "sliggoo-hisui",
  "goodra-hisui",
  "overqwil",
  "avalugg-hisui",
  "kleavor",
  "ursaluna",
  "wyrdeer",
  "basculegion-male",
  "basculegion-female",
  "enamorus-encarnate",
  "enamorus-therian",
  "palkia-origin",
  "dialga-origin"
)


## FUNCTION TO GET ALL POKEMON NAMES

get_all_pokemon_names <- function() {
  url <- "https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0"
  res <- httr::GET(url)
  
  if (res$status_code != 200) {
    stop("Error fetching data from API")
  }
  
  data <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  pokemon_names <- data$results$name
  return(pokemon_names)
}


## FUNCTION TO OBTAIN DATA FROM A GIVEN POKÉMON

catch_pokemon <- function(name) {
  tryCatch({
    url <- paste0("https://pokeapi.co/api/v2/pokemon/", tolower(name))
    resp <- GET(url)
    if (status_code(resp) != 200) return(NULL)
    
    poke_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    types <- poke_data$types$type$name
    Type1 <- types[1]
    Type2 <- if (length(types) > 1) types[2] else NA_character_
    species_url <- poke_data$species$url
    species_resp <- GET(species_url)
    species_data <- fromJSON(content(species_resp, "text", encoding = "UTF-8"))
    
    region_map <- c(
      "generation-i" = "kanto", "generation-ii" = "johto",
      "generation-iii" = "hoenn", "generation-iv" = "sinnoh",
      "generation-v" = "unova", "generation-vi" = "kalos",
      "generation-vii" = "alola", "generation-viii" = "galar",
      "generation-ix" = "paldea"
    )
    
    tibble(
      Name = tolower(name),
      Type1 = Type1,
      Type2 = Type2,
      Region = ifelse(tolower(name) %in% hisuian_pokemon, "hisui", region_map[[species_data$generation$name]]),
      Legendary = species_data$is_legendary,
      Mythical = species_data$is_mythical,
      Baby = species_data$is_baby,
      Fossil = ifelse(tolower(name) %in% fossil_pokemon, TRUE, FALSE),
      Mega = grepl("-mega", tolower(name)),
      Gmax = grepl("-gmax", tolower(name)),
      `Mono-type` = is.na(Type2),
      `Dual-type` = !is.na(Type2),
      Sprite = poke_data$sprites$front_default
    )
  }, error = function(e) {
    message(paste("Error with", name, ":", e$message))
    return(NULL)
  })
}


## UPDATE THE ALL POKEMON DATA FRAME FROM THE API

update_all_poke_df <- function(version = 1, workers = 4, delay = 0.5, output_dir = ".") {
  
  all_names <- get_all_pokemon_names()
  
  message("Retrieving all Pokémon data from API...")
  plan(multisession, workers = workers)
  
  all_pokemon <- future_map(
    all_names,
    function(name) {
      Sys.sleep(delay)  # avoid API rate-limiting
      catch_pokemon(name)
    },
    .progress = TRUE
  )
  
  all_poke_df <- bind_rows(all_pokemon)
  
  output_file <- file.path(output_dir, paste0("all_poke_df_v", version, ".csv"))
  write_csv(all_poke_df, output_file)
  message("Done! File saved to: ", output_file)
  
  return(invisible(all_poke_df))
}


## CONVERT POKEMON TABLE TO ONE-HOT ENCODING FORMAT

# One-hot encoding categorical variables

all_poke_df_original <- read_csv("all_poke_df_v1.csv")

# Put type columns into a single column 
all_poke_df <- all_poke_df_original %>%
  pivot_longer(cols = c(Type1, Type2),
               names_to  = "type_rm",
               values_to = "Type") %>%
  na.omit() %>%    # drop any NA “Type2”
  dplyr::select(-type_rm) %>% 
  # Make dummy variables
  dummy_cols(select_columns = c("Type", "Region"), 
             remove_selected_columns = TRUE) %>% 
  # Combine rows so that there's 1 row per pokemon
  group_by(Name) %>%
  mutate(across(starts_with("Type_"), ~ max(.x, na.rm = TRUE))) %>% 
  distinct() %>% 
  # Change from 1/0 to TRUE/FALSE
  mutate(across(starts_with("Type_")  , ~ . == 1),
         across(starts_with("Region_"), ~ . == 1))


# Declare which type the hint belongs to 
# (to be able to grab it from the df)
type_list <- unique(c(all_poke_df_original$Type1, all_poke_df_original$Type2))
region_list <- unique(all_poke_df_original$Region)


# Select row based on 2 strings
# Return row in all_poke_df where certain columns are TRUE
match_pokemon <- function(df, hint1, hint2) {
  if (hint1 %in% type_list){
    # Append "Type_" before hint1
    hint1 <- paste0("Type_", hint1)
  }
  if (hint2 %in% type_list){
    hint2 <- paste0("Type_", hint2)
  } 
  if (hint1 %in% region_list){
    hint1 <- paste0("Region_", hint1)
  } 
  if (hint2 %in% region_list){
    hint2 <- paste0("Region_", hint2)
  }
  ########## Change case as needed!!!!!!!!
  df %>%
    filter(!!sym(hint1) & !!sym(hint2)) %>%
    dplyr::select(Name) # And image url
}

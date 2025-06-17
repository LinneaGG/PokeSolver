# One-hot encoding categorical variables

library(tidyverse)
library(fastDummies)

# Put type columns into a single column 
all_poke_df <- all_poke_df_test %>%
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
type_list <- unique(c(all_poke_df_test$Type1, all_poke_df_test$Type2))
region_list <- unique(all_poke_df_test$Region)


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
    dplyr::select(Name)
}

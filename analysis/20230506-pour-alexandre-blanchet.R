library(httr)
library(lubridate)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(purrr)
library(ggplot2)

# define todo #########
contributions_files <- list.files(
  "data/",
  pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\dcontributions-pro-fr.csv"
) 
dates_contributions_files <- contributions_files %>% str_extract("\\d\\d\\d\\d-\\d\\d-\\d\\d") 


# define functions  ######
generate_wrangled_contrib_file <- function(mydate){
  message(mydate)
  dest_file <- paste0("data/", mydate, "contributions-pro-fr.csv")
  contributions_raw <- readr::read_delim(dest_file, 
                                         locale = locale(encoding = "windows-1252"), 
                                         col_types = cols(.default = col_character()),
                                         delim =";") %>%
    janitor::clean_names()
  
  
  #https://www.electionsquebec.qc.ca/cartes-electorales/entites-administratives-liees-aux-circonscriptions/#anchor4
  # https://docs.electionsquebec.qc.ca/ORG/6109a4fc846d2/CP_CIRC_MUN_MRC_RA_BRUT.zip
  code_postal_to_circ <-  readr::read_delim("data/CP_CIRC_MUN_MRC_RA_BRUT.txt", 
                                            locale = locale(encoding = "windows-1252"), 
                                            col_types = cols(.default = col_character()),
                                            delim =";") %>%
    janitor::clean_names() %>%
    rename(code_postal = co_postl) 
  
  code_postal_to_circ_slice1 <- code_postal_to_circ %>%  group_by(code_postal) %>% slice(1) %>%  ungroup()
  
  contributions <- contributions_raw %>% 
    mutate(
      annee_financiere = as.numeric(annee_financiere),
      montant_total = as.numeric(str_replace(montant_total, ",", ".")),
      entite_politique = case_when(
        entite_politique == "Québec solidaire  (avant fusion)"~ "Québec solidaire", 
        entite_politique == "Coalition avenir Québec - L'équipe François Legault" ~ "Coalition avenir Québec",
        entite_politique == "Parti conservateur du Québec - Équipe Éric Duhaime" ~ "Parti conservateur du Québec",
        entite_politique == "Parti libéral du Québec/Quebec Liberal Party" ~ "Parti libéral du Québec",
        TRUE ~ entite_politique)
    )  %>% 
    filter(entite_politique %in% c(
      "Québec solidaire", 
      "Parti québécois", 
      "Parti libéral du Québec", 
      "Parti conservateur du Québec",  
      "Coalition avenir Québec" ) 
    )  %>%
    filter(type_dentite_politique == "Parti") #%>%
  #left_join(code_postal_to_circ %>% rename(code_postal = co_postl))
  
  
  wrangled_file <- contributions %>% left_join(code_postal_to_circ_slice1)
  
  return(wrangled_file)
}

last_date <- max(dates_contributions_files)

lastfile <- generate_wrangled_contrib_file(last_date)

annees <- lastfile %>% count(annee_financiere)
nb_donateurs <- lastfile %>% distinct(nom_prenom, code_postal)

nb_deux_dons_meme_annee <- lastfile %>% 
  filter(montant_total >= 50) %>% 
  distinct(nom_prenom, code_postal, annee_financiere, entite_politique) %>%
  count(nom_prenom, code_postal, annee_financiere) %>% 
  filter(n>=2)

nb_donateurs_deux_dons_meme_annees <- nb_deux_dons_meme_annee %>% distinct(nom_prenom, code_postal)

library(tidyr)
wide <- lastfile %>% 
  select(nom_prenom, code_postal, annee_financiere, entite_politique, montant_total) %>% 
  inner_join(nb_deux_dons_meme_annee) %>% 
  mutate(entite_politique = 
           case_when(
             entite_politique=="Québec solidaire"~ "QS",
             entite_politique=="Parti québécois" ~ "PQ",
             entite_politique=="Coalition avenir Québec" ~ "CAQ",
             entite_politique=="Parti conservateur du Québec"~ "PCQ",
             entite_politique=="Parti libéral du Québec" ~ "PLQ"
           )
  ) %>% 
  group_by(nom_prenom, code_postal, annee_financiere) %>%
  pivot_wider(names_from = entite_politique, values_from= montant_total,  values_fill = 0 )

paires <- 
   lastfile %>% 
  select(nom_prenom, code_postal, annee_financiere, entite_politique, montant_total) %>% 
  inner_join(nb_deux_dons_meme_annee) %>% 
  mutate(entite_politique = 
           case_when(
             entite_politique=="Québec solidaire"~ "QS",
             entite_politique=="Parti québécois" ~ "PQ",
             entite_politique=="Coalition avenir Québec" ~ "CAQ",
             entite_politique=="Parti conservateur du Québec"~ "PCQ",
             entite_politique=="Parti libéral du Québec" ~ "PLQ"
           )
  ) %>% 
  group_by(nom_prenom, code_postal, annee_financiere) %>%
  arrange(entite_politique) %>% 
  summarise(paires  = paste0(entite_politique, collapse = "-"))  %>% 
  ungroup() %>%
  arrange(nom_prenom, code_postal, annee_financiere)

paires  %>% count(annee_financiere, paires) %>% 
  arrange(desc(annee_financiere), desc(n)) %>% 
  print(n=100)

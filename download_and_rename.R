library(httr)
library(lubridate)
library(dplyr)
library(janitor)
library(stringr)

url <- "https://donnees.electionsquebec.qc.ca/production/provincial/financement/contribution/contributions-pro-fr.csv"
dest_file <- paste0("data/", Sys.Date(), "contributions-pro-fr.csv")


response <- GET(url, write_disk(dest_file, overwrite = TRUE), progress())



contributions_raw <- readr::read_delim(dest_file, 
                  locale = locale(encoding = "windows-1252"), 
                  col_types = cols(.default = col_character()),
                  delim =";") %>%
  janitor::clean_names()


cumulatif_path <- paste0("data/", Sys.Date(),"_cumulatif.csv")
cumulatif_circ_path <- paste0("data/", Sys.Date(),"_cumulatif_circ.csv")

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

cumulatif <- 
  wrangled_file %>% 
  filter(annee_financiere>= 2023) %>% 
  group_by(entite_politique, annee_financiere) %>% 
  summarise(montant_cumulatif = sum(montant_total)) %>% 
  ungroup() %>%
  mutate(date_cumulatif = today())
  
cumulatif_circ <-   wrangled_file %>% 
  filter(annee_financiere>= 2023) %>% 
  group_by(entite_politique, annee_financiere,  co_circ, nm_circ) %>% 
  summarise(montant_cumulatif = sum(montant_total)) %>% 
  ungroup() %>%
  mutate(date_cumulatif = today())
  
write_csv(cumulatif, cumulatif_path)
write_csv(cumulatif_circ, cumulatif_circ_path)


if (http_status(response)$category == "Success") {
  cat("File downloaded and saved as", dest_file, "\n")
} else {
  cat("Error downloading the file. Please try again later.\n")
}

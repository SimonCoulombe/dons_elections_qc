library(httr)
library(lubridate)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(purrr)
library(ggplot2)



contributions_files <- list.files(
  "data/",
  pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\dcontributions-pro-fr.csv"
) 
dates_contributions_files <- contributions_files %>% str_extract("\\d\\d\\d\\d-\\d\\d-\\d\\d") 


cumulatif_files <- list.files(
  "data/",
  pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\d_cumulatif.csv"
) 
dates_cumulatif_files <- cumulatif_files %>% str_extract("\\d\\d\\d\\d-\\d\\d-\\d\\d") 

todo <- dates_contributions_files[!dates_contributions_files %in%  dates_cumulatif_files]

generate_cumulatif <- function(mydate){
  
  dest_file <- paste0("data/", mydate, "contributions-pro-fr.csv")
  
  contributions_raw <- readr::read_delim(dest_file, 
                                         locale = locale(encoding = "windows-1252"), 
                                         col_types = cols(.default = col_character()),
                                         delim =";") %>%
    janitor::clean_names()
  
  
  cumulatif_path <- paste0("data/", mydate,"_cumulatif.csv")
  cumulatif_circ_path <- paste0("data/", mydate,"_cumulatif_circ.csv")
  
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
    mutate(date_cumulatif = lubridate::ymd(mydate))
  
  cumulatif_circ <-   wrangled_file %>% 
    filter(annee_financiere>= 2023) %>% 
    group_by(entite_politique, annee_financiere,  co_circ, nm_circ) %>% 
    summarise(montant_cumulatif = sum(montant_total)) %>% 
    ungroup() %>%
    mutate(date_cumulatif = lubridate::ymd(mydate))
  
  write_csv(cumulatif, cumulatif_path)
  write_csv(cumulatif_circ, cumulatif_circ_path)
  
  list_cumulatifs <- list.files("data/", pattern= "*cumulatif.csv")
  cumulatifs_quotidiens <- dplyr::bind_rows(purrr::map(list_cumulatifs,   ~readr::read_csv(paste0("data/",.x)))) %>%
    arrange(desc(date_cumulatif), entite_politique)
  write_csv(cumulatifs_quotidiens, "data/cumulatif_quotidiens.csv")
  
  
  list_cumulatifs_circ <- list.files("data/", pattern= "*cumulatif_circ.csv")
  cumulatifs_circ_quotidiens <- dplyr::bind_rows(purrr::map(list_cumulatifs_circ,   ~readr::read_csv(paste0("data/",.x))))%>%
    arrange(desc(date_cumulatif), entite_politique)
  write_csv(cumulatifs_circ_quotidiens, "data/cumulatifs_circ_quotidiens.csv")
  
  
}



for (i in todo){
  print(i)
  generate_cumulatif(i)
}


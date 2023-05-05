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


cumulatif_files <- list.files(
  "data/",
  pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\d_cumulatif.csv"
) 
dates_cumulatif_files <- cumulatif_files %>% str_extract("\\d\\d\\d\\d-\\d\\d-\\d\\d") 

todo <- dates_contributions_files[!dates_contributions_files %in%  dates_cumulatif_files]

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

generate_transfuge <- function(wrangled_file){
  # recherche aux transfuges depuis l'an passé
  # un transfuge est quelqu un qui donne a un seul parti cette annee 
  # et un seul l an passé
  # mais pas le même 
  last2years_1partimax <- wrangled_file %>% 
    filter(annee_financiere>= max(annee_financiere)- 1) %>%
    group_by(nom_prenom, code_postal, annee_financiere) %>%
    filter(n() == 1) %>% 
    ungroup()
  
  transfuges <-  last2years_1partimax %>% 
    filter(annee_financiere== max(annee_financiere)) %>%
    select(nom_prenom, entite_politique, code_postal, annee_financiere) %>%
    inner_join(# deja vu quelque part l an passé
      last2years_1partimax %>% 
        filter(annee_financiere == (max(annee_financiere)-1 )) %>% 
        select(nom_prenom,  code_postal) %>% 
        distinct() 
    ) %>% #mais pas chez nous
    anti_join(
      last2years_1partimax %>% 
        filter(annee_financiere == (max(annee_financiere))-1) %>% 
        select(nom_prenom,  code_postal,entite_politique) %>% 
        distinct() 
    )
  
  transfuges %>% 
    left_join(
      last2years_1partimax %>% 
        filter(annee_financiere == max(annee_financiere)-1 ) %>%
        select(nom_prenom, code_postal, parti_origine = entite_politique)
    )
  
  
}


generate_transfuges_origin_destination <- function(transfuges, mydate){
  transfuges_origin_destination <- transfuges %>% 
    mutate(date= mydate) %>% 
    count(entite_politique, parti_origine)
  
  transfuges_origin_destination
  
}

generate_transfuges_summary <- function(transfuges,mydate){
  
  transfuges_origin_destination <-
    generate_transfuges_origin_destination(transfuges, mydate)
  
  transfuges_summary <- 
    transfuges_origin_destination %>% 
    group_by(entite_politique) %>% 
    summarise(transfuges_entrants = sum(n)) %>%
    ungroup() %>%
    left_join(transfuges_origin_destination %>% 
                group_by(parti_origine) %>% 
                summarise(transfuges_sortants = sum(n))%>%
                ungroup() %>%
                rename(entite_politique = parti_origine))
}

generate_anciens_donateurs <- function(wrangled_file){
  anciens_donateurs <- wrangled_file %>% 
    filter(annee_financiere < max(annee_financiere)) %>% 
    select(nom_prenom, entite_politique, code_postal) %>% 
    distinct() %>%
    mutate(ancien_donateur = 1)
  
  anciens_donateurs
}


generate_cumulatif <- function(mydate,wrangled_file){
  
  cumulatif_path <- paste0("data/", mydate,"_cumulatif.csv")
  cumulatif_circ_path <- paste0("data/", mydate,"_cumulatif_circ.csv")
  
  anciens_donateurs = generate_anciens_donateurs(wrangled_file)
  
  cumulatif <- 
    wrangled_file %>% 
    filter(annee_financiere == max(annee_financiere)) %>% 
    left_join(anciens_donateurs) %>%
    mutate(ancien_donateur = as.numeric(!is.na(ancien_donateur))) %>%
    group_by(entite_politique, annee_financiere) %>% 
    summarise(montant_cumulatif = sum(montant_total), 
              donateurs = n(), 
              anciens_donateurs =  sum(ancien_donateur), 
              nouveaux_donateurs = sum(ancien_donateur ==0), 
              cumulatif_anciens = sum(montant_total * ancien_donateur),
              cumulatif_nouveaux = sum(montant_total * (ancien_donateur==0)),
              pct_nouveaux_donateurs= nouveaux_donateurs / donateurs,
              pct_cumulatif_nouveaux = cumulatif_nouveaux/ montant_cumulatif
    ) %>% 
    ungroup() %>%
    mutate(date_cumulatif = lubridate::ymd(mydate))
  
  cumulatif_circ <-   wrangled_file %>% 
    filter(annee_financiere == max(annee_financiere)) %>% 
    left_join(anciens_donateurs) %>%    
    mutate(ancien_donateur = as.numeric(!is.na(ancien_donateur))) %>%
    group_by(entite_politique, annee_financiere,  co_circ, nm_circ) %>% 
    summarise(montant_cumulatif = sum(montant_total), 
              donateurs = n(), 
              anciens_donateurs =  sum(ancien_donateur), 
              nouveaux_donateurs = sum(ancien_donateur ==0), 
              cumulatif_anciens = sum(montant_total * ancien_donateur),
              cumulatif_nouveaux = sum(montant_total * (ancien_donateur==0)),
              pct_nouveaux_donateurs= nouveaux_donateurs / donateurs,
              pct_cumulatif_nouveaux = cumulatif_nouveaux/ montant_cumulatif
    ) %>% 
    ungroup() %>%
    mutate(date_cumulatif = lubridate::ymd(mydate))
  
  write_csv(cumulatif, cumulatif_path)
  write_csv(cumulatif_circ, cumulatif_circ_path)
  
  
  
}


# generate daily stuff    ####

for (mydate in todo){
  print(mydate)
  
  wrangled_file = generate_wrangled_contrib_file(mydate)
  generate_cumulatif(mydate, wrangled_file)
  
  transfuges = generate_transfuge(wrangled_file)
  transfuges_od <- generate_transfuges_origin_destination(transfuges,mydate) %>%
    mutate(date = mydate)
  transfuges_od_path <- paste0("data/", mydate,"_transfuges_od.csv")
  write_csv(transfuges_od, transfuges_od_path)
  
  transfuges_summary_path <- paste0("data/", mydate,"_transfuges_summary.csv")
  transfuges_summary = generate_transfuges_summary(transfuges, mydate) %>%
    mutate(date = mydate)
  write_csv(transfuges_summary, transfuges_summary_path)
}
# generate aggregated stuff ####

if(length(todo) >= 1){
  print("generating cuulatifs quotidiens")
  list_cumulatifs <- list.files("data/", pattern= "*cumulatif.csv")
  
  cumulatifs_quotidiens <- 
    dplyr::bind_rows(purrr::map(list_cumulatifs,   ~readr::read_csv(paste0("data/",.x)))) %>%
    group_by(entite_politique) %>%
    arrange(entite_politique, date_cumulatif) %>% 
    mutate(diff_hier = if_else(lag(date_cumulatif) == date_cumulatif-1,montant_cumulatif  - lag(montant_cumulatif), NA_real_, NA_real_)) %>% 
    mutate(diff_semaine = if_else(lag(date_cumulatif, 7L) == date_cumulatif-7,montant_cumulatif  - lag(montant_cumulatif, 7L), NA_real_, NA_real_)) %>% 
    ungroup() %>%
    arrange(desc(date_cumulatif), entite_politique)
  write_csv(cumulatifs_quotidiens, "data/cumulatif_quotidiens.csv")
  
  
  list_cumulatifs_circ <- list.files("data/", pattern= "*cumulatif_circ.csv")
  
  cumulatifs_circ_quotidiens <- 
    dplyr::bind_rows(purrr::map(list_cumulatifs_circ,   ~readr::read_csv(paste0("data/",.x))))%>%
    group_by(entite_politique, nm_circ) %>%
    arrange(entite_politique,  nm_circ, date_cumulatif) %>% 
    mutate(diff_hier = if_else(lag(date_cumulatif) == date_cumulatif-1,montant_cumulatif  - lag(montant_cumulatif), NA_real_, NA_real_)) %>% 
    mutate(diff_semaine = if_else(lag(date_cumulatif, 7L) == date_cumulatif-7,montant_cumulatif  - lag(montant_cumulatif, 7L), NA_real_, NA_real_)) %>% 
    ungroup() %>%
    arrange(desc(date_cumulatif), entite_politique)
  write_csv(cumulatifs_circ_quotidiens, "data/cumulatifs_circ_quotidiens.csv")
  
  
  list_transfuges_summary <- list.files("data/", pattern= "*_transfuges_summary.csv")
  transfuges_summary_quotidiens <- 
    dplyr::bind_rows(purrr::map(list_transfuges_summary,
                                ~readr::read_csv(paste0("data/",.x))))%>%
    
    arrange(desc(date), entite_politique)
  write_csv(transfuges_summary_quotidiens, "data/transfuges_summary.csv")
}

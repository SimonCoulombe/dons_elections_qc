library(httr)
library(lubridate)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(purrr)
library(ggplot2)
ggplot2::theme_set(theme_minimal()) # this ggplot2 theme uses roboto condensed font, which works well with the font used for the whole document.
options(ggplot2.discrete.fill  = function() scale_fill_viridis_d() )
options(ggplot2.continuous.fill  = function() scale_fill_viridis_c())
options(ggplot2.discrete.colour = function() scale_color_viridis_d())
options(ggplot2.continuous.colour = function() scale_color_viridis_c())
options(scipen=999)
couleurs_parti_prov <- c(
  "Québec solidaire"= "#FF8040",
  "Parti québécois" = "#004C9D",
  "Coalition avenir Québec" = "#1E90FF",
  "Parti conservateur du Québec"=   "#7B5804" , # "#6495ED",
  "Parti libéral du Québec" = "red", # "#F08080",
  "Québec solidaire" = "#FF8040"#,
  #"autre" = "#DCDCDC"
  
)

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

list_cumulatifs <- list.files("data/", pattern= "*cumulatif.csv")
cumulatifs_quotidiens <- dplyr::bind_rows(purrr::map(list_cumulatifs,   ~readr::read_csv(paste0("data/",.x))))
write_csv(cumulatifs_quotidiens, "data/cumulatif_quotidiens.csv")


list_cumulatifs_circ <- list.files("data/", pattern= "*cumulatif_circ.csv")
cumulatifs_circ_quotidiens <- dplyr::bind_rows(purrr::map(list_cumulatifs_circ,   ~readr::read_csv(paste0("data/",.x))))
write_csv(cumulatifs_circ_quotidiens, "data/cumulatifs_circ_quotidiens.csv")

myplot <- cumulatifs_quotidiens %>% 
  ggplot(aes(x= date_cumulatif, y= montant_cumulatif, color = entite_politique)) + 
  geom_point() + 
  scale_colour_manual(values = couleurs_parti_prov)

ggsave("data/myplot.png", myplot, width= 10, height = 8, units ="in")


library(rtweet)
library(dplyr)
library(readr)
library(httr)
library(glue)
library(stringr)
bot <- rtweet_bot(
  api_key = Sys.getenv("perroquetdejeff_api_key"),
  api_secret = Sys.getenv("perroquetdejeff_api_secret_key"),
  access_token = Sys.getenv("perroquetdejeff_access_token"),
  access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
)

auth_as(bot)


z <- tempfile()
download.file("https://raw.githubusercontent.com/SimonCoulombe/dons_elections_qc/master/data/cumulatif_quotidiens.csv", z)


aa <- read_csv(z)  %>% filter(date_cumulatif == max(date_cumulatif))

# Convert data frame to a tweetable text
tweetable_text <- aa %>%
  arrange(desc(montant_cumulatif)) %>% 
  mutate(montant_cumulatif = round(montant_cumulatif)) %>% 
  mutate_all(as.character) %>%
  #mutate_all(str_wrap, width = 20) %>%
  glue_data("{entite_politique}  {montant_cumulatif} $") %>%
  paste(collapse = "\n")

# Print the tweetable_text

# Print the tweetable_text
mytweet <- paste0(max(aa$date_cumulatif), "\n", tweetable_text)
post_tweet(status =  tweetable_text)

if (http_status(response)$category == "Success") {
  cat("File downloaded and saved as", dest_file, "\n")
} else {
  cat("Error downloading the file. Please try again later.\n")
}

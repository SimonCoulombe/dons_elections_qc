
library(rtweet)
library(dplyr)
library(readr)
library(httr)
library(glue)
library(tidyr)
library(stringr)
library(ggplot2)
library(gt)
library(webshot2)
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


bot <- rtweet_bot(
  api_key = Sys.getenv("perroquetdejeff_api_key"),
  api_secret = Sys.getenv("perroquetdejeff_api_secret_key"),
  access_token = Sys.getenv("perroquetdejeff_access_token"),
  access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
)

auth_as(bot)


#mytempfile <- tempfile()
#download.file("https://raw.githubusercontent.com/SimonCoulombe/dons_elections_qc/master/data/cumulatif_quotidiens.csv", mytempfile)

cumulatif_quotidiens <- read_csv("data/cumulatif_quotidiens.csv")

date_last <- max(cumulatif_quotidiens$date_cumulatif)
latest_data <- cumulatif_quotidiens  %>% filter(date_cumulatif == date_last)

data_hier <-  cumulatif_quotidiens  %>% 
  filter(date_cumulatif != date_last) %>%
  filter(date_cumulatif == max(date_cumulatif))
date_hier <- max(data_hier$date_cumulatif)
jours_hier <- as.numeric(as.difftime(date_last- date_hier, units = "days"))

data_lastweek <-  cumulatif_quotidiens  %>% 
  filter(date_cumulatif <= date_last-7 | date_cumulatif == min(date_cumulatif)) %>%
  filter(date_cumulatif == max(date_cumulatif))
date_lastweek <- max(data_lastweek$date_cumulatif)
jours_lastweek <- as.numeric(as.difftime(date_last- date_lastweek, units = "days"))


all_data <- latest_data %>% 
  left_join(data_hier %>% select(entite_politique, cumulatif_hier = montant_cumulatif)) %>%
  left_join(data_lastweek %>% select(entite_politique, cumulatif_lastweek = montant_cumulatif)) %>%
  mutate(diff_hier = montant_cumulatif - cumulatif_hier,
         diff_lastweek = montant_cumulatif - cumulatif_lastweek) %>% 
  mutate(montant_cumulatif = round(montant_cumulatif),
         diff_hier = round(diff_hier),
         diff_lastweek = round(diff_lastweek)) 

# Convert data frame to a tweetable text
tweetable_text1 <- all_data %>%
  arrange(desc(montant_cumulatif)) %>% 
  mutate_all(as.character) %>%
  #mutate_all(str_wrap, width = 20) %>%
  glue_data("{entite_politique}  {montant_cumulatif}$ ({diff_hier} / {diff_lastweek})") %>%
  paste(collapse = "\n")

# données donateurs
tweetable_text2 <- all_data %>%
  arrange(desc(montant_cumulatif)) %>% 
  mutate_all(as.character) %>%
  #mutate_all(str_wrap, width = 20) %>%
  glue_data("{entite_politique}  {donateurs} ({nouveaux_donateurs} / {round(100*as.numeric(pct_nouveaux_donateurs))} %)") %>%
  paste(collapse = "\n")

# données transfuges  
transfuges_summary <- read_csv("data/transfuges_summary.csv")
last_date_transfuge <- max(transfuges_summary$date)
last_transfuges <- transfuges_summary %>% filter(date ==last_date_transfuge)

transfuges_od <- read_csv(paste0("data/",last_date_transfuge,"_transfuges_od.csv"))


mat <- transfuges_od %>% 
  select(-date) %>%
  mutate(entite_politique = 
           case_when(
             entite_politique=="Québec solidaire"~ "QS",
             entite_politique=="Parti québécois" ~ "PQ",
             entite_politique=="Coalition avenir Québec" ~ "CAQ",
             entite_politique=="Parti conservateur du Québec"~ "PCQ",
             entite_politique=="Parti libéral du Québec" ~ "PLQ"
           ),
         parti_origine = 
           case_when(
             parti_origine=="Québec solidaire"~ "QS",
             parti_origine=="Parti québécois" ~ "PQ",
             parti_origine=="Coalition avenir Québec" ~ "CAQ",
             parti_origine=="Parti conservateur du Québec"~ "PCQ",
             parti_origine=="Parti libéral du Québec" ~ "PLQ"
           )
  ) %>% 
  
  arrange(entite_politique, parti_origine) %>%
  pivot_wider(names_from= parti_origine, values_from =n, values_fill = 0) %>%
  select(entite_politique, order(colnames(.) ))%>% 
  mutate("total_entrants" = PCQ + PLQ+ PQ +CAQ + QS)

mat %>% 
  gt(caption= "Matrice Origine-Destination des donateurs 2022-2023",
     rowname_col = "entite_politique")  %>% 
  
      grand_summary_rows(
        columns = where(is.numeric),
        
    fns = list(
      fn= "sum", label = "Total sortants")
    ,
    fmt = ~ fmt_integer(.)
  )# %>%
  #gtsave(. , "data/matrice_od.png")




# Print the tweetable_text
mytweet1 <- paste0("Cumulatif ", date_last," (différence ",jours_hier, " jour/", jours_lastweek, "jours)"   ,
                   "\n\n", 
                   tweetable_text1)


mytweet2 <- paste0("Nombre de donateurs au ", date_last," (première fois à ce parti/% première fois)"   ,
                   "\n\n", 
                   tweetable_text2)


mytweet3 <- paste0("Nombre de transfuges 2022-2023 au ", date_last, "\n\n(un transfuge donne à un seul parti en 2022 et 2023 et a changé entre 2022 et 2023)")

# # Create a pretty ggplot
myplot <- cumulatif_quotidiens %>%
  ggplot(aes(x = date_cumulatif, y = montant_cumulatif, color = entite_politique)) +
  geom_line(linewidth = 1) +
  geom_point() + 
  scale_y_continuous(limits = c(0, max(cumulatif_quotidiens$montant_cumulatif))) +
  scale_colour_manual(values = couleurs_parti_prov) +
  labs(title = "Cumulatif annuel des dons",
       x = "Date",
       y = "Cumulatif annuel",
       color = "Parti") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )


ggsave("data/myplot.png", myplot, width= 10, height = 8, units ="in", bg = "white")

post_tweet(status =  mytweet1,
           media = c("data/myplot.png"),
           media_alt_text = c("")
)







premier_tweet_de_la_thread <-  get_timeline("covid_coulsim") %>% filter(str_detect(text, "covid")) %>% head(1)



post_tweet(
  status = paste0("RÉGIONS 2/10 covid\n" ,
                  intToUtf8(0x1F4C8), "\n",
                  "Cas par million par région\n",
                  "Hospit par million par région\n",
                  "Décès par million par région\n",
                  "Tests par million par région\n"
  ),
  
  media = c(
    "~/git/adhoc_prive/covid19_PNG/quebec_cases_by_pop.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_new_hospit_par_region.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_deces_par_region.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_tests_par_region.png"
  ),
  token = NULL,
  in_reply_to_status_id = get_timeline("covid_coulsim") %>% arrange(desc(created_at)) %>% filter(str_detect(text, "covid")) %>% pull(status_id) %>% .[1],
  destroy_id = NULL,
  retweet_id = NULL,
  auto_populate_reply_metadata = FALSE
)



post_tweet(
  status = paste0("ÂGE 3A/10 covid\n" ,
                  "Cas par million par groupe d'âge\n",
                  "Hospit par million par groupe d'âge\n",
                  "Décès par million par groupe d'âge\n",
                  "Tests par million par groupe d'âge"
  ),
  
  media = c(
    "~/git/adhoc_prive/covid19_PNG/quebec_age.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_new_hospit_par_age.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_deces_par_age.png",
    "~/git/adhoc_prive/covid19_PNG/quebec_tests_par_age.png"
  ),
  token = NULL,
  in_reply_to_status_id = get_timeline("covid_coulsim") %>% arrange(desc(created_at)) %>% filter(str_detect(text, "covid")) %>% pull(status_id) %>% .[1],
  destroy_id = NULL,
  retweet_id = NULL,
  auto_populate_reply_metadata = FALSE
)

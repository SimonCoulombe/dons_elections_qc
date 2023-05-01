
library(rtweet)
library(dplyr)
library(readr)
library(httr)
library(glue)
library(stringr)
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


bot <- rtweet_bot(
  api_key = Sys.getenv("perroquetdejeff_api_key"),
  api_secret = Sys.getenv("perroquetdejeff_api_secret_key"),
  access_token = Sys.getenv("perroquetdejeff_access_token"),
  access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
)

auth_as(bot)


mytempfile <- tempfile()
download.file("https://raw.githubusercontent.com/SimonCoulombe/dons_elections_qc/master/data/cumulatif_quotidiens.csv", mytempfile)

cumulatif_quotidiens <- read_csv(mytempfile)

date_last <- max(cumulatif_quotidiens$date_cumulatif)
data_last <- cumulatif_quotidiens  %>% filter(date_cumulatif == date_last)

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
tweetable_text <- all_data %>%
  arrange(desc(montant_cumulatif)) %>% 
  mutate_all(as.character) %>%
  #mutate_all(str_wrap, width = 20) %>%
  glue_data("{entite_politique}  {montant_cumulatif}$, ({diff_hier} / {diff_lastweek})") %>%
  paste(collapse = "\n")

# Print the tweetable_text

# Print the tweetable_text
mytweet <- paste0("Cumulatif ", date_last," (différence ",jours_hier, " jour/", jours_lastweek, "jours)"   , "\n", 
                  tweetable_text)




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

 
ggsave("data/myplot.png", myplot, width= 10, height = 8, units ="in")

post_tweet(status =  mytweet,
           media = c("data/myplot.png"),
           media_alt_text = c("")
           )


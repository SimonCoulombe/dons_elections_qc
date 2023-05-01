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
cat(tweetable_text)
post_tweet(status =  tweetable_text)

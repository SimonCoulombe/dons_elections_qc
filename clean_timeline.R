library(rtweet)

bot <- rtweet_bot(
  api_key = Sys.getenv("perroquetdejeff_api_key"),
  api_secret = Sys.getenv("perroquetdejeff_api_secret_key"),
  access_token = Sys.getenv("perroquetdejeff_access_token"),
  access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
)
auth_as(bot)


jdn_timeline <- get_timelines("DonsElectionsQC", n = 3200)

temp <- jdn_timeline %>% mutate( test = year(as_date(created_at))) %>% filter(test< 2023)
tweet_ids <- temp$id_str

for (i in seq_along(tweet_ids)) {
  message("Deleting Tweet ", i, " of ", length(tweet_ids))
  post_destroy(destroy_id = tweet_ids[i])}

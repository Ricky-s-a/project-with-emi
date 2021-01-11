
# library -----------------------------------------------------------------

Packages <- c("tidyverse", "rtweet", "stringr","DT", "RMeCab", "wordcloud", "wordcloud2")
lapply(Packages, library, character.only = TRUE)

# import data from twitter


tweets <- search_tweets("コーチング", 
                        n = 10000, 
                        type = "recent", # "popular is insufficient in number of tweets. 
                        include_rts = FALSE,
                        lang = "ja"
)



# tidy data  --------------------------------------------------------------

tweets_text <- tweets$text

tweets_text_onlyJa <- str_replace_all(tweets_text, "\\p{ASCII}", "")
tweets_text_onlyJa_shiftJis = tweets_text_onlyJa %>% iconv(from = "UTF-8", to = "CP932") %>% na.omit()


tweets_text_all = ""
for (i in 1:length((tweets_text_onlyJa_shiftJis))) {
  tweets_text_all = paste(tweets_text_all, tweets_text_onlyJa_shiftJis[i], sep = "")
}

write.table(tweets_text_all, "tweets_text_all.txt")


# RMeCab ------------------------------------------------------------------


docDF_text <- docDF("tweets_text_all.txt", type = 1, pos = c("名詞", "形容詞")) %>% 
  select(everything(), 
         word = TERM,
         freq = starts_with("tweets"))


docDF_text2 <- docDF_text %>% 
  filter(!POS2 %in% c("非自立", "サ変接続")) %>% 
  arrange(desc(freq))

datatable(docDF_text2)
# Histogram of Freq --------------------------------------------------------

docDF_text2 %>% 
  filter(freq > 500) %>% 
  ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_col(aes()) +
  coord_flip() +
  labs(title = "コーチングと共にツイートされている単語",
       subtitle = "",
       caption = "2021-01-11") +
  xlab("ツイートされた単語 （降順）") +
  ylab("度数")


quantile(docDF_text2$freq)
summary(docDF_text2$freq)
docDF_text2[docDF_text2$freq > 200, ]   

# wordcloud2 --------------------------------------------------------------

df <- docDF_text2 %>% 
  arrange(desc(freq)) %>% 
  select(word, freq)

wordcloud2(data = df, size = 3, color = "random-dark", 
           shuffle = FALSE, shape = 'circle') 
wordcloud2(data = df, size = 3, 
           minRotation = -pi/6, maxRotation = -pi/6,
           color = ifelse(df[,2] > 500, "red", "skyblue"),
           shuffle = FALSE, shape = 'circle') 

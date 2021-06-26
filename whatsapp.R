install.packages("rwhatsapp")
install.packages("tidytext")
install.packages("stopwords")
install.packages("gapminder")
install.packages("tidyr")
install.packages("textdata")
install.packages("devtools")
devtools::install_github("lchiffon/wordcloud2")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(textdata)
library(rwhatsapp)
library(dplyr)
library(gapminder)
library(ggthemes)
library(tidytext)
library(stopwords)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)



chat = rwa_read("/Users/zhangyujing/Desktop/R\ code/test/_chat.txt") %>% 
  filter(!is.na(author)) %>%
  mutate(count_character= nchar(text), 
         words= nchar(gsub('[^ ]+', '',text))+1)

plain_chat<-rwa_read("/Users/zhangyujing/Desktop/R\ code/test/_chat.txt") %>% mutate(count_character= nchar(text), words= nchar(gsub('[^ ]+', '',text))+1)

to_remove = c(stopwords(language = "en"), "media","message","deleted","https","www",
               "omitted","ref","Yeah","Yea","yeah","ahahaha","yea","ok","No","no","yes","Ok",
               "Yes","u","hahahaha","ahah","youtube","m.youtube","ahahah","hah","okkkkk",
               "whatttttt","whattttt","yay","mm","nah","d1","oooh","ohh","hhhh","hhhhh",
               "hhhhhh","haha","hhh","oh","hhhhhhh","hhhhhhhh","OKAY","okay")
  

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill="#FF6347") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")


chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y=n)) +
  geom_bar(stat = "identity", fill="#FFD700") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")


emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

top_chatters<-chat %>% group_by(author) %>% summarise(words=sum(words)) %>%
  top_n(6) %>% pull(author)
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>% filter(author %in% top_chatters ) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  ggtitle("Most often used emojis") 

chatdata=chat
chatdata %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("m.youtube", "", word)) %>%
  mutate(word = gsub("b23", "", word)) %>%
  mutate(word = gsub("d0", "", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 25, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words by author")


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()


chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)

chat_clean %>%
  group_by(author) %>%
  ungroup() %>%
  unnest_tokens(word, text)


bullring_sentiment_afinn = chat_clean %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(author) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "Group Chat Sentiment")

bullring_sentiment_afinn%>%
  ggplot(aes(author, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 2, scales = "free_y")

bing_and_nrc = bind_rows(chat_clean %>% 
                         inner_join(get_sentiments("bing")) %>%
                         mutate(method = "Bing et al."),
                         chat_clean %>% 
                         inner_join(get_sentiments("nrc") %>% 
                         filter(sentiment %in% c("positive", "negative"))) %>%
                         mutate(method = "NRC")) %>%
                         count(method, sentiment) %>%
                         spread(sentiment, n, fill = 0) %>%
                         mutate(sentiment = positive - negative)

bing_word_counts = chat_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()




df=chat %>% unnest_tokens(input = text, output = word) %>% filter(!word %in% to_remove) %>% count(word, sort = TRUE) 
set.seed(1234) # for reproducibility 
#wordcloud(words,freq,scale=c(4,.5),min.freq=3,
#         max.words=Inf,random.order=TRUE, random.color=FALSE, rot.per=.1,
#         colors="black",ordered.colors=FALSE,use.r.layout=FALSE,...)
wordcloud(words = df$word, freq = df$n, min.freq = 5,   
          max.words=50, random.order=FALSE, rot.per=0,      
          colors=brewer.pal(8, "Dark2"))

library(wordcloud2)
str(df)
newdf=df[1:300,]
#wordcloud2(data, size = 1, minSize = 0, gridSize =  0,  
#           fontFamily = NULL, fontWeight = 'normal',
#           color = 'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation = pi/4, 
#           rotateRatio = 0.4,  shape = 'cardioid', ellipticity = 0.65, widgetsize = NULL)
wordcloud2(data=newdf,size = 0.3,
           fontFamily='HanziPen SC',fontWeight='bold',
           color='random-light',backgroundColor="white", shape = 'star')

wordcloud2(data=newdf, size = 1.3, fontFamily='Snell Roundhand',fontWeight='bold',
           color='random-light',backgroundColor="white",figPath ="heartt.png")

wordcloud2(data=newdf, size = 0.7, fontFamily='Phosphate',fontWeight='normal',
           color='random-light',backgroundColor="white",minRotation = -pi/4, maxRotation = pi/4, 
            rotateRatio = 0.4,figPath ="couple.png", ellipticity = 0.65, widgetsize = NULL)

wordcloud2(data=df, size = 0.3, fontFamily='Phosphate',fontWeight='normal',
           color='random-light',backgroundColor="white",minRotation = -pi/4, maxRotation = pi/4, 
           rotateRatio = 0.4,figPath ="love.png", ellipticity = 0.65, widgetsize = NULL)

wordcloud2(data=df, size = 2, fontFamily='Snell Roundhand',fontWeight='normal',
           color='random-light',backgroundColor="white",minRotation = -pi/4, maxRotation = pi/4, 
           rotateRatio = 0.4,figPath ="position.png", ellipticity = 0.65, widgetsize = NULL)

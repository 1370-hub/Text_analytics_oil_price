library(textreadr)
library(dplyr)
oil_up<-read_document("oil_up_t.txt")
oil_up <- data_frame(line = 1:length(oil_up), text = oil_up)
View(oil_up)
oil_down<-read_document("oil_down_t.txt")
oil_down <- data_frame(line = 1:length(oil_down), text = oil_down)
View(oil_down)
library(tidyverse)
library(tidytext)
library(ggplot2)
#tokenization 
cust_stop<-data_frame(word=c('barrel','wsj.com','ydstie', 'wrote','oil','crude','year\'s','world','denbury'),
                      lexicon=c('cust','cust','cust','cust','cust','cust','cust','cust','cust'))
 oil_up_token<-oil_up%>%
  unnest_tokens(word, text)%>%
   anti_join(stop_words) %>%
   anti_join(cust_stop) 
   #count(word, sort=TRUE) 
   #top_n(30)%>%
   # ggplot(aes(word, n))+
   #  geom_col()+
   #  xlab(NULL)+
   #  coord_flip()
oil_up_token
 oil_down_token<-oil_down%>%
   unnest_tokens(word,text)%>%
   anti_join(stop_words) %>%
   anti_join(cust_stop) 
  #count(word,sort = TRUE)
  #  top_n(30) %>%
  #  ggplot(aes(word, n))+
  #  geom_col()+
  #  xlab(NULL)+
  # coord_flip()
  oil_down_token
  # heading to correlagrams and correlations 
 library(tidyr)
 frequency_up_down <- bind_rows(mutate(oil_up_token, author="Ups"),
                        mutate(oil_down_token, author= "Downs") )%>%
   mutate(word=str_extract(word, "[a-z']+")) %>%
   count(author, word) %>%
   group_by(author) %>%
   mutate(proportion = n/sum(n))%>%
   select(-n) %>%
   spread(author, proportion) %>%
   gather(author, proportion, `Downs`)
 
 # plot the correlograms:
 library(scales)
 ggplot(frequency_up_down, aes(x=proportion, y=`Ups`, 
                       color = abs(`Ups`- proportion)))+
   geom_abline(color="grey40", lty=2)+
   geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
   geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
   scale_x_log10(labels = percent_format())+
   scale_y_log10(labels= percent_format())+
   scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
   facet_wrap(~author, ncol=2)+
   theme(legend.position = "none")+
   labs(y= "Ups", x=NULL)
 
 ##########################################
 ##doing the cor.test() ################
 ##########################################
 
 cor.test(data=frequency_up_down[frequency_up_down$author == "Downs",],
          ~proportion + `Ups`)
######getting the lexicons 
 library(textdata)
 
 afinn<-get_sentiments("afinn")
 nrc<-get_sentiments("nrc")
 bing<-get_sentiments("bing")
 loughran<-get_sentiments("loughran")
 
 nrc_negative <- get_sentiments("nrc") %>%
   filter(sentiment == "negative") #what is your sentiment
 nrc_negative
 #inner joining both approaches with negative sentiments
 oil_down_token %>%
   inner_join(nrc_negative) %>%
   count(word, sort=T)
oil_up_token%>%
  inner_join(nrc_negative)%>%
  count(word, sort=TRUE)
#### we can see the articles regarding higher prices of oil has more negative sentiments

nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger") #what is your sentiment
#inner joining with anger sentiments
oil_down_token %>%
  inner_join(nrc_anger) %>%
  count(word, sort=T)
  # ggplot(aes(word, n))+
  #  geom_col()+
  #  xlab(NULL)+
  #  coord_flip()
oil_up_token%>%
  inner_join(nrc_anger)%>%
  count(word, sort=TRUE)
  # ggplot(aes(word, n))+
  #  geom_col()+
  #  xlab(NULL)+
  #  coord_flip()

#####again this is another proof showing higher conflicts take oil prices higher





###############sentiment for oil_down articles
afinn_d <- oil_down_token %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=line) %>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="afinn")

afinn_d
bing_and_nrc <- bind_rows(
  oil_down_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  oil_down_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index=line ,sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_d, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")
###### now sentiments for oil_up articles
afinn_u <- oil_up_token %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=line) %>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="afinn")
bing_and_nrc <- bind_rows(
  oil_up_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  oil_up_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index=line ,sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_u, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############


bing_counts_down <- oil_down_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_down %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
###############
bing_counts_up <- oil_up_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_up %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
###################### Worldcloud
library(RColorBrewer)
library(wordcloud)
oil_down_token %>%
  with(wordcloud(word, n,max.words = 50))
###oil_up world cloud
oil_up_token %>%
  with(wordcloud(word, n,max.words = 50))

###################################################
#################oil down clouds#########################

library(reshape2)
oil_down_token %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), fixed.asp=TRUE,title.size = 1)
library(reshape2)
oil_down_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50,
                   scale=c(0.5,0.5), fixed.asp=TRUE,title.size = 1)

###################################################
#################oil up clouds#########################

library(reshape2)
oil_up_token %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50,
                   scale=c(0.5,0.5), fixed.asp=TRUE,title.size = 1)
library(reshape2)
oil_up_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), fixed.asp=TRUE,title.size = 1)

###########just a graph to consolidate both articles

combined_oil <- bind_rows(mutate(oil_down_token, make="down"),
                           mutate(oil_up_token, make= "up"))

oil_modif <- combined_oil %>%
  count(make, word, sort=TRUE) %>%
  ungroup()

oil_modif2 <- oil_modif %>%
  group_by(make) %>%
  summarize(total=sum(n))

oil_leftjoined <- left_join(oil_modif, oil_modif2)

oil_tfidf <- oil_leftjoined %>%
  bind_tf_idf(word, make, n)

oil_tfidf # we get all the zeors because we are looking at stop words ... too common

oil_tfidf %>%
  arrange(desc(tf_idf))



####

library(ggplot2)
ggplot(oil_leftjoined, aes(n/total, fill = make))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~make, ncol=2, scales="free_y")
#################!!!tf idf######

all_oil<-bind_rows(mutate(oil_down_token,loc_oil='down'),
                         mutate(oil_up_token,loc_oil='up'),)
                         
                        
oil_new <- all_oil %>%
  count(loc_oil, word, sort=TRUE) %>%
  ungroup()%>%
  bind_tf_idf(word,loc_oil,n)

oil_new%>%
  arrange(desc(tf_idf))
oil_new%>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(loc_oil) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=loc_oil))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~loc_oil, ncol=2, scales="free")+
  coord_flip()

#########################bigrams
library(dplyr)
library(tidytext)
library(tidyr)


oil_bigrams <- all_oil %>%
  unnest_tokens(bigram, word, token = "ngrams", n=2)

oil_bigrams #We want to see the bigrams (words that appear together, "pairs")

oil_bigrams %>%
  count(bigram, sort = TRUE) 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_oil <- oil_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_oil <- bigrams_separated_oil %>%
  filter(!word1 %in% stop_words$word) %>% #every word in variable 1 that is not in stop words
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts_oil <- bigrams_filtered_oil %>%
  count(word1, word2, sort = TRUE)

negation_tokens_oil <- c("no","never","without","not","crude")
afinn_data<-get_sentiments("afinn")
negated_words_oil <- bigrams_separated_oil %>%
  filter(word1 %in% negation_tokens_oil) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

View(negated_words_oil)
#these results, the value changes signs! from negative to positive and vice versa
#################################################
#### we can visuals the negated words ###########
#we'll create a function to plot the negations###
#################################################

negated_words_plot_oil <- function(x){
  negated_words_oil %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value>0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot_oil(x="no") 
negated_words_plot_oil(x="not") 
negated_words_plot_oil(x="without") 
negated_words_plot_oil(x="crude")
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################


library(igraph)
bigram_graph_oil <- bigram_counts_oil %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph_oil


library(ggraph)
ggraph(bigram_graph_oil, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(textclean)
library(tm)
library(hunspell)
library(textstem)
library(stopwords)
library(pbapply)


categories <- c("tag/israel-palestine-conflict/", "sports/", "tag/india-pakistan-tensions/")


scrape_full_content <- function(link) {
  tryCatch({
    page <- read_html(link)
    
    content <- page %>%
      html_nodes("div.wysiwyg--all-content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    return(content)
  }, error = function(e) {
    return(NA)
  })
}


scrape_news <- function(category, num_articles = 100) {
  base_url <- "https://www.aljazeera.com/"
  articles <- data.frame()
  page <- 1
  
  while (nrow(articles) < num_articles) {
    cat("Scraping:", category, "Page:", page, "\n")
    
    full_url <- paste0(base_url, category, "?page=", page)
    webpage <- tryCatch(read_html(full_url), error = function(e) return(NULL))
    if (is.null(webpage)) break
    
    titles <- html_nodes(webpage, ".gc__title") %>% html_text(trim = TRUE)
    descriptions <- html_nodes(webpage, ".gc__excerpt") %>% html_text(trim = TRUE)
    dates <- html_nodes(webpage, ".gc__meta") %>% html_text(trim = TRUE)
    links <- html_nodes(webpage, ".gc__title a") %>% html_attr("href")
    links <- paste0("https://www.aljazeera.com", links)
    
    
    min_len <- min(length(titles), length(descriptions), length(dates), length(links))
    if (min_len == 0) break
    
    temp_data <- data.frame(
      title = titles[1:min_len],
      description = descriptions[1:min_len],
      date = dates[1:min_len],
      link = links[1:min_len],
      category = rep(category, min_len),
      stringsAsFactors = FALSE
    )
    
    articles <- bind_rows(articles, temp_data)
    page <- page + 1
    Sys.sleep(1)
  }
  
  return(head(articles, num_articles))
}


news_data <- map_df(categories, ~ scrape_news(.x, num_articles = 20)) 


news_data$content <- sapply(news_data$link, scrape_full_content)


news_data <- news_data[!is.na(news_data$content) & news_data$content != "", ]

write.csv(news_data, "aljazeera_news_scrapped.csv", row.names = FALSE, fileEncoding = "UTF-8")

getwd()
head(news_data, 3)




preprocess_article_simple <- function(text) {
  text <- tolower(text)
  text <- replace_contraction(text)
  text <- str_replace_all(text, "<.*?>", " ")
  text <- str_replace_all(text, "[^[:alnum:][:space:]]", " ")
  text <- str_replace_all(text, "[0-9]", " ")
  text <- str_squish(text)
  
  tokens <- unlist(str_split(text, "\\s+"))
  tokens <- iconv(tokens, to = "ASCII//TRANSLIT")
  tokens <- tokens[!(tokens %in% stopwords("en"))]
  tokens <- lemmatize_words(tokens)
  tokens <- tokens[nchar(tokens) > 1]
  paste(tokens, collapse = " ")
}


cat("Preprocessing first article with output:\n")
preprocess_article_simple(news_data$content[1])


news_data$processed <- sapply(news_data$content, preprocess_article_simple)


write.csv(news_data, "aljazeera_processed_final_file.csv", row.names = FALSE)
cat("\n preprocessed data saved to: aljazeera_processed_final_file.csv\n")



libs <- c("readr", "tm", "topicmodels", "tidyverse", "tidytext", "reshape2", "wordcloud")
for (lib in libs) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}


processed_news <- read_csv("aljazeera_processed_final_file.csv")
corpus_text <- processed_news$processed  


top_word_count <- 7     
topic_number <- 7      


corpus <- Corpus(VectorSource(corpus_text))
doc_term_matrix <- DocumentTermMatrix(corpus)


doc_term_matrix <- removeSparseTerms(doc_term_matrix, 0.95)
doc_term_matrix_dtm<- as.matrix(doc_term_matrix)


lda_model <- LDA(doc_term_matrix, k = topic_number, control = list(seed = 123))


unwanted_terms <- c("say", "jazeera","bbc","get")


topics <- tidy(lda_model, matrix = "beta") %>%
  filter(!term %in% unwanted_terms) %>%  
  group_by(topic) %>%
  slice_max(beta, n = top_word_count, with_ties = FALSE) %>%
  ungroup()

print(topics)

topic_proportions <- tidy(lda_model, matrix = "gamma") %>%
  mutate(document = as.numeric(document))




topic_labels <- c(
  "1" = "league Sports",
  "2" = "India Politics",
  "3" = "Middle East",
  "4" = "kashmir issue",
  "5" = "Military Conflict ",
  "6" = "India Pakistan conflict",
  "7" = "athletic sport"
)


topics <- topics %>%
  mutate(topic_name = topic_labels[as.character(topic)])


p_beta <- topics %>%
  ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free") +
  coord_flip() +
  labs(
    title = "Top Terms for Each Topic",
    x = "Terms",
    y = "Beta"
  )





p_gamma <- topic_proportions %>%
  ggplot(aes(x = factor(topic), y = gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales = "free") +
  labs(
    title = "Topic Proportions for Each Document",
    x = "Topics",
    y = "Proportion (Gamma)"
  )


print(p_beta)
print(p_gamma)


ggsave("top_terms.png", plot = p_beta, width = 12, height = 8, dpi = 300)
ggsave("topic_proportions.png", plot = p_gamma, width = 20, height = 10, dpi = 300)


unique_topics <- unique(topics$topic)

par(mfrow = c(1, 1), mar = c(1, 1, 2, 1))  # Adjust plot layout and margins

for (t in unique_topics) {
  cat("Generating wordcloud for topic:", t, "\n")
  
  topic_terms <- topics %>% filter(topic == t)
  
  wordcloud(words = topic_terms$term,
            freq = topic_terms$beta,
            scale = c(4, 0.5),
            min.freq = min(topic_terms$beta),
            max.words = 100,
            colors = brewer.pal(8, "Set3"),
            random.order = FALSE)
  
  
  topic_name <- topic_labels[as.character(t)]
  title(main = paste("Topic", t, "-", topic_name), cex.main = 1.2)
}

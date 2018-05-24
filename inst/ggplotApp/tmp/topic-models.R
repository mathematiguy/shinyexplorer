# https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html

library(topicmodels)

vocab <- all_claims %>%
  group_by(injury_type) %>%
  summarise(vocab = list(build_vocab(AccidentDesc))) %>%
  mutate(vocab = map2(vocab, injury_type, function(v, o) mutate(v, category = o))) %>%
  .$vocab %>%
  bind_rows() %>%
  as_tibble()

vocab_dtm <- vocab %>%
  mutate(category = as.character(category)) %>%
  cast_dtm(category, term, term_count)

vocab_topics <- LDA(vocab_dtm, k = 10)

vocab_topics_td <- tidy(vocab_topics)

vocab_topics_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic, scales = "free") +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

vocab_topics_gamma <- tidy(vocab_topics, matrix = "gamma")

vocab_topics_gamma
























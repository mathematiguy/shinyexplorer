
plot_data <- build_tfidf_data(all_claims, "AccidentDesc", "occup_L1", "tf_idf", num_terms = 20)

plot_data %>%
  mutate(df = tanh(1 / idf),
         col = log10(term_count) + 1) %>%
  ggplot(aes(x = 1, y = 1, size = df, label = term, colour = category, alpha = col)) +
  geom_text_repel(segment.size = 0, force = 10) +
  guides(colour = FALSE, alpha = FALSE) +
  scale_size(range = c(2, 15), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~category, scales = 'free') +
  labs(x = '', y = '') +
  theme_minimal()



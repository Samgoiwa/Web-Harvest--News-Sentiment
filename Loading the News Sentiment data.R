# Install required packages (run once)
install.packages(c("tidyverse", "syuzhet"))

# Load libraries
library(tidyverse)
library(syuzhet)

# Load your data (adjust path if needed)
news <- read_csv("kenya_open_news.csv")

# Clean and preprocess text
news_clean <- news %>%
  mutate(
    Title_clean = str_squish(str_replace_all(Title, "[^[:alnum:] ]", "")),
    Title_lower = str_to_lower(Title_clean)
  )

# Define keywords
keywords <- c("finance bill", "gen z", "protest", "election")

# Create indicator columns for each keyword
for (k in keywords) {
  colname <- str_replace_all(k, " ", "_")
  news_clean[[colname]] <- str_detect(news_clean$Title_lower, fixed(k))
}

# Sentiment analysis using syuzhet
news_clean <- news_clean %>%
  mutate(Sentiment = get_sentiment(Title, method = "syuzhet"))

# --- PLOT 1: Keyword Trend Chart ---
trend_summary <- news_clean %>%
  pivot_longer(cols = all_of(str_replace_all(keywords, " ", "_")),
               names_to = "Keyword", values_to = "Detected") %>%
  filter(Detected) %>%
  count(Keyword) %>%
  mutate(Keyword = str_replace_all(Keyword, "_", " ") %>% str_to_title())

# Plot keyword mentions
ggplot(trend_summary, aes(x = reorder(Keyword, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Article Mentions by Keyword",
    x = "Keyword",
    y = "Number of Mentions"
  ) +
  theme_minimal()

# --- PLOT 2: Sentiment by Source ---
news_clean %>%
  group_by(Source) %>%
  summarise(Average_Sentiment = mean(Sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Source, Average_Sentiment), y = Average_Sentiment)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Average Sentiment Score by News Source",
    x = "Source",
    y = "Average Sentiment Score"
  ) +
  theme_minimal()


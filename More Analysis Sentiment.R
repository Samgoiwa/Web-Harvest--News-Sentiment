# Required packages
install.packages(c("tidyverse", "syuzhet", "lubridate"))
library(tidyverse)
library(syuzhet)
library(lubridate)

# Load data
news <- read_csv("kenya_open_news.csv")

# Clean and preprocess
news_clean <- news %>%
  mutate(
    Title_clean = str_squish(str_replace_all(Title, "[^[:alnum:] ]", "")),
    Title_lower = str_to_lower(Title_clean),
    PubDate = as.Date(PubDate),
    Sentiment = get_sentiment(Title, method = "syuzhet")
  )

# Add sentiment category
news_clean <- news_clean %>%
  mutate(
    Sentiment_Category = case_when(
      Sentiment > 0.1 ~ "Positive",
      Sentiment < -0.1 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

# Define keywords
keywords <- c("finance bill", "gen z", "protest", "election")
for (k in keywords) {
  colname <- str_replace_all(k, " ", "_")
  news_clean[[colname]] <- str_detect(news_clean$Title_lower, fixed(k))
}

# -----------------------------
# 1. Keyword Trend Over Time
# -----------------------------
keyword_long <- news_clean %>%
  pivot_longer(cols = all_of(str_replace_all(keywords, " ", "_")),
               names_to = "Keyword", values_to = "Detected") %>%
  filter(Detected, !is.na(PubDate)) %>%
  mutate(Keyword = str_replace_all(Keyword, "_", " ") %>% str_to_title()) %>%
  count(PubDate, Keyword)

ggplot(keyword_long, aes(x = PubDate, y = n, color = Keyword)) +
  geom_line(size = 1.2) +
  labs(title = "Keyword Mentions Over Time", x = "Date", y = "Mentions") +
  theme_minimal()

# -----------------------------
# 2. Sentiment Category Distribution
# -----------------------------
ggplot(news_clean, aes(x = Sentiment_Category, fill = Sentiment_Category)) +
  geom_bar() +
  labs(title = "Distribution of Headline Sentiment", x = "Sentiment", y = "Count") +
  theme_minimal()

# -----------------------------
# 3. Sentiment Over Time
# -----------------------------
sentiment_trend <- news_clean %>%
  group_by(PubDate) %>%
  summarise(Average_Sentiment = mean(Sentiment, na.rm = TRUE)) %>%
  filter(!is.na(PubDate))

ggplot(sentiment_trend, aes(x = PubDate, y = Average_Sentiment)) +
  geom_line(color = "darkred", size = 1) +
  geom_smooth(se = FALSE, color = "black") +
  labs(title = "Sentiment Trend Over Time", x = "Date", y = "Average Sentiment") +
  theme_minimal()

# -----------------------------
# 4. Top Positive & Negative Headlines
# -----------------------------
top_positive <- news_clean %>%
  arrange(desc(Sentiment)) %>%
  select(Source, Title, Sentiment) %>%
  slice(1:5)

top_negative <- news_clean %>%
  arrange(Sentiment) %>%
  select(Source, Title, Sentiment) %>%
  slice(1:5)

print("Top 5 Positive Headlines:")
print(top_positive)
view(top_positive)

print("Top 5 Negative Headlines:")
print(top_negative)
view(top_negative)

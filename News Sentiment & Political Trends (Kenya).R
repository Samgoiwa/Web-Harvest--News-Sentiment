# Required packages
install.packages(c("xml2", "httr", "tidyverse"))
library(httr)
library(xml2)
library(tidyverse)

# Parser with User-Agent header
parse_rss <- function(rss_url, source) {
  tryCatch({
    resp <- GET(rss_url, add_headers(`User-Agent` = "Mozilla/5.0"))
    if (http_error(resp)) stop("Feed error")
    feed <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
    items <- xml_find_all(feed, "//item")
    tibble(
      Source = source,
      Title = xml_text(xml_find_first(items, "title")),
      Link = xml_text(xml_find_first(items, "link")),
      PubDate = xml_text(xml_find_first(items, "pubDate")),
      Scraped = Sys.time()
    )
  }, error = function(e) {
    message("⚠️ Failed loading RSS for ", source)
    tibble()
  })
}

# Updated list of feeds
feeds <- list(
  list("https://www.kenyans.co.ke/feeds/news?_wr=0", "Kenyans.co.ke"),
  list("https://allafrica.com/tools/headlines/rdf/kenya/headlines.rdf", "AllAfrica Kenya")
  # Nation & K24 RSS removed due to 403 blocks
)

# Combine all valid feeds
news_list <- map_dfr(feeds, ~ parse_rss(.x[[1]], .x[[2]]))

# Save or preview
print(news_list)
view(news_list)
write_csv(news_list, "kenya_open_news.csv")

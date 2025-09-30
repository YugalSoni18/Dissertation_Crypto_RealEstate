DISSERTATION CODE
Thematic Analysis:
# ===============================
# Thematic Analysis (6 Themes + 6 Graphs)
# ===============================

library(tidyverse)
library(stringr)
library(readr)

# 1. Load and clean data
df <- read_csv("/Users/yugalsoni/Desktop/BADS Dissertation/Interviews/Public_Perception.csv") %>%
  mutate(across(everything(), ~str_trim(.))) %>%      # Trim all columns
  filter(!is.na(Response_Text), Response_Text != "") %>%
  mutate(Response_Text = str_to_lower(Response_Text))

# 2. Define Theme Question IDs
theme_questions <- list(
  Benefits = c("Q4", "Q20"),
  Risks = c("Q5", "Q21"),
  Trust = c("Q6", "Q7", "Q19"),
  Regulations = c("Q11", "Q22"),
  Future = c("Q12", "Q13", "Q23"),
  Suggestions = c("Q12", "Q24")
)

# 3. Define tagging function
tag_theme <- function(text, theme) {
  if (theme == "Benefits") {
    if (str_detect(text, "fast|speed|instant|quick")) return("Speed & Efficiency")
    if (str_detect(text, "transparency|transparent|blockchain|ledger|immutable")) return("Transparency")
    if (str_detect(text, "low cost|cheap|reduce fee|no middleman|lower cost|intermediary")) return("Cost Reduction")
    if (str_detect(text, "cross-border|international|global|foreign|overseas")) return("Cross-border Payments")
    if (str_detect(text, "smart contract|automate|token|fractional|ownership")) return("Smart Contracts & Tokenisation")
  }
  if (theme == "Risks") {
    if (str_detect(text, "volatile|instability|price swing|fluctuate")) return("Volatility")
    if (str_detect(text, "fraud|scam|hacked|security|loss|theft")) return("Security & Fraud Risk")
    if (str_detect(text, "legal|regulation|government|compliance|ban")) return("Regulatory Uncertainty")
    if (str_detect(text, "lack of awareness|don't understand|not familiar")) return("Lack of Knowledge")
  }
  if (theme == "Trust") {
    if (str_detect(text, "trust|confidence|feel safe|reliable")) return("Positive Trust")
    if (str_detect(text, "don't trust|risky|uncomfortable|fear")) return("Lack of Trust")
    if (str_detect(text, "depends|maybe|if|conditional")) return("Conditional Trust")
  }
  if (theme == "Regulations") {
    if (str_detect(text, "regulation|law|government|clarity|legal system")) return("Need for Regulation")
    if (str_detect(text, "support|policy|favor|encourage")) return("Support for Regulation")
    if (str_detect(text, "illegal|ban|penalty|risk")) return("Fear of Legal Risk")
  }
  if (theme == "Future") {
    if (str_detect(text, "yes|will happen|likely|adopt|mainstream")) return("Positive Outlook")
    if (str_detect(text, "no|never|not possible|unlikely|doubt")) return("Negative Outlook")
    if (str_detect(text, "depends|if|maybe|not sure|conditional")) return("Conditional Outlook")
  }
  if (theme == "Suggestions") {
    if (str_detect(text, "awareness|education|understand|knowledge")) return("Public Awareness")
    if (str_detect(text, "law|regulation|clarity|government|legal")) return("Need for Legal Framework")
    if (str_detect(text, "tools|platform|wallet|app|secure")) return("Tech Improvement")
    if (str_detect(text, "training|realtor|agent|broker|onboard")) return("Industry Training")
  }
  return("Other")
}

# 4. Analysis Loop with 6 Graphs
results_list <- list()
quotes_list <- list()

for (theme in names(theme_questions)) {
  cat("Processing theme:", theme, "\n")
  
  # Filter and tag
  theme_df <- df %>%
    filter(Questions_Unique_ID %in% theme_questions[[theme]]) %>%
    mutate(Theme_Tag = map_chr(Response_Text, ~tag_theme(.x, theme)))
  
  # Summary
  summary <- theme_df %>%
    group_by(Group, Region, Theme_Tag) %>%
    summarise(Count = n(), .groups = "drop")
  
  results_list[[theme]] <- summary
  
  # Sample quotes
  quotes <- theme_df %>%
    group_by(Group, Region, Theme_Tag) %>%
    slice_head(n = 1) %>%
    select(Group, Region, Theme_Tag, Response_Text)
  
  quotes_list[[theme]] <- quotes
  
  # Plot - 1 per theme
  p <- ggplot(summary, aes(x = Theme_Tag, y = Count, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Region) +
    labs(title = paste("Theme:", theme),
         x = paste(theme, "Themes"), y = "Number of Mentions") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  print(p)
}

# 5. View one quote sample
View(quotes_list$Trust)






















Sentiment Analysis:
# ===========================================================
# SENTIMENT ANALYSIS (General + By Theme)
# ===========================================================

# 1. Load required libraries
library(tidyverse)
library(tidytext)
library(readr)
library(stringr)

# 2. Load and clean dataset
df <- read_csv("/Users/yugalsoni/Desktop/BADS Dissertation/Interviews/Public_Perception.csv") %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!is.na(Response_Text), Response_Text != "") %>%
  mutate(Response_Text = str_to_lower(Response_Text))

# 3. Load sentiment lexicons
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")

# 4. TOKENIZE entire dataset
tokens <- df %>%
  unnest_tokens(word, Response_Text)

# ===========================================================
# 🔹 PART A – GENERAL SENTIMENT 
# ===========================================================

# A1. BING sentiment (positive/negative counts per person)
bing_sentiment <- tokens %>%
  inner_join(bing, by = "word") %>%
  count(Interviewee_ID, Group, Region, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(Net_Sentiment = positive - negative)

# A2. AFINN sentiment (numeric score per person)
afinn_sentiment <- tokens %>%
  inner_join(afinn, by = "word") %>%
  group_by(Interviewee_ID, Group, Region) %>%
  summarise(Afinn_Score = sum(value), .groups = "drop")

# A3. Merge general sentiment scores
overall_sentiment <- left_join(bing_sentiment, afinn_sentiment,
                               by = c("Interviewee_ID", "Group", "Region"))
print(overall_sentiment)

# A4. Plot: Net Sentiment (BING) by Group
ggplot(overall_sentiment, aes(x = Group, y = Net_Sentiment, fill = Group)) +
  geom_boxplot() +
  labs(title = "Net Sentiment (BING) by Group",
       x = NULL, y = "Net Sentiment") +
  theme_minimal()

# A5. Plot: AFINN Score by Region
ggplot(overall_sentiment, aes(x = Region, y = Afinn_Score, fill = Group)) +
  geom_boxplot() +
  labs(title = "AFINN Sentiment Score by Region",
       x = "Region", y = "AFINN Score") +
  theme_minimal()

# ===========================================================
# View Most Common Positive/Negative Words
# ===========================================================

top_words <- tokens %>%
  inner_join(bing, by = "word") %>%
  count(word, sentiment, sort = TRUE)


# ================================================
# Cleaned & Grouped Sentiment Word Frequency Chart
# ================================================

# 1. Define a custom dictionary of word groupings
grouped_words <- tribble(
  ~original,     ~standard,
  "risky",       "risk",
  "risks",       "risk",
  "risk",        "risk",
  "volatility",  "volatility",
  "scams",       "scam",
  "scam",        "scam",
  "lost",        "lose",
  "lose",        "lose",
  "fake",        "fake",
  "fraud",       "fraud",
  "wrong",       "wrong",
  "delay",       "delay",
  "delays",      "delay",
  "lack",        "lack",
  "like",        "like",
  "trust",       "trust",
  "clear",       "clear",
  "clarity",     "clear",
  "smart",       "smart",
  "right",       "right",
  "proper",      "proper",
  "transparent", "transparent",
  "luxury",      "luxury",
  "well",        "well"
)

# 2. Join and group original words
cleaned_words <- tokens %>%
  inner_join(bing, by = "word") %>%
  left_join(grouped_words, by = c("word" = "original")) %>%
  mutate(final_word = ifelse(is.na(standard), word, standard)) %>%
  count(final_word, sentiment, sort = TRUE)

# 3. Plot grouped sentiment words
cleaned_words %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(final_word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Grouped Sentiment Words",
       x = "Word", y = "Frequency") +
  theme_minimal()


# A6. Export general sentiment
write_csv(overall_sentiment, "Overall_Sentiment_Summary.csv")

# ===========================================================
# 🔹 PART B – SENTIMENT ANALYSIS BY THEME (6 Themes)
# ===========================================================

# B1. Define theme-related question IDs
theme_questions <- list(
  Regulations = c("Q11", "Q22"),
  Risks = c("Q5", "Q21"),
  Trust = c("Q6", "Q7", "Q19"),
  Benefits = c("Q4", "Q20"),
  Future = c("Q12", "Q13", "Q23"),
  Suggestions = c("Q12", "Q24")
)

# B2. Loop through each theme
sentiment_summary <- list()
top_quotes <- list()

for (theme in names(theme_questions)) {
  cat("Processing Theme:", theme, "\n")
  
  theme_df <- df %>%
    filter(Questions_Unique_ID %in% theme_questions[[theme]])
  
  theme_tokens <- theme_df %>%
    unnest_tokens(word, Response_Text)
  
  # AFINN scoring
  theme_afinn <- theme_tokens %>%
    inner_join(afinn, by = "word") %>%
    group_by(Interviewee_ID, Group, Region) %>%
    summarise(Afinn_Score = sum(value), .groups = "drop")
  
  # BING scoring
  theme_bing <- theme_tokens %>%
    inner_join(bing, by = "word") %>%
    count(Interviewee_ID, Group, Region, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(Net_Sentiment = positive - negative)
  
  # Combine scores
  theme_scores <- left_join(theme_afinn, theme_bing, by = c("Interviewee_ID", "Group", "Region"))
  theme_scores$Theme <- theme
  sentiment_summary[[theme]] <- theme_scores
  
  # Plot AFINN by Group
  p1 <- ggplot(theme_scores, aes(x = Group, y = Afinn_Score, fill = Group)) +
    geom_boxplot() +
    labs(title = paste("AFINN Score –", theme), y = "AFINN Score", x = NULL) +
    theme_minimal()
  print(p1)
  
  # Plot BING by Region
  p2 <- ggplot(theme_scores, aes(x = Region, y = Net_Sentiment, fill = Group)) +
    geom_boxplot() +
    labs(title = paste("Net Sentiment (BING) –", theme), y = "Net Sentiment", x = "Region") +
    theme_minimal()
  print(p2)
  
  # Extract top quotes (optional)
  word_scores <- theme_tokens %>%
    inner_join(afinn, by = "word") %>%
    group_by(Interviewee_ID) %>%
    summarise(Total_Score = sum(value), .groups = "drop") %>%
    left_join(theme_df, by = "Interviewee_ID") %>%
    arrange(Total_Score)
  
  top_quotes[[theme]] <- list(
    Most_Negative = head(word_scores, 2),
    Most_Positive = tail(word_scores, 2)
  )
}

# B3. Combine and export theme-based scores
full_theme_sentiment <- bind_rows(sentiment_summary)
write_csv(full_theme_sentiment, "Sentiment_Summary_By_Theme.csv")

# B4. View most extreme quotes for one theme
View(top_quotes$Regulations$Most_Negative)
View(top_quotes$Risks$Most_Negative)
View(top_quotes$Trust$Most_Negative)
View(top_quotes$Benefits$Most_Negative)
View(top_quotes$Future$Most_Negative)
View(top_quotes$Suggestions$Most_Negative)
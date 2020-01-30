library(tidyverse)
library(lubridate)
library(tm) # To make a word cloud
library(wordcloud) # Idem

# ===================================================================
# Importing and cleaning data
# ===================================================================
data <- fromJSON(file = "result.json")

data.chat <- list.filter(data[["chats"]][["list"]], # Get lists of chats
                      .[["name"]] == "Laetitia") # Find chat with name == "Friend"
data.messages <- data.chat[[1]][["messages"]] # Get messages from that chat

getText <- function(x) { # Only retrieve messages
  if (typeof(x[["text"]]) != "character") return("")
  return(as.character(x[["text"]]))
}
  
json <- data.messages %>% {
  tibble( # Get the three pieces of info we want.
    date = map_chr(., "date"),
    from = map(., "from"),
    text = map_chr(., getText)
  ) %>% 
  filter(!(text == ""), !is.na(from))# Filter out some data without info
}

json <- json %>% 
  mutate(week = floor_date(ymd_hms(date),unit = "weeks",
                           week_start = getOption("lubridate.week.start", 1)),
         hour = hour(ymd_hms(date)),
         wday = wday(ymd_hms(date), label = TRUE, abbr = FALSE))

json$from = factor(json$from, levels = unique(json$from))
json$from = fct_recode(json$from, "My Friend" = "Laetitia Surname", "Julie" = "Julie Ducasse") # TO BE EDITED!!

# ===================================================================
# Some figures
# ===================================================================

# Number of messages ================================================
nrow(json)

# Number of messages per person =====================================

json_talkative <- count(json, from)

pdf("telegram-talkative.pdf", width=6, height=1.3)
ggplot(json_talkative, aes(x = "", y = n, fill = from)) +
  geom_bar(stat="identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values  = c("#603F83FF", "#C7D3D4FF"), 
                    name = "Messages sent by ...", 
                    guide=guide_legend(reverse=T)) +
  theme( panel.grid.major = element_blank(),
         axis.ticks.y = element_blank(), 
         panel.background = element_blank(),
         legend.position = "top",
         axis.title = element_blank())
dev.off()

# Number of messages per week =======================================
week.count <- count(json, week, from)

pdf("telegram-weeks.pdf", width=12.5, height=4)
ggplot(data = week.count, aes(x = as.Date(week), y = n, fill = from)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "week", date_labels = "%b %d", 
               expand = c(0.05,0)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "top", 
        axis.title= element_blank()) +
  scale_fill_manual(values  = c("#B1624EFF", "#5CC8D7FF"), name = "Number of messages sent by ...") +
  # labs(x = "", y = "") +
  expand_limits(y = 0)
dev.off()

# What time of the day / what day ===================================
json_time <- json %>% 
  group_by(week, wday, hour) %>% 
  summarise(n = n()) %>% 
  group_by(wday, hour) %>% 
  summarise(mean = mean(n)) %>% 
  complete(hour = seq(0,24,1), fill = list(mean = 0))
View(json_time)

json_time$hour <- factor(json_time$hour, levels = unique(json_time$hour))
json_time$hour <- fct_relevel(json_time$hour, c("0", "1", "2", "3", "4", "5"), after = Inf)
json_time$wday <- fct_relevel(json_time$wday, c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday", "Sunday"))

#' or colour = black outside of aes for contour line
ggplot(json_time, aes(x = wday, y = mean, fill = as.numeric(hour), colour = hour)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar() +
  scale_y_continuous(breaks = seq(-10,30, 5), limits = c(-10, max(json_time$mean))) +
  scale_fill_viridis_c(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  ) 

ggsave("circular.pdf", width = 6, height = 8.5, units = "in", dpi = "print")


# Who is the most selfish  ==========================================
# ===================================================================
words <- c(" i ", "you ", "we ",
           "yes|yeah", "no |nope",
           "sorry",
           "ha ha|haha|ah ah|ahah|he he",
           "thanks|thank you",
           "coffee")

labels <- c("I", "You", "We", "Yes", "No", "Sorry", ":)",
            "Thank you", "Coffee")

subset.friend <- tolower(as.character(filter(json, from == "My Friend")$text))
subset.me <- tolower(as.character(filter(json, from == "Julie")$text))

colnames <- c("Word", "From", "Count")
df.words <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), colnames)

for (i in seq(1:length(words))) {
  fromFriend <- sum(str_count(subset.friend, words[i]), na.rm = TRUE)
  fromMe <- sum(str_count(subset.me, words[i]), na.rm = TRUE)
  rowFriend <- setNames(data.frame(labels[i],"My Friend",fromFriend), colnames)
  rowMe <- setNames(data.frame(labels[i],"Julie",fromMe), colnames)
  df.words <- rbind(df.words, rowFriend, rowMe)
}
View(df.words)

df.words.pct <- df.words %>% 
  group_by(Word) %>% 
  mutate(percentage = round(Count/sum(Count)*100))
View(df.words.pct)

pdf("wordsFreq.pdf", width=6, height=8.5)
ggplot(df.words.pct, aes(x = Word, y = percentage, fill = From)) +
  geom_bar(stat="identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values  = c("orange", "#954535"), name = "Words written by ...") +
  scale_x_discrete(limits = rev(levels(df.words.pct$Word))) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0.01,0))+
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = .3),
        panel.background = element_blank(),
        plot.margin = unit(c(0,1,0,0), "cm")) +
  guides(fill = guide_legend(reverse = TRUE))
dev.off()

# Wordcloud =========================================================
all <- paste(as.character(json$text))
all <- str_replace_all(all,"[^[:graph:]]", " ")
docs <- Corpus(VectorSource(all))

# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# docs <- tm_map(docs, toSpace, "<U*>")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("listtype")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs, control=list(wordLengths=c(6,Inf)))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
pdf("wordcloudBoth.pdf", width=6, height=8.5)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

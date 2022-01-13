#Title: 'Is sustainability advertising just a public relations stunt?' code ----
#Author: Thomas F Johnson
#Contact: Thomas.Frederick.Johnson(at)outlook.com
#Date: October 2020

#Load all packages ----
library(piecewiseSEM)
library(lme4)
library(heavy)
library(reshape)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(lattice)
library(car)
library(merTools)
library(ggeffects)
library(BatchGetSymbols)
library(tm)
library(SnowballC)
library(ggwordcloud)


setwd("/Volumes/GoogleDrive/My Drive/research/social_media/Twitter")

#Load any additional functions ----
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

#Set colour palettes ---- 
miss_sector_pal = c(
  "#3A1FC4", "#A214FF", "#BF6BF7", "#E0B7FC", "#C3B7FC", "#D3EBFA",  "#A9D6F5", "#5AB7F5", "#2998E2", "#1273B3", "#064670"
)


#Load all data ----
uk_accounts = read.csv("Corporate/UK_accounts.csv", stringsAsFactors = F)
climate_leadership = read.csv("Corporate/UK.csv", stringsAsFactors = F)
tweets = readRDS("Corporate/download.rds")
stocks = read.csv("Corporate/Stock_link.csv", stringsAsFactors = F)

#Tidy uk_accounts data ----
uk_accounts = read.csv("Corporate/UK_accounts.csv", stringsAsFactors = F)
uk_accounts$flag = ifelse(uk_accounts$Warning == "" & uk_accounts$Comment == "", 0, 1)
uk_accounts = uk_accounts[,c("Company.Name", "Account", "flag")]
colnames(uk_accounts)[2] = "screen_name"

#Tidy climate_leadership data ----
climate_leadership = climate_leadership[,c(2,5,6,14:22)]
colnames(climate_leadership)[2:3] = c("Sector", "X2019")
climate_leadership = melt(climate_leadership, id.vars=c("Sector", "Company.Name"))
colnames(climate_leadership)[3:4] = c("year", "score")
climate_leadership$score = ifelse(
  climate_leadership$score == "Not Available" |
    climate_leadership$score == "Not Requested" |
    climate_leadership$score == "Not Scored" |
    climate_leadership$score == "F" |
    climate_leadership$score == "",
  NA,
  as.character(climate_leadership$score))
climate_leadership$score = ifelse(climate_leadership$score == "E", NA, climate_leadership$score)
climate_leadership$score = factor(climate_leadership$score, ordered = T, levels = c("E", "D-", "D", "C-", "C", "B-", "B", "A-", "A"))

climate_leadership$year = as.numeric(gsub("X","",climate_leadership$year))
ggplot(climate_leadership[which(!is.na(climate_leadership$score)),]) +
  geom_bar(aes(x = score)) +
  facet_grid(year~.) +
  theme_classic() +
  labs(x = "", y = "Count (N)")

ggplot(climate_leadership[which(!is.na(climate_leadership$score)),]) +
  geom_bar(aes(x = score)) +
  facet_wrap(Sector~., scale = "free_y") +
  theme_classic() +
  labs(x = "", y = "Count (N)")






#Correct climate_leadership sectors ----
climate_leadership$Sector = ifelse(
  climate_leadership$Sector == "N/A",
  "General",
  climate_leadership$Sector)
climate_leadership$Sector = ifelse(
  climate_leadership$Sector == "Transport OEMS",
  "Transport services",
  climate_leadership$Sector)
climate_leadership$Sector = ifelse(
  climate_leadership$Sector == "Cement" |
    climate_leadership$Sector == "Steel",
  "Construction",
  climate_leadership$Sector)
climate_leadership$score = as.numeric(climate_leadership$score)
sectors = climate_leadership[,c(1,2)]
sectors = sectors[!duplicated(sectors),]




#Tidy tweets data ----
tweets = tweets[,c(2,3,4,5,11,12,13,14,78,81,83,91)]
tweets = tweets[!duplicated(tweets),]
tweets = subset(tweets, created_at < as.POSIXct("2020-01-01", tz = "UTC"))
tweets = left_join(tweets, uk_accounts)
tweets = subset(tweets, flag == 0)
length(unique(tweets$Company.Name))
tweets = subset(tweets, Company.Name != "Schroder Real Estate Investment Trust ltd")
tweets = subset(tweets, is_quote == FALSE & is_retweet == FALSE)
tweets$year = as.numeric(format(tweets$created_at,'%Y'))
tweets$enviro = ifelse(grepl("climate|temperature|global warming|sustainable|sustainability|nature|renewable|carbon neutral|flood|drought|desertification|ocean acidification|coral bleaching|wildfire", tweets$text), 1, 0)
tweets$year_from_2010 = (as.numeric(as.Date(tweets$created_at)) - as.numeric(as.Date("2010-01-01")))/365.25
tweets = left_join(tweets, sectors)


#Summarise climate_leadership missing data ---- 
climate_leadership_trim = climate_leadership %>%
  group_by(Company.Name) %>%
  filter(sum(is.na(score)) < n()-1) #All companies with less than 9/10 missing values

climate_leadership_nodata = climate_leadership %>%
  group_by(Company.Name) %>%
  filter(sum(is.na(score)) >= n()-1) #All companies with more than 8/10 missing values

sector_climate_leadership_n = climate_leadership_trim %>%
  group_by(Sector) %>%
  dplyr::summarise(available = n()) #Companies in each sector with atleast two scores

sector_climate_leadership_n_nodata = climate_leadership_nodata %>%
  group_by(Sector) %>%
  dplyr::summarise(missing = n()) #Companies in each sector with less than two scores

sector_climate_leadership = cbind(sector_climate_leadership_n, sector_climate_leadership_n_nodata)
sector_climate_leadership[,3] = NULL
sector_climate_leadership$avail = 100-(
  sector_climate_leadership$available/
    (sector_climate_leadership$available + sector_climate_leadership$missing))*100 #Calculate percentage of companies with more than two data points in each sector


climate_leadership_score = climate_leadership %>%
  group_by(Sector) %>%
  dplyr::summarise(scr = mean(score, na.rm = T), scr_sd = sd(score, na.rm = T)) #Calculate mean score per sector

climate_leadership_score = left_join(sector_climate_leadership, climate_leadership_score) #Link sectro scores to sector missing data stats

sector_climate_leadership_year = climate_leadership %>%
  group_by(Sector,year) %>%
  summarise(missing = sum(is.na(score)), ss = n())
sector_climate_leadership_year$perc = (sector_climate_leadership_year$missing/sector_climate_leadership_year$ss)*100
sector_climate_leadership_year = left_join(sector_climate_leadership_year, sector_climate_leadership, by = "Sector") #Change in sector missing values over time

sector_climate_leadership_year_excl = climate_leadership %>%
  group_by(year) %>%
  summarise(missing = sum(is.na(score)), ss = n())
sector_climate_leadership_year_excl$perc = 
  (sector_climate_leadership_year_excl$missing/sector_climate_leadership_year_excl$ss)*100 #change in missing values across all sectors


#Summarise tweets data ----
tweets_missing_data = tweets %>%
  group_by(screen_name) %>%
  dplyr::summarise(count = n(), start_down = min(created_at), stat = max(statuses_count), create = min(account_created_at))
tweets_missing_data$prop = (tweets_missing_data$count/tweets_missing_data$stat)*100
tweets_missing_data = tweets_missing_data[order(tweets_missing_data$create),]
tweets_missing_data$id = nrow(tweets_missing_data):1
dates = as.POSIXct(seq.Date(as.Date("2010/1/1"), as.Date("2020/1/1"), by = 7))
pool_tweets_missing_data = NULL
for(a in seq_along(dates)){
  tmp_tweets = data.frame(
    date = dates[a], 
    id = nrow(tweets_missing_data):1,
    test = tweets_missing_data$start_down < dates[a])
  pool_tweets_missing_data = rbind(pool_tweets_missing_data, tmp_tweets)
}

tweets_year = tweets %>%
  group_by(Company.Name, screen_name, year) %>%
  dplyr::summarise(
    prop_env = mean(enviro, na.rm = T)*100, 
    follow = median(followers_count, na.rm = T))


#Characterise tweets data ----
tweet_corpus <- Corpus(VectorSource(tweets$text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
tweet_corpus <- tm_map(tweet_corpus, toSpace, "/")
tweet_corpus <- tm_map(tweet_corpus, toSpace, "@")
tweet_corpus <- tm_map(tweet_corpus, toSpace, "\\|")
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removeNumbers)
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
tweet_corpus <- tm_map(tweet_corpus, stemDocument)

tweet_dtm <- TermDocumentMatrix(tweet_corpus)
tweet_dtm2 <- removeSparseTerms(tweet_dtm, sparse = 0.999)
tweet_matrix <- as.matrix(tweet_dtm2)
tweet_term_n <- sort(rowSums(tweet_matrix),decreasing=TRUE)
tweet_term_n <- data.frame(word = names(tweet_term_n),freq=tweet_term_n)
rownames(tweet_term_n) = NULL
head(tweet_term_n, 150)

remove_terms = c(
  "tco", "https","can","http","will","amp","take","get","sorri","now","busi","one","use"
)

trim_tweet_term_n = tweet_term_n[c(1:150),]
trim_tweet_term_n$word = as.character(trim_tweet_term_n$word)
trim_tweet_term_n = trim_tweet_term_n[!trim_tweet_term_n$word %in% remove_terms,]
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "discov",
                                "discover", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "suppli",
                                "supplier", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "futur", 
                                "future", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "ensur", 
                                "ensure", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "pleas", 
                                "please", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "servic",
                                "service", trim_tweet_term_n$word)
trim_tweet_term_n$word = ifelse(trim_tweet_term_n$word == "manag",
                                "manage", trim_tweet_term_n$word)
trim_tweet_term_n = subset(trim_tweet_term_n, freq != 7922)
trim_tweet_term_n = subset(trim_tweet_term_n, freq != 7747)
trim_tweet_term_n = subset(trim_tweet_term_n, freq != 9440)
write.csv(trim_tweet_term_n, "Corporate/word_counts.csv")
trim_tweet_term_n$col = "grey"
trim_tweet_term_n$col= ifelse(trim_tweet_term_n$word == "sustain", "darkgreen", trim_tweet_term_n$col)



#Download stocks data ----
first.date <- "2010-01-01"
last.date <- "2020-01-01"
freq.data <- 'yearly'
tickers <- stocks$Stock.Code
tickers = tickers[which(!is.na(tickers))]

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
lse_price = l.out$df.tickers[,c("ticker","ref.date","price.adjusted")]
colnames(lse_price) = c("Stock.Code","year","stock_pri")
lse_price$year = lubridate::year(lse_price$year)
lse_price = left_join(lse_price, stocks[,c(2:3)])

#Pool tidied data ----
pool_df = left_join(climate_leadership, tweets_year)
pool_df = left_join(pool_df, lse_price)
pool_df = subset(pool_df, Company.Name != "St. James Place")
summarise_pool = NULL
for(a in unique(pool_df$Company.Name)){
  sub = subset(pool_df, Company.Name == a)
  sub = sub[complete.cases(sub),]
  if(sum(!is.na(sub$score)) > 1 & sum(!is.na(sub$prop_env)) > 1 & sum(!is.na(sub$stock_pri)) > 1 ){
    score_mod = lm(log(score)~year, data = sub)
    prop_mod = lm(log(prop_env+1)~year, data = sub)
    pri_mod = lm(log(stock_pri)~year, data = sub)
    tmp_df = data.frame(
      Sector = sub$Sector[1],
      Company.Name = a,
      score = mean(sub$score, na.rm = T), 
      score_rate = score_mod$coefficient[[2]],
      prop_env = mean(sub$prop_env, na.rm = T), 
      prop_env_rate = prop_mod$coefficient[[2]],
      pri = mean(sub$stock_pri, na.rm = T), 
      pri_rate = pri_mod$coefficient[[2]],
      follow = mean(sub$follow, na.rm = T))
  } else {
    tmp_df = data.frame(
      Sector = sub$Sector[1],
      Company.Name = a,
      score = mean(sub$score, na.rm = T), 
      score_rate = NA,
      prop_env = mean(sub$prop_env, na.rm = T), 
      prop_env_rate = NA,
      pri = mean(sub$stock_pri, na.rm = T), 
      pri_rate = NA,
      follow = mean(sub$follow, na.rm = T))
  }
  summarise_pool = rbind(summarise_pool, tmp_df)
}
summarise_pool = summarise_pool[complete.cases(summarise_pool),]



#Model change in climate leadership - all companies ----
climate_leadership= as.data.frame(climate_leadership[order(climate_leadership$Sector, climate_leadership$Company.Name, climate_leadership$year),]) 
climate_leadership$Company.Name = as.factor(climate_leadership$Company.Name)

m1 = lme4::lmer(score ~ year + (1 |Sector/Company.Name), data = climate_leadership)
AIC(m1)
summary(m1)
ranef(m1)
0.14 + (1.96*0.012)
0.14 -(1.96*0.012)
png("Corporate/m1_resid.png", width = 500, height = 500, res = 150)
densityplot(residuals(m1), xlab = "Residuals")
dev.off()
MuMIn::r.squaredGLMM(m1)





model_save_m1 = data.frame(
  model = "m1",
  formula = "score ~ year + (1|Sector/Company.Name",
  term = "year",
  coef = summary(m1)$coefficients[2,1],
  lci_95 = conf_m1[5,1],
  uci_95 = conf_m1[5,2])




climate_leadership_sub = climate_leadership[climate_leadership$Company.Name %in% summarise_pool$Company.Name, ]
m1_sub = lmer(score ~ year + (1|Sector/Company.Name), data = climate_leadership_sub)
summary(m1_sub)
0.16 + (1.96*0.017)
0.16 -(1.96*0.017)

best_sec = climate_leadership[which(!is.na(climate_leadership$score)),] %>%
  group_by(Sector) %>%
  dplyr::summarise(mn_score = mean(score, na.rm = T), sd_score = sd(score, na.rm = T), N = n())

#Model change in sustainable messaging - all companies ----
m2 = glmer(enviro ~ year_from_2010 + (1 + year_from_2010|Sector/Company.Name), data = tweets, family=binomial(link='logit'))
#saveRDS(m2, "Corporate/Tweets_model.rds")
m2 = readRDS("Corporate/Tweets_model.rds")
summary(m2)
0.35 + (1.96*0.08)
0.35 -(1.96*0.08)
0.35 + ranef(m2)$Sector[,2]

m2_assump = data.frame(
  fit = m2@frame$enviro,
  pred = predict(m2),
  resid = scale(residuals(m2)))
MuMIn::r.squaredGLMM(m2)

ggplot() +
  geom_jitter(data = m2_assump, aes(x = pred, y = resid), alpha = 0.1) +
  geom_smooth(data = m2_assump, aes(x = pred, y = resid)) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(text = element_text(size=14)) +
  labs(x = "Predicted values", y = "Standardised residuals")
ggsave(plot = last_plot(), "Corporate/m2_resid.png", width = 5, height = 4)


model_save_m2 = data.frame(
  model = "m2",
  formula = "enviro_tweet_bin ~ year + (1 + year|Sector/Company.Name",
  term = "year",
  coef = summary(m2)$coefficients[2,1],
  lci_95 = 0.35 - (1.96*0.08),
  uci_95 = 0.35 + (1.96*0.08))

ggpredict(m2, terms = "year_from_2010[0]")
ggpredict(m2, terms = "year_from_2010[10]")

predict(m2, newdata = data.frame(year_from_2010 = 0,
                                 Sector = "General",
                                 Company.Name = "Renewi"), type = "response")
predict(m2, newdata = data.frame(year_from_2010 = 10,
                                 Sector = "General",
                                 Company.Name = "Renewi"), type = "response")

tweets_sub = tweets[tweets$Company.Name %in% summarise_pool$Company.Name, ]
m2_sub = glmer(enviro ~ year_from_2010 + (1 + year_from_2010|Company.Name/Sector), data = tweets_sub, family=binomial(link='logit'))
summary(m2_sub)
0.38 + (1.96*0.05)
0.38 -(1.96*0.05)
MuMIn::r.squaredGLMM(m2_sub)

best_sec2 = tweets[which(!is.na(tweets$enviro)),] %>%
  group_by(Sector) %>%
  dplyr::summarise(mn_enviro = mean(enviro, na.rm = T), sd_enviro = sd(enviro, na.rm = T), N = n())


#Model change in sustainable messaging - all companies ----




#Model PSEM ----
summarise_pool$follow = log10(summarise_pool$follow)
summarise_pool$score_rate = (exp(summarise_pool$score_rate)*100)-100
summarise_pool$prop_env_rate = (exp(summarise_pool$prop_env_rate)*100)-100
summarise_pool$pri_rate = (exp(summarise_pool$pri_rate)*100)-100
summarise_pool$wash = residuals(lm(prop_env~score, data = summarise_pool))
summarise_pool = subset(summarise_pool, Company.Name != "Taylor Wimpey Plc" & Company.Name != "Kingfisher")

full_psem <- psem(
  lmer(score ~ follow + (1|Sector),data = summarise_pool),
  lmer(prop_env ~ score + follow + (1|Sector),data = summarise_pool),
  lmer(score_rate ~ score + follow + (1|Sector),data = summarise_pool),
  lmer(prop_env_rate ~ prop_env + score + score_rate + follow + (1|Sector),data = summarise_pool),
  lmer(pri_rate ~ wash + prop_env + score + score_rate + prop_env_rate + (1|Sector),data = summarise_pool))

hist(residuals(full_psem[[1]]))
hist(residuals(full_psem[[2]]))
hist(residuals(full_psem[[3]]))
hist(residuals(full_psem[[4]]))
hist(residuals(full_psem[[5]]))

summarise_pool2 = summarise_pool
summarise_pool2$prop_env = log10(summarise_pool2$prop_env+0.1)


full_psem <- psem(
  lmer(score ~ 
         follow + 
         (1|Sector),data = summarise_pool2),
  lmer(prop_env ~ 
         score + 
         follow + 
         (1|Sector),data = summarise_pool2),
  lmer(score_rate ~ 
         score + 
         follow + 
         (1|Sector),data = summarise_pool2),
  lmer(prop_env_rate ~ 
         prop_env + 
         score + 
         score_rate + 
         follow + 
         (1|Sector),data = summarise_pool2),
  lmer(pri_rate ~ 
         wash +
         score_rate + 
         prop_env_rate + 
         (1|Sector),data = summarise_pool2))


hist(residuals(full_psem[[1]]))
hist(residuals(full_psem[[2]]))
hist(residuals(full_psem[[3]]))
hist(residuals(full_psem[[4]]))
hist(residuals(full_psem[[5]]))
plot(full_psem[[1]])
plot(full_psem[[2]])
plot(full_psem[[3]])
plot(full_psem[[4]])
plot(full_psem[[5]])
summary(full_psem)

confint(full_psem[[1]], method = "boot")
confint(full_psem[[2]], method = "boot")
confint(full_psem[[3]], method = "boot")
confint(full_psem[[4]], method = "boot")
confint(full_psem[[5]], method = "boot")





# Plots missing data ----
plt_sector_climate_leadership_missing_perc = 
  ggplot(data = sector_climate_leadership, 
         aes(x = reorder(Sector, -avail),
             y = avail,
             fill = reorder(Sector, -avail)),
         colour = "black") +
  guides(fill = F) +
  geom_col(alpha = 0.55) +
  geom_text(aes(label = paste0("N = ", (available + missing)/10), hjust = -0.5)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(values = miss_sector_pal) +
  coord_flip() +
  labs(x = "", y = " ") +
  theme_classic() +
  guides(colour = F)


plt_climate_leadership_score = ggplot() +
  geom_pointrange(data = climate_leadership_score, aes(x = avail, ymin = scr-scr_sd, y = scr, ymax = scr+scr_sd, colour = reorder(Sector, -avail))) +
  geom_smooth(data = climate_leadership_score, aes(x = avail, y = scr), method = "lm", colour = "black", alpha = 0.15, fullrange = T) +
  scale_colour_manual(values = miss_sector_pal) +
  xlim(0,100) +
  scale_y_continuous(breaks = c(1:8), labels = c("D-", "D", "C-", "C", "B-", "B", "A-", "A")) +
  guides(colour = F) +
  theme_classic() +
  labs(x = "Missing climate scores (%)", y = " ")

plt_sector_climate_leadership_year = ggplot() +
  geom_line(data = sector_climate_leadership_year_excl, aes(x = year, y = perc), size = 2, linetype = "dashed") +
  geom_line(data = sector_climate_leadership_year, aes(x = year, y = perc, colour = reorder(Sector, -avail)), size = 1.5, alpha = 0.35) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(breaks = c(2011,2013,2015,2017,2019)) +
  labs(x = " ", y = "Missing climate scores (%)") +
  scale_colour_manual(values = miss_sector_pal) +
  theme_classic() +
  guides(colour = F)

plt_tweets_missing_year = ggplot(tweets_missing_data) +
  geom_linerange(aes(x = id, ymin = create, ymax = start_down), colour = "blue", alpha = 0.05) +
  geom_linerange(aes(x = id, ymin = start_down, ymax = as.POSIXct("2020-01-01", tz = "UTC")), colour = "blue", alpha = 0.4) +
  coord_flip(ylim = c(as.POSIXct("2010-01-01"), as.POSIXct("2020-01-01"))) +
  scale_y_datetime(breaks = c(as.POSIXct("2011-01-01"), as.POSIXct("2013-01-01"), as.POSIXct("2015-01-01"), as.POSIXct("2017-01-01"), as.POSIXct("2019-01-01")), labels = c("2011","2013","2015","2017","2019")) +
  theme_classic() +
  labs(x = "Company id", y = "Year")

ggarrange(
  ggarrange(
    plt_sector_climate_leadership_missing_perc,
    plt_climate_leadership_score, 
    nrow = 2, labels = c("a", "c"), align = "v"),
  ggarrange(
    plt_sector_climate_leadership_year,
    plt_tweets_missing_year, 
    nrow = 2, labels = c("b", "d"), align = "v"),
          ncol = 2, widths = c(1,0.7))
ggsave(plot = last_plot(), "Corporate/missing.png", width = 8.5, height = 6.5, dpi = 300)



#Plots change over time ----

m1_predict = ggpredict(m1, terms = "year")

plt_m1_climate_leadership_model = ggplot() +
  geom_line(data = climate_leadership, stat = "smooth", method = lm, aes(x = year, y = score, group = Company.Name), color = "grey", size = 1, alpha = 0.1, se = F) +
  geom_line(data = m1_predict, aes(x = x, y = predicted), size = 1.2) +
  geom_ribbon(data = m1_predict, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_classic() +
  labs(x = "Year", y = "\nClimate leadership score") +
  scale_x_continuous(breaks = c(2011,2013,2015,2017,2019)) +
  scale_y_continuous(breaks = c(1:8), labels = c("D-", "D", "C-", "C", "B-", "B", "A-", "A")) +
  coord_cartesian(ylim = c(1,9))


m2_predict = ggpredict(m2, terms = "year_from_2010")

plt_m2_tweets_year = ggplot() +
  geom_line(data = tweets, stat = "smooth", method = glm, method.args = list(family = "binomial"), aes(x = year_from_2010, y = enviro, group = Company.Name), color = "grey", size = 1, alpha = 0.1, se = F) +
  geom_line(data = m2_predict, aes(x = x, y = predicted), size = 1.2) +
  geom_ribbon(data = m2_predict, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_classic() +
  labs(x = "Year", y = "Probability of tweet\ncontaining environmental term") +
  scale_x_continuous(breaks = c(1,3,5,7,9), labels = c("2011","2013","2015","2017","2019"), limits = c(0,10)) +
  coord_cartesian(ylim = c(0,0.2))


plt_tweets_wordcloud = ggplot(trim_tweet_term_n, aes(label = word, size = freq, colour = col)) +
  scale_colour_manual(values = c("dark green", "grey")) +
  geom_text_wordcloud_area(shape = "square") +
  scale_size_area(max_size = 10.5) +
  theme_minimal()

ggarrange(
  ggarrange(
    plt_m1_climate_leadership_model,
    plt_m2_tweets_year,
    nrow = 2, labels = c("a","c")),
  plt_tweets_wordcloud,
  ncol = 2, labels = c(" ", "b"), widths = c(0.7,1))
ggsave(plot = last_plot(), "Corporate/change_year.png", width = 8.5, height = 6.5, dpi = 300)


#Plots psem ----
sem1_predict = ggpredict(full_psem[[1]], terms = "follow")
sem2_predict = ggpredict(full_psem[[2]], terms = "score")
sem3_predict = ggpredict(full_psem[[4]], terms = "prop_env")


plt_climate_leadership_follow = ggplot() +
  geom_point(data = summarise_pool, aes(x = 10^follow, y = score), colour = "black", alpha = 0.7) +
  geom_line(data = sem1_predict, aes(x = 10^x, y = predicted)) +
  geom_ribbon(data = sem1_predict, aes(x = 10^x, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  scale_x_log10(breaks = c(100,1000,10000, 100000), labels = c("100","1000", "10000", "100000"),  expand = c(0, 0), limits = c(100, 350000)) +
  scale_y_continuous(breaks = c(1:9), labels = c("E", "D-", "D", "C-", "C", "B-", "B", "A-", "A"),  expand = c(0, 0), limits = c(2.5,9.2)) +
  theme_classic() +
  labs(x = "\nFollowers (N)", y = "\nClimate leadership score") 


plt_tweets_climate_leadership = ggplot() +
  geom_point(data = summarise_pool, aes(x = score, y = prop_env), colour = "black", alpha = 0.7) +
  geom_line(data = sem2_predict, aes(x = x, y = 10^(predicted)-0.1)) +
  geom_ribbon(data = sem2_predict, aes(x = x, ymin = 10^(conf.low)-0.1, ymax = 10^(conf.high)-0.1), alpha = 0.1) +
  scale_x_continuous(breaks = c(1:9), labels = c("E", "D-", "D", "C-", "C", "B-", "B", "A-", "A"), expand = c(0, 0), limits = c(3,9.1)) +
  scale_y_continuous(breaks = c(0,5,10,15), expand = c(0, 0)) +
  theme_classic() +
  labs(x = "\nClimate leadership score", y = "\n Environmental messaging (%)") 


plt_tweet_rate_mess = ggplot() +
  geom_point(data = summarise_pool, aes(x = prop_env, y = prop_env_rate), colour = "black", alpha = 0.7) +
  geom_line(data = sem3_predict, aes(x = 10^(x)-0.1, y = predicted)) +
  geom_ribbon(data = sem3_predict, aes(x = 10^(x)-0.1, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  scale_x_continuous(breaks = c(0,5,10,15),  expand = c(0, 0)) +
  theme_classic() +
  labs(x = "\n Environmental messaging (%)", y = "Annual change\n in environmental messaging (%)") 



ggarrange(
  plt_climate_leadership_follow,
  plt_tweets_climate_leadership ,
  plt_tweet_rate_mess, 
  ncol = 3, align = "h", labels = c("a", "b", "c"))
ggsave("Corporate/psem.png", width = 8, height = 3)


summarise_pool$Sector <- factor(summarise_pool$Sector, levels = c("Agricultural commodities", "Coal", "Construction","Chemicals", "Transport services", "General", "Electric utilities", "Paper & forestry", "Metals & mining", "Oil & gas", "Food, beverage & tobacco"))

miss_sector_pal2 = c(
  "#A214FF", "#BF6BF7", "#E0B7FC", "#C3B7FC", "#D3EBFA",  "#A9D6F5", "#5AB7F5", "#2998E2", "#1273B3", "#064670"
)

a = ggplot(summarise_pool) +
  geom_boxplot(aes(y = Sector, x = score, fill = Sector), alpha = 0.55) +
  geom_jitter(aes(y = Sector, x = score, colour = Sector), alpha = 0.55) +
  scale_fill_manual(values = miss_sector_pal2) +
  scale_colour_manual(values = miss_sector_pal2) +
  scale_x_continuous(breaks = c(1:9), labels = c("E", "D-", "D", "C-", "C", "B-", "B", "A-", "A"),  expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Climate score\n", y = " ")
a
b = ggplot(summarise_pool) +
  geom_boxplot(aes(y = Sector, x = prop_env, fill = Sector), alpha = 0.55) +
  geom_jitter(aes(y = Sector, x = prop_env, colour = Sector), alpha = 0.55) +
  scale_fill_manual(values = miss_sector_pal2) +
  scale_colour_manual(values = miss_sector_pal2) +
  scale_x_continuous(breaks = c(0,5,10,15),   expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  labs(x = "Environmental tweets (%)\n", y = " ")

c = ggplot(summarise_pool) +
  geom_boxplot(aes(y = Sector, x = score_rate, fill = Sector), alpha = 0.55) +
  geom_jitter(aes(y = Sector, x = score_rate, colour = Sector), alpha = 0.55) +
  scale_fill_manual(values = miss_sector_pal2) +
  scale_colour_manual(values = miss_sector_pal2) +
  scale_x_continuous(breaks = c(-20,0,20,40),   expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  labs(x = "Annual change\n in climate score (%)", y = " ")

d = ggplot(summarise_pool) +
  geom_boxplot(aes(y = Sector, x = prop_env_rate, fill = Sector), alpha = 0.55) +
  geom_jitter(aes(y = Sector, x = prop_env_rate, colour = Sector), alpha = 0.55) +
  scale_fill_manual(values = miss_sector_pal2) +
  scale_colour_manual(values = miss_sector_pal2) +
  scale_x_continuous(breaks = c(-50,0,50,100,150),   expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  labs(x = "Annual change in\n environmental tweets (%)", y = " ")

ggarrange(a,b,c,d, ncol = 4, widths = c(0.82,0.5,0.5,0.5))
ggsave("Corporate/sectors.png", width = 10, height = 5)




library(ordinal)

climate_leadership_ord = climate_leadership
climate_leadership_ord$score = climate_leadership_ord$score - 1
climate_leadership_ord$score = ifelse(climate_leadership_ord$score == 1, 2, climate_leadership_ord$score)
climate_leadership_ord$score = ifelse(climate_leadership_ord$score == 3, 4, climate_leadership_ord$score)
climate_leadership_ord$score = ifelse(climate_leadership_ord$score == 5, 6, climate_leadership_ord$score)
climate_leadership_ord$score = ifelse(climate_leadership_ord$score == 7, 8, climate_leadership_ord$score)

climate_leadership_ord$score = as.ordered(climate_leadership_ord$score)
climate_leadership_ord = climate_leadership_ord[complete.cases(climate_leadership_ord),]

m1_ord = clmm(score ~ scale(year) + (1|Company.Name), data = climate_leadership_ord)
summary(m1_ord)
confint(m1_ord)
m1_ord_pred = ggpredict(m1_ord, terms = "year[2010:2019]")
levels(m1_ord_pred$response.level) <- rev(c(
  "A", 
  "B", 
  "C",
  "D"))
m1_ord_pred$response.level <- factor(m1_ord_pred$response.level,levels(m1_ord_pred$response.level)[4:1])

ggplot(m1_ord_pred) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x,  ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks = c(2011,2013,2015,2017,2019)) +
  facet_wrap(response.level~.) +  
  theme_classic() +
  labs(x = "Year", y = "Probability") 
ggsave("Corporate/ordinal.png", width = 7, height = 7)

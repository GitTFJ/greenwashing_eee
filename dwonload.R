library(rtweet)
acc = read.csv("Corporate/UK_accounts.csv")
acc$Account = as.character(acc$Account)
acc = subset(acc, !is.na(Account))

TwitterTokens = create_token(app = "my_app",
                             consumer_key = "9wGggmqWS5MADrH7n07gGuBm4", 
                             consumer_secret = "Z6GrgY6EY9B0wZuTKjSlMJ6G2s2qNMNgrVL7FGfPJh9hSEcZtU",
                             access_token = "920713349243047936-wNEhOU1dlpnxfDrcK6K1r7AN4ur02ys",
                             access_secret = "R5e7D7leiavqWQVEUwSYd17xdJq7yTlhyI4WXu0N4gtIG")

comb_df = NULL
summary_df = NULL
for(a in 1:nrow(acc)){
  name = acc$Account[a]
  df = get_timelines(user = name,
                     n = 3200,
                     parse = T)
  
  if(length(df) == 90){
    df$Company.Name = as.character(acc$Company.Name[a])
    df$NumberID = acc$NumberID[a]
    comb_df = rbind(comb_df, df)
  }
  
  tmp = data.frame(Account = as.character(name), Observations = nrow(df))
  message(paste(tmp[,1],": ", tmp[,2]))
  summary_df = rbind(summary_df, tmp)
  
  if(nrow(df) > 3000){
    print(paste("Sleeping - restarts at: ", Sys.time() + (60*3)))
    Sys.sleep(60*3)
  } else if (nrow(df) > 2000){
    print(paste("Sleeping - restarts at: ", Sys.time() + (60*2)))
    Sys.sleep(60*2)
  } else if (nrow(df) > 1000){
    print(paste("Sleeping - restarts at: ", Sys.time() + (60*1)))
    Sys.sleep(60*1)
  }  else{
    print(paste("Sleeping - restarts at: ", Sys.time() + (60*0.5)))
    Sys.sleep(60*0.5)
  }
}

write.csv(summary_df, "Corporate/download_summary.csv")
saveRDS(comb_df, "Corporate/download.rds")

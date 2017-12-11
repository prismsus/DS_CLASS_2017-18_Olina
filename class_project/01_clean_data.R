#get data from web
#s1
webpage1 <-
  read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_contestants")
#table 1
tbls_s1 <- webpage1 %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)
#s2
webpage2 <-
  read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_Season_2_contestants")
#table 1
tbls_s2 <- webpage2 %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = T)
#s1
pd101_s1_trainee <- tbls_s1[[1]]
#clean korean characters
pd101_s1_trainee$Company <-
  str_replace_all(pd101_s1_trainee$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s1_trainee$Name <-
  str_replace_all(pd101_s1_trainee$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s1_trainee$Name <-
  gsub("[[:digit:]]", "", pd101_s1_trainee$Name)
names(pd101_s1_trainee) <-
  make.names(names(pd101_s1_trainee), unique = T)
#clean first few rows
pd101_s1_trainee <-
  filter(pd101_s1_trainee, Age > 10 & Age < 50) %>%
  #convert types
  mutate(
    Age = type.convert(Age),
    Ranking = type.convert(Ranking),
    Judges.evaluation = type.convert(Judges.evaluation),
    evaluation1_num = as.numeric(Judges.evaluation),
    Judges.evaluation.1 = type.convert(Judges.evaluation.1),
    evaluation2_num = as.numeric(Judges.evaluation.1) - 1,
    Ranking = as.numeric(Ranking),
    #ep1 ranking
    Ranking.1 = as.numeric(Ranking.1),
    #ep2 ranking
    Ranking.12 = as.numeric(Ranking.12),
    Gender = "F"
  )
#delete empty space after names
pd101_s1_trainee <- pd101_s1_trainee %>%
  mutate(Name = str_trim(Name))
#rename variables
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking"] <-
  "Ranking_ep1"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.1"] <-
  "Ranking_ep2"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.2"] <-
  "Ranking_ep3"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.3"] <-
  "Ranking_ep5"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.4"] <-
  "Votes_ep5"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.5"] <-
  "Ranking_ep6"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.6"] <-
  "Ranking_ep8"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.7"] <-
  "Votes_ep8"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.8"] <-
  "Ranking_ep10"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.9"] <-
  "Votes_ep10"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.10"] <-
  "Ranking_ep11"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.11"] <-
  "Votes_ep11"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Ranking.12"] <-
  "Ranking_final"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Judges.evaluation"] <-
  "Evaluation1"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee) == "Judges.evaluation.1"] <-
  "Evaluation2"
#s2
pd101_s2_trainee <- tbls_s2[[1]]
#clean korean characters
pd101_s2_trainee$Company <-
  str_replace_all(pd101_s2_trainee$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s2_trainee$Name <-
  str_replace_all(pd101_s2_trainee$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s2_trainee$Name <-
  gsub("[[:digit:]]", "", pd101_s2_trainee$Name)
names(pd101_s2_trainee) <-
  make.names(names(pd101_s2_trainee), unique = T)
#clean first few rows
pd101_s2_trainee <-
  filter(pd101_s2_trainee, Age > 10 & Age < 50) %>%
  #convert types
  mutate(
    Age = type.convert(Age),
    Ranking = type.convert(Ranking),
    Judges.evaluation = type.convert(Judges.evaluation),
    evaluation1_num = as.numeric(Judges.evaluation),
    Judges.evaluation.1 = type.convert(Judges.evaluation.1),
    evaluation2_num = as.numeric(Judges.evaluation.1),
    Ranking = as.numeric(Ranking),
    #ep1 ranking
    Ranking.1 = as.numeric(Ranking.1),
    #ep2 ranking
    Ranking.12 = as.numeric(Ranking.12),
    Gender = "M"
  )

#delete empty space after names
pd101_s2_trainee <- pd101_s2_trainee %>%
  mutate(Name = str_trim(Name))
#rename variables
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking"] <-
  "Ranking_ep1"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.1"] <-
  "Ranking_ep2"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.2"] <-
  "Ranking_ep3"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.3"] <-
  "Ranking_ep5"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.4"] <-
  "Votes_ep5"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.5"] <-
  "Ranking_ep6"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.6"] <-
  "Ranking_ep8"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.7"] <-
  "Votes_ep8"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.8"] <-
  "Ranking_ep10"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.9"] <-
  "Votes_ep10"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.10"] <-
  "Ranking_ep11"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.11"] <-
  "Votes_ep11"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Ranking.12"] <-
  "Ranking_final"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Judges.evaluation"] <-
  "Evaluation1"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee) == "Judges.evaluation.1"] <-
  "Evaluation2"
#combine
pd101_s1_heights <- read_csv("pd101_heights_s1.csv")
pd101_s2_heights <- read_csv("pd101_heights_s2.csv")
pd101_s1_trainee <- pd101_s1_trainee %>%
  left_join(pd101_s1_heights, by = "Name") %>%
  mutate(`Training time (months)` = Training_time_month + Training_time_years *
           12)
pd101_s2_trainee <- pd101_s2_trainee %>%
  left_join(pd101_s2_heights, by = "Name") %>%
  mutate(`Training time (months)` = Training_time_month + Training_time_years *
           12)
df <- rbind(pd101_s1_trainee, pd101_s2_trainee)

pd101_s1_height_age <- pd101_s1_trainee %>%
  select(Name, Company, Age, Height, Ranking_final, Evaluation1)
pd101_s2_height_age <- pd101_s2_trainee %>%
  select(Name, Company, Age, Height, Ranking_final, Evaluation1)


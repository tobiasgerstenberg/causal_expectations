#' ---
#' title: Causal expectations analysis file
#' author: Tobias Gerstenberg
#' date: June 20, 2018
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' ------------------------------------------------------------------------------

knitr::opts_chunk$set(fig.width=10, fig.height=6, warning=FALSE, message=FALSE)
figure.path = "../../figures/"


#+ Load packages -------------------------------------------------------------------------------
#' # Load packages 
library(RSQLite)
library(tidyjson)
library(broom)
library(scales)
library(knitr)
library(tidyverse)

#+ Read and structure data  --------------------------------------------------------------------
#' # Read and structure data  

con = dbConnect(SQLite(),dbname = "../../data/data_anonymized.db");
df.data = dbReadTable(con,"causal_expectations")
dbDisconnect(con)

# filter out incompletes 
df.data = df.data %>% 
  filter(status %in% 3:5)

# demographic information 
df.demographics = df.data$datastring %>% 
  spread_values(age = jstring('questiondata','age'),
                gender = jstring('questiondata','sex'),
                feedback = jstring('questiondata','feedback')
  ) %>% 
  rename(participant = document.id) %>% 
  mutate(time = difftime(df.data$endhit,df.data$beginhit,units = 'mins'),
         age = as.numeric(age)) %>% 
  bind_cols(df.data %>% select(experiment = codeversion))

# practice data: count how many of the 30 practice questions (3 for each of the 10 trials) participants
# answered correctly
df.practice = df.data$datastring %>% 
  as.tbl_json() %>% 
  spread_values(structure = jstring(questiondata,structure)) %>% 
  enter_object('data') %>%
  gather_array('order') %>% 
  enter_object('trialdata') %>% 
  gather_array('index') %>%
  append_values_string('values') %>% 
  mutate(index = rep(c('name','value'),nrow(.)/2)) %>% 
  filter(index == 'value') %>% 
  mutate(index = rep(c('id','blockA','blockB','answerA','answerB','answerOutcome'),nrow(.)/6)) %>% 
  rename(participant = document.id) %>% 
  left_join(df.demographics %>% select(participant,experiment),by = 'participant') %>% 
  spread(index,values) %>% 
  mutate_at(vars('blockA','blockB'),funs(ifelse(. == 'FALSE','no','yes'))) %>% 
  mutate(outcome = ifelse(structure == 'conjunctive' & (blockA == 'no' & blockB == 'no'),'yes','no'),
         outcome = ifelse(structure == 'disjunctive' & (blockA == 'no' | blockB == 'no'),'yes',outcome),
         outcome = ifelse(structure == 'xor' & (blockA != blockB),'yes',outcome),
         scoreA = ifelse(blockA == answerA, 1, 0),
         scoreB = ifelse(blockB == answerB, 1, 0),
         scoreOutcome = ifelse(outcome == answerOutcome, 1, 0)) %>% 
  group_by(participant,experiment,structure) %>% 
  summarise(score = sum(scoreA,scoreB,scoreOutcome)) %>% 
  ungroup()

# main data 
df.long = df.data$datastring %>% 
  as.tbl_json() %>% 
  enter_object('questiondata') %>% 
  gather_object('name') %>% 
  append_values_string('value') %>% 
  as.data.frame() %>% 
  rename(participant = document.id) %>% 
  left_join(df.demographics %>% select(participant,experiment),by = 'participant') %>% 
  spread(name,value) %>% 
  mutate_at(vars('Awill','Bwill','counterfactual','pA','pB'),funs(as.numeric(.))) %>% 
  mutate(outcome = NA,
         outcome = ifelse(experiment == 'experiment_1', 'positive','negative'),
         balls = ifelse(experiment %in% paste0("experiment_",c(1,3)),'both balls go through', 'both balls are blocked'),
         balls = factor(balls,levels = c('both balls go through', 'both balls are blocked')),
         choice = 'normal',
         choice = ifelse(pA == 0.2 & sentenceChoice == 'Acause','abnormal',choice),
         choice = ifelse(pB == 0.2 & sentenceChoice == 'Bcause','abnormal',choice)
         ) %>% 
  left_join(df.practice %>% select(participant,score),by = 'participant') %>% 
  select(-age,-sex,-condition,-counterbalance) %>% 
  select(participant,experiment,outcome,balls,structure,pA,pB,Awill,Bwill,everything(),-feedback,feedback)

# apply exclusion criteria 
df.long = df.long %>% 
  filter(score >= 28) %>% #exclude based on answers to practice trials 
  filter((pA == 0.2 & pB == 0.8 & (Awill+20) < Bwill) |
           (pA == 0.8 & pB == 0.2 & (Bwill+20) < Awill) |
           (pA == pB & abs(Awill-Bwill) < 20)) %>% #exclude based on probability ratings
  filter((structure == 'conjunctive' & counterfactual < 50) |
           (structure == 'disjunctive' & counterfactual > 50) | 
           (structure == 'xor' & counterfactual > 50)) #exclude based on counterfactual rating
  
# only use first 50 participants in each condition 
df.long = df.long %>%
  group_by(balls,structure) %>%
  filter(row_number() <= 50) %>%
  ungroup() %>%
  arrange(participant)

# remove excluded participants from demographics 
df.demographics = df.demographics %>% 
  filter(participant %in% unique(df.long$participant))

#+ Demographic information ---------------------------------------------------------------------
#' # Demographic information 

df.demographics %>% 
  summarise(age.mean = mean(age),
            age.sd = sd(age),
            n.participants = nrow(.),
            n.female = sum(gender == 'female')) %>% 
  mutate_at(vars(contains('age')),funs(round(.))) %>% 
  kable()

#+ Stats: Binomial exact tests -----------------------------------------------------------------------
#' # Stats: Binomial exact tests 

tmp = df.long %>% 
  mutate(success = ifelse(choice == 'normal',0,1)) %>% 
  group_by(balls,structure) %>% 
  summarise(success = sum(success),
            n = n()
  )

tmp2 = tmp %>% 
  rowwise() %>% 
  do(test = tidy(binom.test(.$success,.$n,p=0.5))) %>% 
  unnest()

df.stats = tmp %>% 
  bind_cols(tmp2) %>% 
  select(balls,structure,estimate,conf.low,conf.high,p.value) %>% 
  mutate_at(vars(estimate,contains('conf')),funs(round(.*100,2))) %>% 
  mutate(result = paste0(estimate,'\\%, CI = [', conf.low, '\\%, ', conf.high, '\\%]')) 

df.stats %>% 
  select(balls,structure,result) %>% 
  kable()

#+ Plot: Percentages ---------------------------------------------------------
#' # Plot: Percentages 

getIntervals = function(x){return(binom.test(sum(na.omit(x)),length(na.omit(x)))$conf.int[1:2])}

df.plot = df.long %>% 
  select(participant,outcome,balls,structure,choice) %>% 
  mutate(choice = factor(choice,
                         levels = c('normal','abnormal'),
                         labels = c('ball that normally goes through', 'ball that normally gets blocked    ')),
         response = choice %>% as.numeric()-1,
         structure = factor(structure,
                            levels = c('conjunctive','disjunctive','xor'),
                            labels = c('AND','OR','XOR'))
  )

df.bars = df.plot %>% 
  count(outcome,balls,structure,choice) %>% 
  group_by(outcome,balls,structure) %>% 
  mutate(percentage = n/sum(n),
         groupsize = sum(n)) %>% 
  ungroup()

df.error = df.plot %>%
  group_by(outcome,balls,structure) %>%
  summarise(ci_low = getIntervals(response)[1],
            ci_high = getIntervals(response)[2]) %>% 
  ungroup()

df.bars = df.bars %>%
  left_join(df.error)

ggplot(df.bars,aes(x = structure,y=percentage))+
  geom_bar(stat= "identity", aes(fill = choice),color="black")+
  geom_hline(yintercept = 0.5,linetype=2,size=1)+
  facet_grid(~balls)+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,color="black", size = 1)+
  scale_y_continuous(labels = percent_format())+
  coord_cartesian(ylim = c(0,1.01), x = c(0.5,3.5), expand = F)+
  scale_fill_brewer(type = 'qual', palette = 3)+
  labs(y = "percentage selected", x = 'causal structure')+
  theme_bw()+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        panel.spacing.x = unit(1,"cm"),
        axis.title.x = element_text(margin = margin(t = 0.5, r = 0, b = 0.1, l = 0,"cm")),
        strip.background = element_blank())+
  guides(fill = guide_legend(reverse = T)
  )
# ggsave("../../figures/results.pdf",width = 10,height = 6)

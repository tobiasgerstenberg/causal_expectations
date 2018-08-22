#' ---
#' title: Causal expectations analysis file
#' author: Tobias Gerstenberg
#' date: August 17, 2018
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
library(Hmisc)
library(tidyverse)

#+ Read and structure data  --------------------------------------------------------------------
#' # Read and structure data  

rm(list = ls())

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
  spread_values(structure = jstring(questiondata, structure)) %>% 
  enter_object('data') %>%
  gather_array('order') %>% 
  enter_object('trialdata') %>% 
  gather_array('index') %>%
  append_values_string('values') %>% 
  mutate(index = rep(c('name', 'value'), nrow(.)/2)) %>% 
  filter(index == 'value') %>% 
  mutate(index = rep(c('id', 'blockA', 'blockB', 'answerA', 'answerB', 'answerOutcome'), nrow(.)/6)) %>% 
  rename(participant = document.id) %>% 
  left_join(df.demographics %>% select(participant, experiment), by = 'participant') %>% 
  spread(index, values) %>% 
  mutate_at(vars('blockA', 'blockB'), funs(ifelse(. == 'FALSE', 'no', 'yes'))) %>% 
  mutate(outcome = ifelse(structure == 'conjunctive' & (blockA == 'no' & blockB == 'no'), 'yes', 'no'),
         outcome = ifelse(structure == 'disjunctive' & (blockA == 'no' | blockB == 'no'), 'yes', outcome),
         outcome = ifelse(structure == 'xor' & (blockA != blockB), 'yes', outcome),
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
  left_join(df.demographics %>% select(participant, experiment), by = 'participant') %>% 
  spread(name,value) %>% 
  mutate_at(vars('Awill', 'Bwill', 'counterfactual', 'pA', 'pB'), funs(as.numeric(.))) %>% 
  mutate(outcome = NA,
         outcome = ifelse(experiment == 'experiment_1', 'positive','negative'),
         outcome = factor(outcome, levels = c('negative', 'positive')),
         balls = ifelse(experiment %in% paste0("experiment_", c(1, 3)), 'both balls go through', 'both balls are blocked'),
         balls = factor(balls, levels = c('both balls go through', 'both balls are blocked')),
         choice = 'normal',
         choice = ifelse(pA == 0.2 & sentenceChoice == 'Acause', 'abnormal', choice),
         choice = ifelse(pB == 0.2 & sentenceChoice == 'Bcause', 'abnormal', choice)
  ) %>% 
  left_join(df.practice %>% select(participant,score),by = 'participant') %>% 
  select(-age,-sex,-condition,-counterbalance) %>% 
  select(participant,experiment,outcome,balls,structure,pA,pB,Awill,Bwill,everything(),-feedback,feedback)

# apply exclusion criteria 
df.long = df.long %>% 
  filter(score >= 28) %>% #exclude based on answers to practice trials 
  filter((pA == 0.2 & pB == 0.8 & (Awill + 20) < Bwill) |
           (pA == 0.8 & pB == 0.2 & (Bwill + 20) < Awill) |
           (pA == pB & abs(Awill-Bwill) < 20)) %>% #exclude based on probability ratings
  filter((structure == 'conjunctive' & counterfactual < 50) |
           (structure == 'disjunctive' & counterfactual > 50) | 
           (structure == 'xor' & counterfactual > 50)) #exclude based on counterfactual rating

# only use first 50 participants in each condition 
df.long = df.long %>%
  group_by(balls, structure) %>%
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
            n.female = sum(gender == 'female'),
            time.mean = mean(time),
            time.sd = sd(time)) %>% 
  mutate_at(vars(contains('age')), funs(round(.))) %>% 
  mutate_at(vars(contains('time')), funs(round(.,2))) %>% 
  kable()

#+ Stats: Binomial exact tests -----------------------------------------------------------------------
#' # Stats: Binomial exact tests 

tmp = df.long %>% 
  mutate(success = ifelse(choice == 'normal',0,1)) %>% 
  group_by(balls, structure) %>% 
  summarise(success = sum(success),
            n = n()
  )

tmp2 = tmp %>% 
  rowwise() %>% 
  do(test = tidy(binom.test(.$success, .$n, p=0.5))) %>% 
  unnest()

df.stats = tmp %>% 
  bind_cols(tmp2) %>% 
  select(balls, structure, estimate, conf.low, conf.high, p.value) %>% 
  mutate_at(vars(estimate, contains('conf')), funs(round(.*100, 2))) %>% 
  mutate(result = paste0(estimate, '\\%, CI = [', conf.low, '\\%, ', conf.high, '\\%]')) 

df.stats %>% 
  select(balls, structure, result) %>% 
  kable()

# Model predictions ---------------------------------------------------------------------------------------

# percentage of selecting the ball that normally gets blocked
df.selections = df.long %>% 
  mutate(structure = factor(structure,levels = c('conjunctive', 'disjunctive', 'xor'), labels = c('and', 'or', 'xor'))) %>% 
  group_by(balls, structure) %>% 
  summarise(percentage = sum(choice == 'abnormal')/n()) %>% 
  ungroup() %>% 
  arrange(balls, structure)

# functions
func_softmax = function(a, b, beta){
  return(exp(a*beta)/rowSums(exp(cbind(a, b)*beta)))
}

func_outcome_probability = function(a, b, structure){
  if (structure == 'or'){
    out = 1-(1-a)*(1-b) #theoretical
    # out = 0.9 #empirical
  }else if(structure == 'and'){
    out = (1-a)*(1-b) #theoretical
    # out = 0.1 #empirical
  }
  else if (structure == 'xor'){
    out = a*(1-b)+(1-a)*b #theoretical
    # out = 0.8 #empirical
  }
  return(out)
}

func_icard = function(a, b, structure,balls){
  if (balls == 1){ #both balls go through
    if (structure == 'and'){
      out = a*b+(1-a)
    }else if (structure == 'or'){
      out = a
    }
    else if (structure == 'xor'){
      out = a*b+(1-a)
    }
  } else if (balls == 0){ #both balls are blocked 
    if (structure == 'and'){
      out = 1-a
    }else if (structure == 'or'){
      out = a*b+(1-b)
    }
    else if (structure == 'xor'){
      out = a*b+(1-b)
    }
  }
  return(out)
}

# predictions 

balls = c(0, 1) #balls go through or don't 
structure = c('and', 'or', 'xor')
change = 0.1
a.prob = 0.8 #normally goes through
b.prob = 0.2 #normally is blocked 

df.prediction = expand.grid(balls = balls, structure = structure) %>%
  mutate(a.prob  = a.prob,
         b.prob = b.prob,
         a.intervention = ifelse(balls == 0, a.prob + change, a.prob-change),
         b.intervention = ifelse(balls == 0, b.prob + change, b.prob-change),
         outcome = ifelse(balls == 0 | structure == 'xor', 0, 1)
  ) %>% 
  mutate_at(vars(contains('intervention')), funs(ifelse(. < 0, 0, .))) %>% 
  mutate_at(vars(contains('intervention')), funs(ifelse(. > 1, 1, .))) %>% 
  rowwise() %>%
  # correspondence model 
  mutate(a.prob.actual = ifelse(balls == 1, a.prob, (1 - a.prob)),
         b.prob.actual = ifelse(balls == 1, b.prob, (1 - b.prob)),
         p.outcome = func_outcome_probability(a.prob, b.prob, structure),
         p.outcome.actual = ifelse(outcome == 1, p.outcome,(1 - p.outcome)),
         a.correspondent = 1 - abs(p.outcome.actual - a.prob.actual),
         b.correspondent = 1 - abs(p.outcome.actual - b.prob.actual)
  ) %>% 
  # icard model 
  mutate(a.icard = func_icard(a.prob, b.prob, structure, balls),
         b.icard = func_icard(b.prob, a.prob, structure, balls)
  ) %>% 
  ungroup() %>% 
  mutate(
    balls = factor(balls, levels =  c(1, 0), labels = c('both balls go through', 'both balls are blocked')),
    outcome = factor(outcome, levels = c(0, 1), labels = c('negative', 'positive'))
  ) %>% 
  arrange(balls, structure)

# fit beta 
func_fit_beta = function(data, cause, alternative){
  
  # sum of squared error 
  func_sse = function(x){
    prediction = func_softmax(cause, alternative, beta = x[1])
    out = (data - prediction)^2
    return(sum(out))
  }
  
  # fite beta
  fit = optim(par = runif(1), fn = func_sse, method = "BFGS")
  
  return(fit)
}

# save betas in a separate data frame
model.names = c('abnormal selection', 'correspondence', 'necessity and sufficiency')

df.betas = tibble(model = model.names,
                  beta = NA)
df.betas$beta[1] = func_fit_beta(data = df.selections$percentage, cause = rep(c(1,0),each=3), alternative = rep(c(0,1),each=3))$par
df.betas$beta[2] = func_fit_beta(data = df.selections$percentage, cause = df.prediction$b.correspondent, alternative = df.prediction$a.correspondent)$par
df.betas$beta[3] = func_fit_beta(data = df.selections$percentage, cause = df.prediction$b.icard, alternative = df.prediction$a.icard)$par

# add predictions with fitted betas
df.prediction = df.prediction %>% 
  mutate(abnormal_selection = func_softmax(balls == 'both balls go through', balls != 'both balls go through', df.betas$beta[1]),
         correspondence = func_softmax(b.correspondent, a.correspondent, df.betas$beta[2]),
         icard = func_softmax(b.icard, a.icard, df.betas$beta[3])
  )

#+ Plot: Percentages ---------------------------------------------------------
#' # Plot: Percentages 

# function to get confidence intervals from an exact binomial test 
getIntervals = function(nsuccess, ntrials){return(binom.test(nsuccess,ntrials)$conf.int[1:2])}

# data frame with data 
df.plot = df.long %>%
  select(participant, outcome, balls, structure, choice) %>%
  mutate(choice = factor(choice,
                         levels = c('normal', 'abnormal'),
                         labels = c(' ball that normally goes through',
                                    ' ball that normally gets blocked')),
         response = choice %>% as.numeric()-1,
         structure = factor(structure,
                            levels = c('conjunctive', 'disjunctive', 'xor'),
                            labels = c('and', 'or', 'xor'))
  ) %>%
  count(outcome, balls, structure, choice) %>%
  group_by(outcome, balls, structure) %>%
  mutate(percentage = n/sum(n),
         groupsize = sum(n)) %>%
  group_by(outcome, balls, structure, choice) %>%
  mutate(ci_low = getIntervals(nsuccess = n, ntrials = groupsize)[1],
         ci_high = getIntervals(nsuccess = n, ntrials = groupsize)[2]) %>%
  mutate_at(vars(contains('ci_')), funs(ifelse(choice == ' ball that normally goes through', NA, .))) %>%
  ungroup()

# data frame with predictions
df.plot.prediction = df.plot %>%
  na.omit() %>%
  left_join(df.prediction %>% select(balls, structure, outcome,
                                     model.abnormal_selection = abnormal_selection,
                                     model.correspondence = correspondence,
                                     model.icard = icard
  )) %>%
  mutate(normal = ifelse(balls == 'both balls go through', 0, 1)) %>%
  select(balls, structure, outcome, contains('model')) %>%
  gather('index', 'value', -c(balls, structure, outcome)) %>%
  mutate(index = factor(index,
                        levels = c('model.abnormal_selection',
                                   'model.correspondence',
                                   'model.icard'
                        ),
                        labels = c('abnormal selection',
                                   'correspondence',
                                   'necessity and sufficiency'
                        )))

ggplot(df.plot, aes(x = structure, y = percentage))+
  geom_bar(stat= "identity", aes(fill = choice), color = "black")+
  geom_hline(yintercept = 0.5,linetype=2,size=1)+
  facet_grid(~balls)+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, color = "black", size = 1)+
  geom_point(data = df.plot.prediction, aes(y = value, group = index, color = index), position = position_dodge(0.9), size = 4)+
  scale_y_continuous(labels = percent_format())+
  scale_fill_manual(values = c(' ball that normally gets blocked' = "#0C67E0",
                               ' ball that normally goes through' = "#72E669"
  ))+
  scale_color_grey(start = 0.8, end = 0)+
  coord_cartesian(ylim = c(0, 1.01), x = c(0.5, 3.5), expand = F)+
  labs(y = "percentage selected", x = 'causal structure')+
  scale_x_discrete(labels = toupper(levels(df.plot$structure)))+
  theme_bw()+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = 'vertical',
        legend.title = element_blank(),
        panel.spacing.x = unit(1,"cm"),
        axis.title.x = element_text(margin = margin(t = 0.5, r = 0, b = 0.1, l = 0,"cm")),
        strip.background = element_blank())+
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2))

# ggsave("../../figures/results.pdf",width = 10,height = 6)



library(dplyr)
library(ggplot2)
library(car)

#Now being version controlled with Git
MainTable3 <- mutate(MainTable1, ORGID13 = ifelse(!is.na(ORGID13_new), ORGID13_new, ORGID13), ORGANIZATION13 = ifelse(!is.na(ORGANIZATION13_new), ORGANIZATION13_new, ORGANIZATION13))
OrgNameID <- MainTable3 %>% count(ORGANIZATION15, ORGID15)


# Renaming 2013 ORGIDs to match with 2015 ORGID formats for the same organizations 
# "Energy and Mines" is ORGID13 == "MEM" and ORGID15 == "EM"
# "International Trade" is ORGID13 == "IT" and ORGID15 == "MIT"
# "Office of the Premier" is ORGID13 =="PREM" and ORGID15 == "PO"
# "JTST" is ORGID13 == "JTST" and ORGID15 == "JTSTL"

MainTable3 <- MainTable3 %>% mutate(ORGID13_15format = ifelse(ORGID13 == "MEM", "EM", ifelse(ORGID13 == "IT", "MIT", 
  ifelse(ORGID13 == "PREM", "PO", ifelse(ORGID13 == "JTST", "JTSTL", ORGID13)))))

# So OIPC, which was included in 2013, was out of sample in 2015; how should they be treated? As 
 # outside of public service or just completely disregarded? 

# Now creating new migration variables for MainTable3 (treating OIPC as a regular organization here)
MainTable3 <- MainTable3 %>% mutate(NR15 = (ORGID15!=" " & is.na(EngState15_Mig)))
MainTable3 <- MainTable3 %>% mutate(NR13 = (ORGID13_15format!=" " & is.na(EngState13_Mig)))
MainTable3 <- MainTable3 %>% mutate(NPS15 = (ORGID15!=" " & ORGID13_15format==" "))
MainTable3 <- MainTable3 %>% mutate(LPS13 = (ORGID13_15format!=" " & ORGID15==" "))
MainTable3 <- MainTable3 %>% mutate(MWPS = (ORGID13_15format!=ORGID15 & ORGID13_15format!=" " & ORGID15!=" "))
MainTable3$EngState13 <- recode(MainTable3$EngState13_Mig,"NA=99") #recode to save future headache
MainTable3$EngState15 <- recode(MainTable3$EngState15_Mig,"NA=99") #recode to save future headache

# Output into a tally table
CountOutputTable1 <- MainTable3 %>% 
  group_by(ORGID13_15format, ORGID15, EngState13_Mig, EngState15_Mig) %>% tally %>% print(n=nrow(.))

# Convert the currently Integer engagement score variables into factor variables
CountOutputTable1$EngState13_Mig <- factor(CountOutputTable1$EngState13_Mig)
levels(CountOutputTable1$EngState13_Mig) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged")
CountOutputTable1$EngState15_Mig <- factor(CountOutputTable1$EngState15_Mig)
levels(CountOutputTable1$EngState15_Mig) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged")

# Just as an aside, creating a mean engagement score table here where NAs are excluded in the eng_state column
MeanEngagementScore <- MainTable3 %>% 
  filter(!is.na(ENG_STATE_15)) %>% group_by(ORGID15) %>% 
  summarise(n_obs = n(), MES = mean(ENG_STATE_15)) %>% arrange(MES) %>%print(n=nrow(.))

# Comparing 2013 engagement scores of those who moved within PS versus those who left altogether
MeanEngagementScoreMovers13 <- MainTable3 %>% 
  filter(!is.na(ENG_STATE_13), ORGID13_15format!=ORGID15) %>% group_by(ORGID13_15format, LPS13, MWPS) %>% 
  summarise(n_obs = n(), MES = mean(ENG_STATE_13)) %>% arrange(ORGID13_15format) %>%print(n=nrow(.))

# Logistic & Probit regression of leaving public service taking Eng_State_13 as a 
  # predictor variable and Org13 as control (intrepration of logit estimated coefficients is simpler)
LogitRegress1 <- glm(LPS13 ~ ENG_STATE_13 + ORGID13_15format, data=MainTable3, family="binomial")
summary(LogitRegress1)
ProbitRegress1 <- glm(LPS13 ~ ENG_STATE_13 + ORGID13_15format, family=binomial(link="probit"), data=MainTable3)
summary(ProbitRegress1)

  # exponentiating the coefficients and inerpreting them as odds-ratios using the logistic regression estimates
exp(coef(LogitRegress1))
exp(cbind(OR = coef(LogitRegress1), confint(LogitRegress1)))
  # based on the output from above code, it can be said that for a one unit increase in ENG_STATE_13 index score 
    #(higher meaning more disengaged), the odds of leaving public service as opposed to staying 
      #increase by a factor of 1.09 

# Create predicted probability of leaving public service 
LogitPredict <- MainTable3 %>%  select(ENG_STATE_13, ORGID13_15format) %>% filter (!is.na(ENG_STATE_13)) %>% mutate(ENG_STATE_13 = mean(ENG_STATE_13)) %>% group_by(ORGID13_15format, ENG_STATE_13) %>% summarise()
LogitPredict$ORGP <- predict(LogitRegress1, newdata = LogitPredict, type = "response")
    # in the output from above code, we can see what the probability of leaving public service is for  
      # each organization given an average level of engagement score.

# Want to create a table of predicted probabilities while varying engagement score, for each organization 
DistinctOrgs <- MainTable3 %>% distinct(ORGID13_15format) %>% filter(ORGID13_15format!=" ")
LogitPredict2 <- with(DistinctOrgs, data.frame(ENG_STATE_13 = rep(seq(from = 1, to = 6, length.out = 6), 29), ORGID13_15format = factor(rep((ORGID13_15format), each = 6))))
LogitPredict3 <- cbind(LogitPredict2, predict(LogitRegress1, newdata = LogitPredict2, type="link", se=TRUE))
LogitPredict3 <- within(LogitPredict3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))})


# plot the predicted probabilities of leaving public service by Organization
ggplot(LogitPredict3, aes(x = ENG_STATE_13, y = PredictedProb)) + 
  geom_line(aes(colour = ORGID13_15format), size=1) + ggtitle("Probability of Leaving Public Service") + 
  theme_bw() + xlab("Engagement Score") + ylab("Predicted Probability") + 
  scale_colour_hue(name="Organization ID")
 
# plot predicted probabilities of leaving at average engagement score
ggplot(LogitPredict, aes(x= ORGID13_15format, y= ORGP, fill = ORGID13_15format)) + geom_bar(stat="identity") +
  scale_fill_manual(values=topo.colors(29)) + guides (fill=FALSE) + ggtitle("Probability of Leaving Public Service \n at Mean Levels of Engagement") + 
  xlab("Organization") + theme(axis.title.y = element_blank())

# Turning "Wide" data (where time is delineated by different variables) into "Long" data (where there is a variable
 # for time called 'Year') --- Utilizing tidyr package for this, which has similar syntax as dplyr
long_MT3 <- MainTable3 %>% gather(Year, EngagementScore, EngState13:EngState15)
long_MT3 <- long_MT3 %>% mutate(Yr = ifelse(Year == "EngState13", "2013", "2015" ))

# Turning engagement score into factor variable and recoding to text labels
long_MT3$EngagementScore <- factor(long_MT3$EngagementScore)
levels(long_MT3$EngagementScore) <- c("Engaged", "Happily Detached", "Unhappily Dedicated", "Disengaged", "Non Respondents")

StateTablePSSG <- long_MT3 %>% filter(ORGID15=="PSSG" | ORGID13_15format=="PSSG") %>% 
  group_by(EngagementScore) %>% 
  summarize(Count2013 = sum(Yr=="2013" & ORGID13_15format=="PSSG" & !is.na(EngagementScore)), 
            Count2015 = sum(Yr=="2015" & ORGID15=="PSSG" & !is.na(EngagementScore))) %>% 
  mutate(NetGainOrLoss = Count2015 - Count2013) %>% 
  mutate(PercentageChange = ((Count2015-Count2013)/(Count2013))* 100)


# Testing new, random tables to put on the RMD file
MainTable3 %>% filter(ORGID15=="PSSG" | ORGID13_15format == "PSSG") %>% group_by(EngState15_Mig) %>%
  summarize(NR15 = sum(NR15=="TRUE" & ORGID15=="PSSG"),NR13 = sum(NR13=="TRUE" & ORGID13_15format=="PSSG"),
  NPS15 = sum(NPS15=="TRUE"), LPS13 = sum(LPS13=="TRUE"), MWPS15 = sum(MWPS=="TRUE" & ORGID15 == "PSSG"),
  MWPS13 = sum(MWPS=="TRUE" & ORGID13_15format=="PSSG"))

# Testing HTMLTable package
htmlTable(MatrixTest, 
          align = "c",
          cgroup=c("Commitment"),
          n.cgroup=c(2),
          header=c("<60",">=60"),
          rgroup=c("Satisfaction"),
          n.rgroup=c(2),
          rnames=c("<60", ">=60"),
          caption="Table n: Engagement State",
          css.cell = "padding-left: .5em; padding-right: .2em;"
          ) 

## Loop to generate State Tables for all ministries


for (i in unique(long_MT3$ORGID15))
{
  #assign(paste("StateTable",i,sep="")  
assign(paste(i), long_MT3 %>% filter(ORGID15==i | ORGID13_15format==i) %>% 
  group_by(EngagementScore) %>% 
  summarize(Count2013 = sum(Yr=="2013" & ORGID13_15format==i & !is.na(EngagementScore)), 
            Count2015 = sum(Yr=="2015" & ORGID15==i & !is.na(EngagementScore))) %>% 
  mutate(NetGainOrLoss = Count2015 - Count2013) %>% 
  mutate(PercentageChange = ((Count2015-Count2013)/(Count2013))* 100))
}



##
for (i in UniqueMinistries)
{
assign(paste("TestMatrix",i,sep=""), data.matrix(StateTable[i], rownames.force = NA)
       )  
  
}

##
for (i in UniqueMinistries)
{
rownames(StateTable[i]) <- 
  c("**Engaged**", "**Happily Detached**","**Unhappily Dedicated**",
    "**Disengaged**","**Non Respondents**")
}


# Trying to create rmarkdown loop for multiple reports
## Execute this code from here (the console, not RMarkdown) as it will have access to the global environment
load("//sfp.idir.bcgov/u152/NAJSAQIB$/R/MigrationAnalysis/Sept7.RData")
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(knitr)
for (id in unique(long_MT3$ORGID15))
{
  render("LoopTestSept8.Rmd",output_file = paste0('report', id, '.html'))
}

get(paste("StateTable",id,sep=""))

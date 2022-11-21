# load packages
library(tidyverse)
library(lme4)
library(lmerTest)

# read the data
antData <- read_csv("data/antData.csv")

# find area loss
antData$Loss <- antData$TotalArea - antData$AreaFinal

# find are lost as a percent
antData$PercLoss <- antData$Loss / antData$TotalArea

# standardize the percent loss by time
antData$PercLossStandard <- 60 * antData$PercLoss / antData$`Time(min)` 

# remove trials that were longer than 120 minutes
shortAntData <- 
  antData %>% 
  filter(`Time(min)` < 130)

# count how many of each group I have
spNestCount <- shortAntData %>% 
  group_by(Sp, Nest, Treatment) %>% 
  summarize(n=n())

# look at normalcy, distributions, and potential confounding variables
## histograms of perc loss
ggplot(data = shortAntData, aes(x = PercLossStandard)) +
  geom_histogram() +
  facet_grid(rows = vars(Sp), cols = vars(Nest))

# could initial leaf size be a confounding variable?
ggplot(data=shortAntData, mapping=aes(x=TotalArea, y=PercLossStandard)) +
  geom_point() # no it doesn't seem like it is
# could time of day be a confounding variable? 
ggplot(data=shortAntData, mapping=aes(x=StartTime, y=PercLossStandard)) +
  geom_point() # no it doesn't seem like it is

# basic descriptive stats on the data
shortAntDataSummary <- shortAntData %>% 
  group_by(Sp, Nest, Treatment) %>% 
  summarize(meanLoss = mean(PercLossStandard), sdLoss = sd(PercLossStandard))

# looking at the data another way
ggplot(data = subset(shortAntData, Nest %in% "Gym"), aes(x = Treatment, y = PercLossStandard, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Sp) +
  labs(title="Percentage lost of leaves left at the gym nest", y = "Percent loss in an hour")

ggplot(data = subset(shortAntData, Nest %in% "Power"), aes(x = Treatment, y = PercLossStandard, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Sp) +
  labs(title="Percentage lost of leaves left at the power station nest", y = "Percent loss in an hour")


  
ggplot(data = subset(shortAntData, Nest %in% "Gym"), aes(x = Treatment, y = PercLossStandard, fill = Treatment)) +
  geom_boxplot() +
  scale_y_continuous(trans = "pseudo_log") +
  facet_wrap(~Sp) 
  
ggplot(data = subset(shortAntData, Nest %in% "Power"), aes(x = Treatment, y = PercLossStandard, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Sp) +
  labs(title="Percentage lost of leaves left at the power station nest", y = "Percent loss in an hour")




# need to separate by age and treatment
shortAntData <- shortAntData %>% 
  mutate(Age = case_when(Treatment == "RR" | Treatment == "RU" ~ "Young", Treatment == "UU" | Treatment == "UR" ~ "Old"))

shortAntData <- shortAntData %>% 
  mutate(RollStatus = case_when(Treatment == "RR" | Treatment == "UR" ~ "Rolled", Treatment == "UU" | Treatment == "RU" ~ "Unrolled"))

# what are the ones where the perc loss was -?
negLoss <- shortAntData %>% filter(PercLossStandard < 0) # there are 6 where it is negative,

# list of all the largely negative values: Trial5YoungRolled (need to switch values for rows 19 and 20 in shortAntData)
## Trial13YoungUnrolled, Trial34OldUnrolled, Trial50OldROlled need to be removed because I don't see why they are wrong
### Which means removing rows 52, 112, and 117
shortAntData <- shortAntData %>%   filter(!row_number() %in% c(52, 112, 177))
### now to manually change rows 19 and 20 ###IDK HOW


# is my data zero-inflated
100*sum(shortAntData$PercLossStandard == 0)/nrow(shortAntData) # yes, 55% of the data are zeros

## zero inflated beta regression
library(gamlss)

# treatment as a factor
shortAntData$Treatment <- as.factor(shortAntData$Treatment)

# well sometimes I have negative values. but those are due to measurement errors. everything that is negative needs to be 0
shortAntData <- shortAntData %>% 
  mutate(PercLossStandardNoNeg = ifelse(PercLossStandard < 0, 0, PercLossStandard))

# it seems to really hate nas so I am going to make a simple table with only my variables of interest
herbForMod <- shortAntData %>% dplyr::select(Trial, Age, RollStatus, Treatment, PercLossStandard, PercLossStandardNoNeg, Nest, Date, Sp)

herbForMod %>% mutate(dataClass = case_when(PercLossStandardNoNeg == 1 ~ "one",
                                                          PercLossStandard == 0 ~ "zero",
                                                          PercLossStandardNoNeg != 0 & PercLossStandardNoNeg != 1 ~ "between")) %>% 
  group_by(dataClass, Nest, Sp) %>% 
  summarize(n())


m1 <- gamlss(PercLossStandardNoNeg ~ Age * RollStatus + Sp,  family = BEINF, data = herbForMod, trace = F)
summary(m1) # do i still need to have the random effects? 

# changing the one that is one to .9999
herbForMod <- herbForMod %>% 
  mutate(PercLossStandardAllGood = ifelse(PercLossStandardNoNeg == 1, .999, PercLossStandardNoNeg))

m2 <- gamlss(PercLossStandardAllGood ~ Age * RollStatus + Sp + Nest,  family = BEZI, data = herbForMod, trace = F)
summary(m2) # do i still need to have the random effects? 

m3 <- gamlss(PercLossStandardAllGood ~ Age * RollStatus + Nest,  family = BEZI, data = herbForMod[herbForMod$Sp == "scaber",], trace = F)
summary(m3) 

m4 <- gamlss(PercLossStandardAllGood ~ Age * RollStatus + Nest,  family = BEZI, data = herbForMod[herbForMod$Sp == "gig",], trace = F)
summary(m4) 

m5 <- gamlss(PercLossStandardAllGood ~ Age * RollStatus + Nest,  family = BEZI, data = herbForMod[herbForMod$Sp == "velu",], trace = F)
summary(m5) 


# viz for presentation

# New facet label names for sp variable
sp.labs <- c("R. gigante", "C. scaber", "H. velutina")
names(sp.labs) <- c("gig", "scaber", "velu")

nest.labs <- c("Nest 1", "Nest 2")
names(nest.labs) <- c("Gym", "Power")

# Create the plot

herbForMod %>% 
  ggplot(aes(y=PercLossStandardAllGood, x = Age, fill = RollStatus))  +
  geom_boxplot() +
  facet_wrap(~ Sp, 
               labeller = labeller(Sp = sp.labs, Nest = nest.labs)) +
  theme(panel.spacing = unit(0.5, "cm"), strip.text.x = element_text(face = "italic")) +
  scale_fill_discrete(labels=c('Rolled', 'Unrolled')) +
  ylab("Percentage of Leaf Eaten in 1 hr") +
  xlab("Leaf Age") +
  scale_fill_discrete(name = "Leaf Status")


# logisitic regression
herbForMod <- herbForMod %>% mutate(eatenPresence = case_when(PercLossStandardNoNeg <= 0.05 ~ 0,
                                            PercLossStandard > 0.05 ~ 1))

veluAntMod <- glmer(eatenPresence ~ RollStatus + Age + Nest + (1|Trial), family = binomial, data = herbForMod[herbForMod$Sp == "velu",])
summary(veluAntMod)  

scaberAntMod <- glmer(eatenPresence ~ RollStatus + Age + Nest + (1|Trial), family = binomial, data = herbForMod[herbForMod$Sp == "scaber",])
summary(scaberAntMod)  

gigAntMod <- glmer(eatenPresence ~ RollStatus + Age + Nest + (1|Trial), family = binomial, data = herbForMod[herbForMod$Sp == "gig",])
summary(gigAntMod)  

# what percentage of leaves are eaten vs not 
herbForMod %>% 
  group_by(RollStatus, eatenPresence) %>% 
  summarize(n())

# what percentage of leaf loss 
herbForMod %>% 
  group_by(RollStatus) %>% 
  summarize(mean(PercLossStandardAllGood))

             
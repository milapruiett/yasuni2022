# this is the cleaner version


# load dependencies
library("tidyverse")
library("gamlss")
library("lme4")

# read the data
standing <- read_csv("data/StandingHerbivoryDataMila.xlsx - Sheet1.csv")

# format existing columns
standing$LeafWidth <- as.numeric(standing$LeafWidth)
standing$LeafLength <- as.numeric(standing$LeafLength)
standing$UH <- as.numeric(standing$UH)

# calculate the leaf area
standing <- standing %>% mutate(LeafArea = pi * LeafWidth / 2 * LeafLength / 2)

# calculate total herbivory
standing <- standing %>% mutate(totalHerb = UH + RH)

# calculate herbivory percentage
standing <- standing %>% mutate(uhHerbPerc = UH / LeafArea) %>% 
  mutate(rhHerbPerc = RH / LeafArea) %>% 
  mutate(herbPerc = totalHerb / LeafArea)

# remove the NA and unknown species
standing <- standing %>% filter(Sp == "giga"| Sp == "scaber"| Sp == "velutia"| Sp == "stricta")

# creating presence / absence columns for all types of herbivory
standing <- standing %>% mutate(rhPresence = case_when(RH <= 0 ~ 0, RH > 0 ~ 1)) %>% 
  mutate(herbPresence = case_when(totalHerb <= 0 ~ 0, totalHerb > 0 ~ 1 )) %>% 
  mutate(uhPresence = case_when(totalHerb <= 0 ~ 0, totalHerb > 0 ~ 1 ))

# logistic regression for each species and each type of herbivory ? 

scaberStandingMod <- glmer(rhPresence ~ LeafNo + (1|ID), family = binomial, data = standing[standing$Sp == "scaber",])
summary(scaberStandingMod)  
 ### ?????



# on a given plant, how many leaves will have herbivory on average?
herbByPlant <- standing %>% 
  group_by(Sp, ID) %>% 
  summarize(numLeaves =n_distinct(LeafNo), 
            numLeavesWHerb = sum(herbPresence == 1), 
                                 numLeavesWRolledHerb = sum(rhPresence == 1), 
                                 numLeavesWUnrolledHerb = sum(uhPresence == 1)) %>% 
  mutate(plantPercHerb = numLeavesWHerb / numLeaves) %>% 
  mutate(plantPercHerb = if_else(plantPercHerb > 1, 1, plantPercHerb)) %>% 
  mutate(plantPercRolledHerb = numLeavesWRolledHerb / numLeaves) %>% 
  mutate(plantPercUnrolledHerb = numLeavesWUnrolledHerb / numLeaves)

herbByPlant %>% group_by(Sp) %>% 
  summarize(avgLeavesWHerb = mean(plantPercHerb, na.rm = T), 
            sdLeavesWHerb = sd(plantPercHerb, na.rm = T))

ggplot(herbByPlant, aes(Sp, plantPercHerb, fill = Sp)) +
  geom_boxplot() + 
  ylab("Percent of Leaves with Herbivory per Individual") +
  xlab("Species") +
  scale_fill_discrete(name = "Species", labels=c('R. gigante', 'C. scaber', "H. stricta", "H. velutia")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 


# long form of herb by plant 

# create long form data, where each row is a percent of herbivory
longFormPresenceAbsence <- herbByPlant %>% 
  dplyr::select("Sp", "ID", "plantPercRolledHerb", "plantPercUnrolledHerb") %>% 
  pivot_longer(cols = c(plantPercRolledHerb, plantPercUnrolledHerb), 
               names_to = "Rolled",
               values_to = "percentHerbPresence")

Sp.labs <- c("R. gigante", "C. scaber", "H. scaber", "H. velutina")
names(Sp.labs) <- c("giga", "scaber","stricta",  "velutia")

longFormPresenceAbsence %>% 
  ggplot(aes(Rolled, percentHerbPresence, fill = Rolled)) +
  geom_boxplot() +
  facet_wrap(~Sp) +
  scale_fill_discrete(name = "Herbivory Type", labels =c("Rolled", "Unrolled")) +
  xlab("") +
  facet_wrap(~ Sp, 
             labeller = labeller(Sp = Sp.labs)) +
  theme(strip.text.x = element_text(face = "italic")) +
  ylab("Percentage of Leaves with Herbivory")

###### NEED MRM to help w/ stats here

summary(lmer(data = longFormPresenceAbsence, percentHerbPresence ~ Rolled + Sp + (1|ID)))
lmer(lmer(family = logistic, data = longFormPresenceAbsence, percentHerbPresence ~ Rolled + Sp + (1|ID))))

# how much leaf area is lost due to herbivory on average
standingSummary <- standing %>% group_by(Sp) %>% 
  summarize(meanPercHerb = mean(herbPerc, na.rm = T), sdPercHerb = sd(herbPerc, na.rm = T))

ggplot(standing, aes(Sp, herbPerc, fill =Sp)) +
  geom_boxplot() +
  scale_y_continuous(trans = "pseudo_log") +
  ylab("Percent of Herbivory Per Leaf") +
  xlab("Species") +
  scale_fill_discrete(name = "Species", labels=c('R. gigante', 'C. scaber', "H. stricta", "H. velutia")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 


# does the amount of herbivory change depending on leaf age- is that different for rolled vs unrolled?

# create long form data, where each row is a percent of herbivory
herbivoryLongForm <- standing %>% 
  dplyr::select("Sp", "ID", "LeafNo", "uhHerbPerc", "rhHerbPerc") %>% 
  pivot_longer(cols = c(uhHerbPerc, rhHerbPerc), 
               names_to = "Rolled",
               values_to = "percentHerb")

herbivoryLongForm %>% 
  subset(LeafNo != "Terminal") %>% 
  ggplot(aes(LeafNo, percentHerb, fill = Rolled)) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Roll Status", labels =c("Rolled", "Unrolled")) +
  xlab("Leaf Number") +
  ylab("Percent Herbivory") +
  scale_y_continuous(trans = "pseudo_log") +
  facet_wrap(~Sp)

# i'm not sure if boxplot or scatterplot is correct

# create a data set for beta regression (remove the one where the percent is greater than 1)
dataForBetaRegStandingHerb <- herbivoryLongForm %>% filter(percentHerb < 1) %>% na.omit()

# count to see how zero-inflated it is
dataForBetaRegStandingHerb %>% mutate(dataClass = case_when(percentHerb == 1 ~ "one",
                                                            percentHerb == 0 ~ "zero",
                                                            percentHerb != 0 & percentHerb != 1 ~ "between")) %>% 
  group_by(dataClass) %>% 
  summarize(n())

dataForBetaRegStandingHerb$LeafNo <- as.numeric(dataForBetaRegStandingHerb$LeafNo) # should i treat leaf no as a numeric or cat variable? 

# run the zero-inflated beta regression, for each species
modHVel <- gamlss(percentHerb ~ Rolled + LeafNo,  family = BEZI, data = dataForBetaRegStandingHerb[dataForBetaRegStandingHerb$Sp == "velutia" , ], trace = F)
summary(modHVel)   

modHStr <- gamlss(percentHerb ~ LeafNo + Rolled,  family = BEZI, data = dataForBetaRegStandingHerb[dataForBetaRegStandingHerb$Sp == "stricta" , ], trace = F)
summary(modHStr)   

modCSca <- gamlss(percentHerb ~ LeafNo +  Rolled,  family = BEZI, data = dataForBetaRegStandingHerb[dataForBetaRegStandingHerb$Sp == "scaber" , ], trace = F)
summary(modCSca)   

modRgig <- gamlss(percentHerb ~ LeafNo + Rolled,  family = BEZI, data = dataForBetaRegStandingHerb[dataForBetaRegStandingHerb$Sp == "giga" , ], trace = F)
summary(modRgig)   

# a viz with the summary
longformSummary <- herbivoryLongForm %>% 
  group_by(Sp, Rolled, LeafNo) %>% 
  summarize(meanHerb = mean(percentHerb, na.rm = T), sdHerb = sd(percentHerb, na.rm = T))

longformSummary %>% 
  subset(LeafNo != "Terminal") %>% 
  ggplot(mapping = aes(y = meanHerb, x = LeafNo, fill = Rolled)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~Sp) +
  scale_fill_discrete(name = "Roll Status", labels =c("Rolled", "Unrolled")) +
  xlab("Leaf Number") +
  facet_wrap(~ Sp, 
             labeller = labeller(Sp = Sp.labs)) +
  theme(strip.text.x = element_text(face = "italic")) +
  ylab("Mean Percent Herbivory") 
# can add error bars with:  geom_errorbar(aes(ymin = meanHerb - sdHerb, ymax = meanHerb + sdHerb), position = position_dodge(width = 0.9))


# for Murdock presentation
herbivoryLongForm %>% 
  ggplot(aes(x = Rolled, y = percentHerb, fill = Rolled)) +
    geom_boxplot() +
  facet_wrap(~ Sp) +
  scale_fill_discrete(name = "Herbivory Type", labels =c("Rolled", "Unrolled")) +
  xlab("") +
  facet_wrap(~ Sp, 
             labeller = labeller(Sp = Sp.labs)) +
  theme(strip.text.x = element_text(face = "italic")) +
  scale_y_continuous(trans = 'log10') +
  ylab("Percent of Leaf Lost to Herbivory") 

foo <- standing %>% 
  subset( Sp == "giga" & (rhHerbPerc > 0 | uhHerbPerc > 0))
  
t.test(x = foo$rhHerbPerc, y = foo$uhHerbPerc, paired = T)


# question: are losses when leaf is rolled less than when leaf is unrolled in the first leaf?

herbivoryLongForm %>% 
  filter(Sp == "stricta" & percentHerb > 0 ) %>% 
  ggplot(aes(x = Rolled, y = percentHerb)) +
  geom_boxplot()

summary(glmer(percentHerb ~ Rolled + (1|ID/LeafNo), data = na.omit(herbivoryLongForm[herbivoryLongForm$Sp == "stricta" & herbivoryLongForm$percentHerb > 0, ])))
summary(gamlss(percentHerb ~ Rolled, random = ~1|ID/LeafNo, family = BEZI, data = na.omit(herbivoryLongForm[herbivoryLongForm$Sp == "stricta", ])))
summary(gamlss(percentHerb ~ Rolled, random = ~1|ID/LeafNo, family = BE, data = na.omit(herbivoryLongForm[herbivoryLongForm$Sp == "stricta" & herbivoryLongForm$percentHerb >0 , ])))


summary(lmer(percentHerb ~ Rolled + (1|ID), data = na.omit(herbivoryLongForm[herbivoryLongForm$Sp == "velutia", ])))
summary(gamlss(percentHerb ~ Rolled + (1|ID), family = BEZI, data = na.omit(herbivoryLongForm[herbivoryLongForm$Sp == "velutia", ])))



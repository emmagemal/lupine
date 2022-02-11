### Script for data analysis and figures 
### Assignment 3 - Using citizen science 
### EERCV at Stockholm University

### Library ----
library(plyr)
library(tidyverse)
library(lme4)

### Importing the data ----
# areas where lupine previously wasn't present but is now 
lupine <- read.csv("data/lupine_areas.csv", header = T)
fescue <- read.csv("data/Festuca_ovina_areas.csv", header = T)
strawberry <- read.csv("data/Fragaria_vesca_areas.csv", header = T)
stjohnswort <- read.csv("data/Hypericum_maculatum_areas.csv", header = T)
hawkbit <- read.csv("data/Leontodon_autumnalis_areas.csv", header = T)
woodrush <- read.csv("data/Luzula_multiflora_areas.csv", header = T)
cowwheat <- read.csv("data/Melampyrum_pratense_areas.csv", header = T)
smallcowwheat <- read.csv("data/Melampyrum_sylvaticum_areas.csv", header = T)
bramble <- read.csv("data/Rubus_saxatilis_areas.csv", header = T)
redclover <- read.csv("data/Trifolium_pratense_areas.csv", header = T)
whiteclover <- read.csv("data/Trifolium_repens_areas.csv", header = T)
bushvetch <- read.csv("data/Vicia_sepium_areas.csv", header = T)

# control areas where lupine is currently not present
fescue_con <- read.csv("data/Festuca_ovina_controls.csv", header = T)
strawberry_con <- read.csv("data/Fragaria_vesca_controls.csv", header = T)
stjohnswort_con <- read.csv("data/Hypericum_maculatum_controls.csv", header = T)
hawkbit_con <- read.csv("data/Leontodon_autumnalis_controls.csv", header = T)
woodrush_con <- read.csv("data/Luzula_multiflora_controls.csv", header = T)
cowwheat_con <- read.csv("data/Melampyrum_pratense_controls.csv", header = T)
smallcowwheat_con <- read.csv("data/Melampyrum_sylvaticum_controls.csv", header = T)
bramble_con <- read.csv("data/Rubus_saxatilis_controls.csv", header = T)
redclover_con <- read.csv("data/Trifolium_pratense_controls.csv", header = T)
whiteclover_con <- read.csv("data/Trifolium_repens_controls.csv", header = T)
bushvetch_con <- read.csv("data/Vicia_sepium_controls.csv", header = T)


### Data manipulation for 'over time' ----
# removing any NA rows  
strawberry <- strawberry[1:56, ]

# combining species that are subspecies or varieties 
unique(fescue$Artnamn)
unique(strawberry$Artnamn)
unique(stjohnswort$Artnamn)
unique(hawkbit$Artnamn)   # "Vanlig höstfibbla" "Höstfibbla"
unique(woodrush$Artnamn)   # "Säterfryle" "Vanlig ängsfryle" "Ängsfryle" 
unique(cowwheat$Artnamn)   # "Ängskovall" "Vanlig ängskovall"
unique(smallcowwheat$Artnamn)
unique(bramble$Artnamn)
unique(redclover$Artnamn)
unique(whiteclover$Artnamn)
unique(bushvetch$Artnamn)

hawkbit <- hawkbit %>% 
              mutate(Artnamn = case_when(Artnamn == "Vanlig höstfibbla" ~ "Höstfibbla",
                                         Artnamn == "Höstfibbla" ~ "Höstfibbla"))
woodrush <- woodrush %>% 
              mutate(Artnamn = case_when(Artnamn == "Säterfryle" ~ "Ängsfryle",
                                         Artnamn == "Vanlig ängsfryle" ~ "Ängsfryle",
                                         Artnamn == "Ängsfryle" ~ "Ängsfryle"))
cowwheat <- cowwheat %>% 
                mutate(Artnamn = case_when(Artnamn == "Vanlig ängskovall" ~ "Ängskovall",
                                           Artnamn == "Ängskovall" ~ "Ängskovall"))


# combining all the non-lupine species into 1 dataframe
natives <- rbind.fill(list(fescue, strawberry, stjohnswort, hawkbit, woodrush, cowwheat, 
                   smallcowwheat, bramble, redclover, whiteclover, bushvetch))

# extracting the relevant columns
str(natives)

natives <- natives %>% 
              dplyr::select(Artnamn, Ostkoordinat, Nordkoordinat, Startdatum, Slutdatum, Area)

lupine <- lupine %>% 
              dplyr::select(Ostkoordinat, Nordkoordinat, Startdatum, Slutdatum, Area)


# dividing the data into 'no lupine' (1994) and 'lupine' (≥1995)
# also dividing 'lupine' into older (1995-2014) and more recent lupine expansion (2015-2022)
natives_absent <- natives %>% 
                    filter(Slutdatum < 1995-01-01) %>% 
                    as.data.frame() %>% 
                    mutate(Area = as.factor(Area))
 
natives_present <- natives %>% 
                      filter(Slutdatum >= 1995-01-01) %>% 
                      as.data.frame() %>% 
                      mutate(Area = as.factor(Area))

natives_present_old <- natives_present %>% 
                          filter(Slutdatum < 2015-01-01) %>% 
                          as.data.frame() %>% 
                          mutate(Area = as.factor(Area))

natives_present_new <- natives_present %>% 
                          filter(Slutdatum >= 2015-01-01) %>% 
                          as.data.frame() %>% 
                          mutate(Area = as.factor(Area))


### Data manipulation for 'present day' ----
# obserbations found in 'control' sites, where no lupine is currently present 

# combining species that are subspecies or varieties 
unique(fescue_con$Artnamn)
unique(strawberry_con$Artnamn)
unique(stjohnswort_con$Artnamn)
unique(hawkbit_con$Artnamn)   # "Vanlig höstfibbla" "Höstfibbla"
unique(woodrush_con$Artnamn)   # "Säterfryle" "Vanlig ängsfryle" "Ängsfryle" 
unique(cowwheat_con$Artnamn)   
unique(smallcowwheat_con$Artnamn)
unique(bramble_con$Artnamn)
unique(redclover_con$Artnamn)
unique(whiteclover_con$Artnamn)
unique(bushvetch_con$Artnamn)

hawkbit_con <- hawkbit_con %>% 
                  mutate(Artnamn = case_when(Artnamn == "Vanlig höstfibbla" ~ "Höstfibbla",
                                             Artnamn == "Höstfibbla" ~ "Höstfibbla"))
woodrush_con <- woodrush_con %>% 
                    mutate(Artnamn = case_when(Artnamn == "Säterfryle" ~ "Ängsfryle",
                                               Artnamn == "Vanlig ängsfryle" ~ "Ängsfryle",
                                               Artnamn == "Ängsfryle" ~ "Ängsfryle"))

# combining all the non-lupine species into 1 dataframe
natives_con <- rbind.fill(list(fescue_con, strawberry_con, stjohnswort_con, hawkbit_con, 
                               woodrush_con, cowwheat_con, smallcowwheat_con, bramble_con, 
                               redclover_con, whiteclover_con, bushvetch_con))

# extracting the relevant columns
str(natives_con)

natives_con <- natives_con %>% 
                  dplyr::select(Artnamn, Ostkoordinat, Nordkoordinat, Startdatum, Slutdatum, Area)

# removing observations from before 1995
natives_con2 <- natives_con %>% 
                  filter(Slutdatum >= 1995-01-01) 


### Calculations ----
# counting abundance of lupine in each area (number of observations per area)
lupine <- lupine %>% 
              mutate(Year = substring(Slutdatum, 0, 4)) 

lupine_sum <- lupine %>% 
                group_by(Year) %>% 
                add_count(Area, name = "abundance")
lupine_sum2 <- lupine_sum %>% 
                  group_by(Area) %>% 
                  summarize(abundance = mean(abundance))
  
## Richness 'over time'
## Calculating species richness per area per year 
natives_rich <- natives %>% 
                  dplyr::select(Artnamn, Slutdatum, Area) %>% 
                  mutate(Year = substring(Slutdatum, 0, 4)) %>%
                  group_by(Area, Year) %>% 
                  mutate(richness = length(unique(Artnamn))) %>% 
                  summarize(Area = mean(Area),
                            richness = mean(richness))
natives_rich <- natives_rich %>% 
                  mutate(presence = case_when(Year < 1995 ~ "absent",
                                              Year >= 1995 ~ "present")) %>% 
                  mutate(Year = as.numeric(Year))

## Calculating species richness per area per period
rich_absent <- aggregate(data = natives_absent, Artnamn ~ Area, function(x) length(unique(x)))
rich_present <- aggregate(data = natives_present, Artnamn ~ Area, function(x) length(unique(x)))
rich_present_old <- aggregate(data = natives_present_old, Artnamn ~ Area, 
                              function(x) length(unique(x)))
rich_present_new <- aggregate(data = natives_present_new, Artnamn ~ Area, 
                              function(x) length(unique(x)))


# creating a combined dataframe for all richness
Area <- c("1", "2", "3", "4")
Artnamn <- c(0, 4, 0, 9)
rich_absent <- data.frame(Area, Artnamn)   # adding the other 2 area's to the dataframe 
rich_absent <- rich_absent %>% 
                  mutate(presence = "absent",   # adding info to aid in understanding
                         period = "past, no lupine",
                         years = "1950-1994") 

rich_present <- rich_present %>% 
                    mutate(presence = "present",
                           period = "current, lupine expansion",
                           years = "1995-2022") %>% 
                    mutate(Artnamn = as.numeric(Artnamn))

Artnamn <- c(0, 11, 0, 2)
rich_present_old <- data.frame(Area, Artnamn)
rich_present_old <- rich_present_old %>% 
                          mutate(presence = "present",
                                 period = "current, initial lupine expansion",
                                 years = "1995-2014")

rich_present_new <- rich_present_new %>% 
                        mutate(presence = "present",
                               period = "current distribution",
                               years = "2015-2022") %>% 
                        mutate(Artnamn = as.numeric(Artnamn))

richness_time <- rbind.fill(list(rich_absent, rich_present, rich_present_old, rich_present_new))

# making a richness_time object with only one kind of 'present' intervals 
richness_time_duo <- full_join(rich_absent, rich_present)
richness_time_trio <- rbind.fill(list(rich_absent, rich_present_old, rich_present_new))

# changing 'Artnamn' into 'richness'
richness_time_duo <- richness_time_duo %>% 
                        mutate(richness = Artnamn) %>% 
                        dplyr::select(-Artnamn)
richness_time_trio <- richness_time_trio %>% 
                          mutate(richness = Artnamn) %>% 
                          dplyr::select(-Artnamn)

# averaging richness across the plots for 
time_duo_sum <- richness_time_duo %>% 
                  group_by(presence) %>% 
                  summarize(richness = mean(richness))

time_trio_sum <- richness_time_trio %>% 
                    group_by(years) %>% 
                    summarize(richness = mean(richness)) %>% 
                    mutate(presence = case_when(years == "1950-1994" ~ "absent",
                                                years == "1995-2014" ~ "present",
                                                years == "2015-2022" ~ "present"))

## Richness 'presently' (control areas)
## Calculating richness per area per year
control_rich <- natives_con %>% 
                  dplyr::select(Artnamn, Slutdatum, Area) %>% 
                  mutate(Year = substring(Slutdatum, 0, 4)) %>%
                  group_by(Area, Year) %>% 
                  mutate(richness = length(unique(Artnamn))) %>% 
                  summarize(Area = Area,
                            richness = mean(richness))
control_rich <- control_rich %>% 
                  mutate(presence = "absent") %>% 
                  mutate(Year = as.numeric(Year))

# splitting control area into categorical years 
control2 <- control_rich %>% 
              mutate(group = case_when(Year < 1997 ~ "1950-1996",
                                       Year >= 1997 & Year < 2014 ~ "1997-2013",
                                       Year >= 2014 ~ "2014-2022")) 
control2 <- control2 %>% 
              group_by(group) %>% 
              summarize(richness = mean(richness))

ggplot(control2, aes(x = group, y = richness)) +
  geom_bar(stat = "identity")

# combining control with present data
natives_rich_present <- natives_rich %>% 
                          filter(Year >= 1995) %>% 
                          mutate(Year = as.numeric(Year))

control_present <- full_join(control_rich, natives_rich_present)


## Richness per area for the full time period 
rich_control <- aggregate(data = natives_con2, Artnamn ~ Area, function(x) length(unique(x)))

str(rich_control)
rich_control <- rich_control %>% 
                  mutate(Area = as.character(Area)) %>% 
                  mutate(presence = "absent",
                         period = "current, no lupine",
                         years = "1995-2022")


# combing with present time data (from richness_time)
richness_control <- full_join(rich_present, rich_control)

# removing one of the 'rich_present' rows to make it 3 areas per grouping 
richness_control <- richness_control[-2, ]
richness_control[2, 2] <- 10.5

# renaming 'Artnamn' to 'richness'
richness_control <- richness_control %>% 
                        mutate(richness = Artnamn) %>% 
                        dplyr::select(-Artnamn)

# summarizing the data
control_sum <- richness_control %>% 
                  group_by(presence) %>% 
                  summarize(richness = mean(richness))


## Combining lupine observations with native richness per year 
lupine_short <- lupine_sum %>% 
                  dplyr::select(Year, Area, abundance) %>% 
                  mutate(Year = as.numeric(Year))
lupine_natives <- full_join(lupine_short, natives_rich)

lupine_natives$abundance[is.na(lupine_natives$abundance)] <- 0


## Number observations of everything over time
obs <- full_join(natives, natives_con)
obs <- obs %>% 
          dplyr::select(Artnamn, Slutdatum, Area) %>% 
          mutate(Year = substring(Slutdatum, 0, 4)) 

lupine2 <- lupine %>% 
            mutate(Artnamn = "lupine") %>% 
            dplyr::select(Artnamn, Slutdatum, Area) %>% 
            mutate(Year = substring(Slutdatum, 0, 4)) 
lupine2 <- lupine2[, c("Artnamn", "Slutdatum", "Area", "Year")]

obs <- full_join(lupine2, obs) 

obs <- obs %>% 
          group_by(Year) %>% 
          add_count(Artnamn, name = "observations") %>% 
          summarize(observations = sum(observations))
obs <- obs %>% 
          mutate(Year = as.numeric(Year))


### Plotting effects of lupine on species richness ----
# general theme for the plots
themelup <- theme_bw() +
              theme(panel.grid = element_blank(),
                    axis.title.x = 
                      element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                    axis.title.y = 
                      element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    legend.position = "none") +
              theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

(ab_pres <- ggplot(time_duo_sum, aes(x = presence, y = richness, fill = presence)) +
              geom_bar(stat = "identity") +
              ylab("Species richness") +
              xlab("Lupine presence") + 
              themelup +
              scale_y_continuous(limits = c(0,12), expand = c(0, 0)) +
              scale_fill_manual(values = c("#C2BBF0", "#E9842C")) +
              scale_x_discrete(labels = c("Absent", "Present")))
    # when lupine is absent, richness is lower 
  
ggsave(plot = ab_pres, filename = "figures/past_pres.png", width = 4, height = 4, units = "in")

# using a more split up 'present'
(ab_pres_trio <- ggplot(time_trio_sum, aes(x = years, y = richness, fill = presence)) +
                  geom_bar(stat = "identity") +
                  ylab("Species richness") +
                  xlab("Time period") +
                  themelup + 
                  theme(legend.position = "right") +
                  scale_y_continuous(limits = c(0,12), expand = c(0, 0)) +
                  scale_fill_manual(values = c("#C2BBF0", "#E9842C"),
                                    labels = c("Absent", "Present"),
                                    name = "Lupine presence"))
    # richness has mainly increased over the last 7 years 

ggsave(plot = ab_pres_trio, filename = "figures/past_pres_trio.png", 
       width = 5.5, height = 4, units = "in")


# comparing to control areas 
(con_pres <- ggplot(control_sum, aes(x = presence, y = richness, fill = presence)) +
                geom_bar(stat = "identity") +
                ylab("Species richness") +
                xlab("Lupine presence") + 
                themelup +
                scale_y_continuous(limits = c(0,12), expand = c(0, 0)) +
                scale_fill_manual(values = c("#574187", "#E8A94B")) +
                scale_x_discrete(labels = c("Absent", "Present")))
  # not as much of a difference here, they're more similar (not as much of an effect of lupine)
  # could mean lupine isn't the driving variable in our time difference

ggsave(plot = con_pres, filename = "figures/con_pres.png", 
       width = 4, height = 4, units = "in")

# looking at change per year
(ab_time <- ggplot(natives_rich, aes(x = Year, y = richness)) +   # over time
              geom_smooth(method = "lm", color = "grey", alpha = 0.2) +
              geom_point(aes(color = presence)) +
              ylab("Species Richness") +
              themelup +
              scale_color_manual(values = c("#C2BBF0", "#E9842C")))

ggsave(plot = ab_time, filename = "figures/past_pres_time.png", 
       width = 5, height = 4, units = "in")

(con_time <- ggplot(control_present, aes(x = Year, y = richness)) +   # control 
                geom_smooth(method = "lm", aes(color = presence, fill = presence)) +
                geom_point(aes(color = presence)) +
                ylim(0, 12) +
                ylab("Species Richness") +
                themelup +
                scale_color_manual(values = c("#574187", "#E8A94B")) +
                scale_fill_manual(values = c("#574187", "#E8A94B")))

ggsave(plot = con_time, filename = "figures/con_pres_time.png", 
       width = 6, height = 4.5, units = "in")

# looking at effect of lupine observations 
(outlier <- ggplot(lupine_natives, aes(x = abundance, y = richness)) +   # over time
              geom_smooth(method = "lm", color = "#9FB798") +
              geom_point(color = "#799B6F") +
              xlab("Lupine abundance") +
              ylab("Species Richness") +
              themelup)

ggsave(plot = outlier, filename = "figures/ab_rich_outlier.png", 
       width = 5, height = 4, units = "in")

lupine_natives2 <- lupine_natives %>% 
                      filter(abundance < 1000)

(ab_rich <- ggplot(lupine_natives2, aes(x = abundance, y = richness)) +   # over time
              geom_smooth(method = "loess", color = "#799B6F") + 
              geom_point(color = "#799B6F") +
              xlab("Lupine abundance") +
              ylab("Species Richness") +
              themelup)

ggsave(plot = ab_rich, filename = "figures/ab_rich.png", 
       width = 5, height = 4, units = "in")

# lupine abundance over time
lupine_short2 <- lupine_short %>% 
                    filter(abundance < 1000)

(lup_ab <- ggplot(lupine_short2, aes(x = Year, y = abundance)) +
              geom_point(color = "#799B6F") +
              ylim(0, 90) +
              ylab("Lupine abundance") +
              themelup)

ggsave(plot = lup_ab, filename = "figures/lupine_abund.png", 
       width = 5, height = 4, units = "in")

# observations over time
(obs_outlier <- ggplot(obs, aes(x = Year, y = observations)) +
                  geom_point(color = "#424931") +
                  ylab("Observations") +
                  themelup)

ggsave(plot = obs_outlier, filename = "figures/obs_outlier.png", 
       width = 5, height = 4, units = "in")

obs2 <- obs %>% 
          filter(observations < 1000)
(obs_time <- ggplot(obs2, aes(x = Year, y = observations)) +
                geom_smooth(method = "lm", color = "#656E49") +
                geom_point(color = "#424931") +
                ylab("Observations") +
                themelup)

ggsave(plot = obs_time, filename = "figures/observations.png", 
       width = 5, height = 4, units = "in")


### Statistics ----
## T-tests
# t-test for past vs. present (absent vs. present)
richness_time_duo <- richness_time_duo %>% 
                        mutate(presence = as.factor(presence))
str(richness_time_duo)

t.test(richness ~ presence, richness_time_duo)
  # p = 0.03819, t = -3.487, DF = 3.08
  # significant difference 

# t-test for absent vs. present (control vs. lupines present)
t.test(richness ~ presence, richness_control)
  # p = 0.1012, t = -2.1213, DF = 4
  # NOT significantly different 


## Linear models
# effect of lupine abundance on richness
abund_lm <- lm(richness ~ abundance, data = lupine_natives2)
summary(abund_lm)  # significant, p = 0.101
abund_poly <- lm()

# effect of lupine presence on richness (past vs. present)
time_lm <- lm(richness ~ Year, data = natives_rich)
summary(time_lm)   

# effect of lupine presence on richness (control vs. not)
control_lm <- lm(richness ~ Year + presence, data = control_present)
control_lm2 <- lmer(richness ~ Year + (1|presence), data = control_present)
summary(control_lm)  # significant 
summary(control_lm2)  # significant decline
                      # presence of lupine explains some variance (~42% of it)
library(car)
Anova(control_lm2)

# observations over time
obs_lm <- lm(observations ~ Year, data = obs2)
summary(obs_lm)
 

########## NO LONGER USING (nmds) ##########
## NMDS to compare communities 
## Past vs. present communities 
natives <- natives %>% 
              mutate(presence = case_when(Slutdatum < 1995-01-01 ~ "absent",
                                          Slutdatum >= 1995-01-01 ~ "present"))
natives <- natives %>% 
              mutate(groupings = case_when(Area == "1" & presence == "absent" ~ "absent1",
                                           Area == "2" & presence == "absent" ~ "absent2",
                                           Area == "3" & presence == "absent" ~ "absent3",
                                           Area == "4" & presence == "absent" ~ "absent4",
                                           Area == "1" & presence == "present" ~ "present1",
                                           Area == "2" & presence == "present" ~ "present2",
                                           Area == "3" & presence == "present" ~ "present3",
                                           Area == "4" & presence == "present" ~ "present4")) %>%
              group_by(groupings) %>% 
              add_count(Artnamn, name = "abundance")

natives_short <- natives %>% 
                    dplyr::select(Artnamn, groupings, abundance) %>% 
                    mutate(abundance = as.numeric(abundance)) %>% 
                    pivot_wider(names_from = Artnamn,
                                values_from = abundance,
                                values_fn = mean)

natives_short[is.na(natives_short)] <- 0   # converting NA's to 0

# making 'groupings' to row names
natives_short <- data.frame(natives_short, row.names = "groupings")  
natives_short <- as.matrix(natives_short)   # making it a matrix 

str(natives_short)

# doing the ordination (distance matrix)
time_NMDS <- metaMDS(natives_short, distance = "bray", k = 2)
# no convergence, not enough data 

biplot(pcoa(vegdist(natives_short, "bray")))


## Control vs not
natives_con <- natives_con %>% 
                  mutate(presence = "absent") %>% 
                  mutate(Area = as.factor(Area))
natives_present <- natives_present %>% 
                      mutate(presence = "present")
# combining 'control' and 'present'
control_combo <- full_join(natives_con, natives_present)

control_combo <- control_combo %>% 
                  mutate(groupings = case_when(Area == "1" & presence == "absent" ~ "absent1",
                                               Area == "2" & presence == "absent" ~ "absent2",
                                               Area == "3" & presence == "absent" ~ "absent3",
                                               Area == "4" & presence == "absent" ~ "absent4",
                                               Area == "1" & presence == "present" ~ "present1",
                                               Area == "2" & presence == "present" ~ "present2",
                                               Area == "3" & presence == "present" ~ "present3",
                                               Area == "4" & presence == "present" ~ "present4")) %>%
                  group_by(groupings) %>% 
                  add_count(Artnamn, name = "abundance")

combo_short <- control_combo %>% 
                  dplyr::select(Artnamn, presence, groupings, abundance) %>% 
                  mutate(abundance = as.numeric(abundance)) %>% 
                  pivot_wider(names_from = Artnamn,
                              values_from = abundance,
                              values_fn = mean)

combo_short[is.na(combo_short)] <- 0   

combo_short <- data.frame(combo_short, row.names = "groupings")  
combo_short <- as.matrix(combo_short)   
combo_short <- combo_short[, -1]
combo_short <- matrix(as.numeric(1:11))

# doing the ordination (distance matrix)
control_NMDS <- metaMDS(combo_short, distance = "bray", k = 2)
# no convergence, not enough data 

biplot(pcoa(vegdist(natives_short, "bray")))

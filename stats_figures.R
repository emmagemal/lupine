### Script for data analysis and figures 
### Assignment 3 - Using citizen science 
### EERCV at Stockholm University

### Library ----
library(plyr)
library(tidyverse)

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
                add_count(Area, name = "Abundance")
lupine_sum <- lupine_sum %>% 
                group_by(Area) %>% 
                summarize(Abundance = mean(Abundance))
  
## Richness 'over time'
# calculating native species richness per area per period
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

# renaming 'Artnamn' to 'richness'
richness_control <- richness_control %>% 
                        mutate(richness = Artnamn) %>% 
                        dplyr::select(-Artnamn)

# summarizing the data
control_sum <- richness_control %>% 
                  group_by(presence) %>% 
                  summarize(richness = mean(Artnamn))


### Plotting effects of lupine on species richness ----
ggplot(time_duo_sum, aes(x = presence, y = richness, fill = presence)) +
  geom_bar(stat = "identity")
  # when lupine is absent, richness is lower 

ggplot(richness_time_duo, aes(x = presence, y = richness, fill = presence)) +
  geom_boxplot()
  
# using a more split up 'present'
ggplot(time_trio_sum, aes(x = years, y = richness, fill = presence)) +
  geom_bar(stat = "identity")
  # richness has mainly increased over the last 7 years 

ggplot(richness_time_trio, aes(x = years, y = richness, fill = presence)) +
  geom_boxplot()
  

# comparing to control areas 
ggplot(control_sum, aes(x = presence, y = richness, fill = presence)) +
  geom_bar(stat = "identity")
  # not as much of a difference here, they're more similar (not as much of an effect of lupine)
  # could mean lupine isn't the driving variable in our time difference


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

## NMDS to compare communities 
library(vegan)
# past vs. present communities 
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
              mutate(abundance = )

taxa <- unique(natives$Artnamn) #creates list of each unique species
samples <- sort(unique(natives$groupings)) #creates list of each unique site or sample

#make empty matrix ready to fill in
matrix1 <- matrix(nrow = length(samples), ncol = length(taxa), dimnames = list(samples, taxa))

for(r in 1:nrow(LL.data)){
  samp <- LL.data[r, 8]
  tax <- LL.data[r, 1]
  matrix1[samp,tax] <- LL.data[r, 3]
} # 1, 2, 3 here relate the the column number in the raw data in which the sample name, 
# species name and data are in

matrix1[is.na(matrix1)] <- 0   #convert NA's to 0

time_NMDS <- metaMDS(natives, k = 2, trymax = 100)

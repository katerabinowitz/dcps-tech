library(tidyverse)
library(stringr)
library(ggbeeswarm)
library(here)
setwd("/Users/katerabinowitz/Documents/DataLensDCOrg/dcps/techInventory")

### Read in data ###
### Read in data ###
### Read in data ###
dcpsTechRaw <-  read.csv(here("tech_inventory.csv"), strip.white = TRUE, stringsAsFactors = FALSE)[c(1:13)]
dcpsEnrollRaw <- read.csv(here("enrollmentAuditByGrade.csv"), strip.white = TRUE, stringsAsFactors = FALSE) %>% 
                 filter(LEA.Name %in% c("District of Columbia Public Schools") & Enrolled > 100)
dcpsAtRiskRaw <- read.csv(here("dpcs_budget_atrisk.csv"), strip.white = TRUE, stringsAsFactors = FALSE) %>% 
              filter(YEAR == "2018")
dcpsStaffRaw <- read.csv(here("FOIA list FY 2018.csv"), strip.white = TRUE, stringsAsFactors = FALSE)

# output dell tech that lacks shipping data
# use serial number to pull shipping data from dell website
# dell <- dcpsTech %>% filter(is.na(Shipping.Date)) %>% 
                      # filter(Asset.Grouped %in% c("DESKTOP", "LAPTOP") & Manufacturer == "DELL")
# write.csv(dell, "dellServiceTags.csv", row.names=FALSE)

dell <- read.csv("dellServiceTagWithDates.csv", strip.white = TRUE, stringsAsFactors = FALSE)
dell <- dell %>% distinct(Service.Tag, .keep_all = TRUE) %>% 
                 rename(Serial..=Service.Tag) %>%
                 select(Serial.., System.Ship.Date)

dcpsTech <- left_join(dcpsTechRaw, dell, by="Serial..") %>% 
                        mutate(Dell.Shipping.Date = ifelse(Shipping.Date == "", System.Ship.Date, Shipping.Date),
                                Dell.Shipping.Date = as.Date(Dell.Shipping.Date, "%m/%d/%Y"),
                                Location.Name = tolower(Location.Name),
                                Asset.Grouped = ifelse(grepl("LAPTOP", Asset.Type), "LAPTOP", Asset.Type),
                                School.Name = gsub(" es", " elementary school", Location.Name),
                                School.Name = gsub(" ms", " middle school", School.Name),
                                School.Name = gsub(" hs", " high school", School.Name),
                                School.Name = gsub(" ec", " education campus", School.Name),
                                School.Name = ifelse(School.Name %in% c("bilingual school-oyster campus", "bilingual school- adams campus"),
                                                     "oyster-adams bilingual school", School.Name),
                               
                                Level = ifelse(grepl(" es", Location.Name), "elementary", 
                                          ifelse(grepl(" ms| middle school", Location.Name), "middle",
                                            ifelse(grepl(" hs", Location.Name), "high", 
                                              ifelse(grepl(" ec", Location.Name), "education campus",
                                                "other")))),
                               
                               School.Name2 = gsub(" middle school| high school| elementary school| education campus", "", School.Name),
                               School.Name2 = gsub(" @ francis-stevens", " francis-stevens", School.Name2),
                               School.Name2 = gsub("\\,.*|@.*|\\(.*| alternative|h\\.|d\\. ", "", School.Name2),
                               School.Name2 = trimws(School.Name2),
                               
                               mod_or_newYr = case_when(# new schools
                                                        grepl("macfarland", School.Name2) ~ "2016",
                                                        grepl("brookland", School.Name2) ~"2015",
                                                        grepl("river terrace", School.Name2) ~ "2015",
                                                        grepl("van ness", School.Name2) ~ "2015",
                                                        grepl("ron brown", School.Name2) ~ "2016",
                                                        # full modernizations
                                                        grepl("ballou", School.Name2) ~ "2015",
                                                        grepl("cardozo", School.Name2) ~ "2013",
                                                        grepl("cleveland", School.Name2) ~ "2015",
                                                        grepl("deal", School.Name2) ~ "2014",
                                                        grepl("dunbar", School.Name2) ~ "2013",
                                                        grepl("ellington", School.Name2) ~ "2017",
                                                        grepl("hearst", School.Name2) ~ "2015",
                                                        grepl("lafayette", School.Name2) ~ "2015",
                                                        grepl("mann", School.Name2) ~ "2015",
                                                        grepl("marie reed", School.Name2) ~ "2017",
                                                        grepl("mckinley", School.Name2) ~ "2014",
                                                        grepl("payne", School.Name2) ~ "2016",
                                                        grepl("powell", School.Name2) ~ "2016",
                                                        grepl("roosevelt", School.Name2) ~ "2017",
                                                        grepl("stanton", School.Name2) ~ "2016",
                                                        grepl("stuart-hobson", School.Name2) ~ "2015",
                                                        grepl("turner", School.Name2) ~ "2013",
                                                        grepl("watkins", School.Name2) ~ "2017"),
                               
                               Physical.Room.Number = toupper(Physical.Room.Number),
                               classroom = ifelse(grepl("OFFICE|STORAGE|PRINCIPAL|MANAGER|TEACHER|LOUNGE|CLOSET|CLINC|BOILER|DESK|NURSE|KITCHEN|CUSTODIA|PSYCHOLOGIST|SOCIAL WORKER|VAULT|SUPPLY ROOM|SUITE",
                                                        Physical.Room.Number), 0, 1),
                               over3 = ifelse(Dell.Shipping.Date < as.Date("2015-01-01"), 1, 0),
                               over4 = ifelse(Dell.Shipping.Date < as.Date("2014-01-01"), 1, 0)) %>%
                          select(-System.Ship.Date) %>%
                              # remove choice and non-schools
                          filter(!(School.Name2 %in% c("c.o.i.c.e. academy", "warehouse", "youth services center")))


dcpsEnroll <- dcpsEnrollRaw  %>%
  mutate(PARC.Enrolled = X03 + X04 + X05 + X06 + X07 + X08 + X09 + X10 + X11 + X12) %>% 
  mutate(School.Name = tolower(School.Name)) %>%
  mutate(School.Name2 = gsub(" middle school| high school| elementary school| education campus", "",School.Name),
         School.Name2 = gsub(" @ francis-stevens", " francis-stevens", School.Name2),
         School.Name2 = gsub("\\,.*|@.*|\\(.*", "", School.Name2), 
         School.Name2 = gsub("benjamin |duke | college preparatory|woodrow | i\\.| c\\.|h\\.|d\\.", "", School.Name2),
         School.Name2 = trimws(School.Name2),
         Enrolled.612 = X06 + X07 + X08 + X09 + X10 + X11 + X12) %>%
  select(School.Name2, Enrolled, PARC.Enrolled, Enrolled.612)

dcpsAtRisk <- dcpsAtRiskRaw %>% mutate(School.Name2 = tolower(SCHOOLNAME),
                                    School.Name2 = gsub(" es| ms| hs| ec| education campus|benjamin | middle school|college preparatory high school| sec|h\\.|d\\.| alternative", "", School.Name2),
                                    School.Name2 = gsub(" @ francis-stevens", " francis-stevens", School.Name2),
                                    School.Name2 = gsub("\\,.*|@.*|\\(.*", "", School.Name2), 
                                    School.Name2 = gsub("cap hill montessori", "capitol hill montessori school", School.Name2),
                                    School.Name2 = gsub("oyster-adams bilingual", "oyster-adams bilingual school", School.Name2),
                                    School.Name2 = gsub(" ace", " architecture", School.Name2),
                                    School.Name2 = trimws(School.Name2)) %>%
                             select(School.Name2, ATRISKPCT, LEVELTEXT)

dcpsStaff <- dcpsStaffRaw %>% rename(School.Name2 = Department.Name) %>%
                              mutate(School.Name2 = tolower(School.Name2),
                                     School.Name2 = gsub(" es| ms| hs| ec| shs| benjamin | jhs|college prep|h\\.|d\\.| shs", "", School.Name2),
                                     School.Name2 = ifelse(School.Name2 == "amidon", "amidon-bowen",
                                                      ifelse(School.Name2 == "capitol hill montessori@logan",  "capitol hill montessori school",
                                                        ifelse(School.Name2 == "duke ellington school of arts", "ellington school of the arts",
                                                          ifelse(School.Name2 == "eliot", "eliot-hine",
                                                            ifelse(School.Name2 == "hyde", "hyde-addison", 
                                                              ifelse(School.Name2 == "king, ml", "king", 
                                                                ifelse(School.Name2 == "lasalle", "lasalle-backus",
                                                                  ifelse(School.Name2 == "luke c. moore academy", "luke moore",
                                                                    ifelse(School.Name2 == "oyster - adams billingual s", "oyster-adams bilingual school",
                                                                      ifelse(School.Name2 == "phelps", "phelps architecture",
                                                                        ifelse(School.Name2 == "reed marie lc", "marie reed",
                                                                          ifelse(School.Name2 == "ronald brown", "ron brown", 
                                                                            ifelse(School.Name2 == "school w/out walls", "school without walls",
                                                                              ifelse(School.Name2 == "school within school @ goding", "school-within-school",
                                                                                ifelse(School.Name2 == "stuart hobson", "stuart-hobson",
                                                                                  ifelse(School.Name2 == "webb - wheatley", "wheatley",
                                                                                    ifelse(School.Name2 == "wilson j.o.", "j.o. wilson",
                                                                                      ifelse(School.Name2 == "harris c.w.", "c.w. harris",
                                                                                        ifelse(School.Name2 == "francis stevens ec", "school without walls francis-stevens",
                                                                                           School.Name2))))))))))))))))))),
                                     School.Name2 = trimws(School.Name2)) %>%
                              group_by(School.Name2) %>% summarise(staffN = n()) 
  



dcps1 <- inner_join(dcpsTech, dcpsEnroll, by="School.Name2")
dcps2 <- left_join(dcps1, dcpsAtRisk, by="School.Name2")
dcps <- left_join(dcps2, dcpsStaff, by="School.Name2") %>% mutate(staffStudentN = staffN + Enrolled) %>%
                                                           filter(LEVELTEXT %in% c("Elementary", "Middle",
                                                                                   "High", "Education Campus"))


### Equipment coverage ###
### Equipment coverage ###
### Equipment coverage ###

equipSum <- function(assetEquip, classEquip) {
  dcps %>% filter(Asset.Grouped == assetEquip) %>% 
            group_by(School.Name, LEVELTEXT) %>% 
            summarise(count = n(), pop = mean(staffStudentN), 
                     enrolled = mean(Enrolled), atRisk = mean(ATRISKPCT)) %>%
            mutate(prop = pop / count, studentProp = enrolled / count, class=classEquip) %>% 
            arrange(prop) %>%
    filter(count > 1)
}

ipad <- equipSum("TABLET/IPAD", "ipad")
laptop <- equipSum("LAPTOP", "laptop")
desktop <- equipSum("DESKTOP", "desktop")
whiteboard <- equipSum("WHITEBOARD (INTERACTIVE)", "whiteboard")
projector <- equipSum("PROJECTOR (LCD)", "projector")


ggplot(laptop, aes(class, prop, color=factor(LEVELTEXT))) + 
  geom_quasirandom(varwidth = TRUE) + coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggplot(desktop, aes(class, prop, color=factor(LEVELTEXT))) + 
  geom_quasirandom(varwidth = TRUE) + coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggplot(ipad, aes(class, prop, color=factor(LEVELTEXT))) + 
  geom_quasirandom(varwidth = TRUE) + coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggplot(whiteboard, aes(class, prop, color=factor(LEVELTEXT))) + 
  geom_quasirandom(varwidth = TRUE) + coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())

ggplot(projector, aes(class, prop, color=factor(LEVELTEXT))) + 
  geom_quasirandom(varwidth = TRUE) + coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank())


### Age of equipment ###
### Age of equipment ###
### Age of equipment ###
dcps <- dcps %>% mutate(year = format(as.Date(Dell.Shipping.Date, format="%d/%m/%Y"),"%Y"))

shipSum <- dcps %>% filter(Asset.Grouped %in% c("LAPTOP")) %>% 
                  filter(!(is.na(Dell.Shipping.Date))) %>% 
                  group_by(School.Name, Asset.Grouped) %>%
                  summarise(minShip = min(Dell.Shipping.Date),
                            medShip = median(Dell.Shipping.Date),
                            maxShip = max(Dell.Shipping.Date),
                            atRisk = mean(ATRISKPCT)) %>%
                  arrange(medShip) %>%
                  mutate(year = as.numeric(format(as.Date(medShip, format="%d/%m/%Y"),"%Y")))

availableLaptop <- dcps %>% filter(Asset.Grouped %in% c("LAPTOP")) %>% 
                      select(School.Name, Dell.Shipping.Date) %>%
                      group_by(School.Name) %>%
                      summarise_all(funs(sum(is.na(.))/length(.)))

desktopSum <- dcps %>% filter(Asset.Grouped %in% c("DESKTOP")) %>% 
  filter(!(is.na(Dell.Shipping.Date))) %>% 
  group_by(School.Name, Asset.Grouped) %>%
  summarise(minShip = min(Dell.Shipping.Date),
            medShip = median(Dell.Shipping.Date),
            maxShip = max(Dell.Shipping.Date),
            atRisk = mean(ATRISKPCT)) %>%
  arrange(medShip) %>%
  mutate(year = as.numeric(format(as.Date(medShip, format="%d/%m/%Y"),"%Y")))

availableDesktop <- dcps %>% filter(Asset.Grouped %in% c("DESKTOP")) %>% 
  select(School.Name, Dell.Shipping.Date) %>%
  group_by(School.Name) %>%
  summarise_all(funs(sum(is.na(.))/length(.)))

dcps %>% filter(Asset.Grouped %in% c("LAPTOP")) %>% filter(!(is.na(Dell.Shipping.Date))) %>% 
                 mutate(pre2015 = ifelse(year < 2015, 1, 0)) %>% 
                 group_by(pre2015) %>% summarise(count=n())

dcps %>% filter(Asset.Grouped %in% c("DESKTOP")) %>% filter(!(is.na(Dell.Shipping.Date))) %>% 
  mutate(pre2015 = ifelse(year < 2015, 1, 0)) %>% 
  group_by(pre2015) %>% summarise(count=n())

dcps %>% filter(Asset.Grouped %in% c("LAPTOP")) %>% filter(!(is.na(Dell.Shipping.Date))) %>% 
  filter(year < 2011) %>%
  group_by(School.Name) %>% summarise(pre15=n()) %>%
  right_join(laptop, by="School.Name") %>% 
  mutate(propL = pre15 / count) %>% select(School.Name, propL) %>% arrange(desc(propL))

dcps %>% filter(Asset.Grouped %in% c("DESKTOP")) %>% filter(!(is.na(Dell.Shipping.Date))) %>% 
  filter(year < 2011) %>%
  group_by(School.Name) %>% summarise(pre15=n()) %>%
  right_join(desktop, by="School.Name") %>% 
  mutate(propL = pre15 / count) %>% select(School.Name, propL) %>% arrange(desc(propL))

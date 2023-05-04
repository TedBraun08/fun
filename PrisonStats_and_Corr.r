
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Data from US Bureau of Justice Statistics for this section
#https://csat.bjs.ojp.gov/map-query
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(maps)
library(tidyverse)
#library(gifski)
#library(av)
library(gganimate)
library(stringr)
library(corrplot)

LclDir <- "C:/various/kojinteki/tjb/DesertVista_Class/2023_DesertVista_Programming_Class/Prison_Statistics"
setwd(LclDir)

#ExcludeVals <- c("FEDERAL", "DISTRICT OF COLUMBIA")
ExcludeVals <- c("FEDERAL", "ALASKA", "HAWAII")

PData <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(skip = 1, show_col_types = FALSE, .)) %>% 
  filter(!STATE %in% ExcludeVals) %>% 
  mutate(IncarPer100k = ifelse(STATE == "DISTRICT OF COLUMBIA", 0, VALUE)) %>% 
  mutate(StateLC = tolower(STATE))

vecStates <- sort(unique(pull(PData, STATE)))
vecYears <- sort(unique(pull(PData, YEAR)))
states_map <- map_data("state")

str(states_map)
str(PData)
#PDataMap2 <- merge(states_map, PData, by.x = "region", by.y = "StateLC")
PDataMap <- inner_join(states_map, PData, by = c("region" = "StateLC"))

for (aYear in vecYears) {
  aYearOf_PDataMap <- PDataMap %>% 
    filter(YEAR == aYear)
  aYearFileName <- paste0("aMap_", aYear, '.png')
  strTitle <- paste0("Year: ", aYear)
  #str(aYearOf_PDataMap)
  print(aYearFileName)
  p1 <- ggplot(aYearOf_PDataMap, aes(x = long, y = lat, group = group, fill = IncarPer100k)) +
    geom_polygon(colour = "black") +
    coord_map("polyconic") + 
    ggtitle(strTitle) +
    theme(plot.title = element_text(size=16, face="bold"),
          axis.title.y = element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), 
          axis.title.x = element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  ggsave(aYearFileName, plot=p1, width = 9, height = 7)
}

# Thread .png's together via ImageMagick and create gif and mp4 files 
#LclDir2 <- sub("//", "/",LclDir)  #Replace the double quotes with single quotes
#setwd(LclDir)
#Issue ImageMagick 'convert' command (non-R) to "paste" the .png files together
system('"C:\\Program Files\\ImageMagick-7.1.0-Q16-HDRI\\convert" -delay 50 *.png PrisonAdmitPer100k_02.gif')
system('"C:\\Program Files\\ImageMagick-7.1.0-Q16-HDRI\\convert" -delay 50 *.png PrisonAdmitPer100k_02.mp4')


PData$State <- str_to_title(PData$StateLC)  #Capitialize first characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Now create a moving bar chart with the same data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

KeepStates <- c("ARIZONA","ARKANSAS", "CALIFORNIA", "KENTUCKY", "LOUISIANA",
                "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "NEW YORK", "NORTH DAKOTA", "SOUTH DAKOTA", "TEXAS", "VERMONT")
PDataFilt <- PData %>%  
  filter(STATE %in% KeepStates) %>% 
  mutate(IncarPer100k = round(IncarPer100k,0),
         Values = round(VALUE,0)) %>% 
  arrange(YEAR, State) 
#PDataFilt$Index <- cumsum(c(1,diff(PDataFilt$YEAR)!=0))
#PDataFilt <- PDataFilt %>% 
#  select(Index, State, IncarPer100k, YEAR, Values)  

#Create multiple plots at once
a <- ggplot(PDataFilt, aes(x = State, y = IncarPer100k, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Year: {closest_state}") +
  #geom_text(aes(label = Values, y = IncarPer100k),
  #          position = position_dodge(0.9), vjust = -1 ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(size=22, face = "bold")) +
  transition_states(states = YEAR, transition_length = 2, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('cubic-in-out')
  #ease_aes('sine-in-out')

#Animate all of them together and save to a file
a_gif <- animate(a, width = 1100, height = 700, nframes = 150, fps=4)
anim_save("States_12.gif", a_gif)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Now read in US Census Bureau's Census of Governments and its associated annual survey state data.
#  Gathered via Urban Institute as is no-bueno getting from US Census Bureau website (one year at a time data)
#  https://state-local-finance-data.taxpolicycenter.org/pages.cfm
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LclDir <- paste0(LclDir, "/SupportInfo")
setwd(LclDir)

USCensus_State_Data_File <- 'Govt_State_Data_Bonanza_all_in_2020_Dollars.csv'

USCensus_State_Data_DirFile <- paste0(LclDir, "/", USCensus_State_Data_File)

str(USCensus_State_Data_DirFile)
CensusStateData <- read_csv(USCensus_State_Data_DirFile, skip = 2, show_col_types = FALSE) %>% 
#CensusStateData <- CensusStateData %>% 
  rename(LiqStoresRev = 3,
         EducExp = 4,
         EducAssit = 6,
         PubWelfExp = 10) %>% 
  select(State, Year, LiqStoresRev, EducExp, EducAssit, PubWelfExp) %>% 
  mutate(Year = str_sub(Year,1,4),
         LiqStoresRev = str_replace_all(LiqStoresRev, " ", ""),
         EducExp = str_replace_all(EducExp, " ", ""),
         EducAssit = str_replace_all(EducAssit, " ", ""),
         PubWelfExp = str_replace_all(PubWelfExp, " ", "")) %>% 
  filter(Year != "vari") %>% 
  mutate( across(
    .cols = everything(),
    ~str_replace( ., "\\$", "" )
  ) ) %>%  
  mutate_at(c('LiqStoresRev', 'EducExp', 'EducAssit', 'PubWelfExp'), as.numeric)
 
str(CensusStateData)
write_csv(CensusStateData, "CensusStateData.csv")


PData$YEAR <- as.character(PData$YEAR)

Together <- inner_join(CensusStateData, PData, by = c("State" = "State", "Year" = "YEAR"))
Together <- Together %>% 
  rename(InCarcerations = IncarPer100k) %>% 
  filter(!is.na(LiqStoresRev)) %>% 
  filter(!is.na(EducExp)) %>% 
  filter(!is.na(EducAssit)) %>% 
  filter(!is.na(PubWelfExp)) %>% 
  filter(!is.na(InCarcerations)) %>% 
  select(-STATE, -VALUE, -StateLC, -State)

#Together[Together == 0] <- NA

vecYears <- sort(unique(pull(Together, Year)))

LclDir <- paste0(LclDir, "/CorrPlots")
setwd(LclDir)

str(Together)
for (aYear in vecYears) {
  Tog <- Together %>% 
    filter(Year == aYear) %>% 
    select(-Year)
  aDirFile <- paste0(aYear, '_cp.png')
  aTitle <- aYear
  png(height=900, width=900, file=aDirFile, type = "cairo") #title=aTitle,
  corrplot(cor(Tog), method="circle",  mar=c(0,0,5,0), tl.offset = 1)
  mtext(aTitle, at=2.5, line=-0.5, cex=2)
  dev.off()
}

system('"C:\\Program Files\\ImageMagick-7.1.0-Q16-HDRI\\convert" -delay 50 *.png CorrelationMatrix_01.gif')
system('"C:\\Program Files\\ImageMagick-7.1.0-Q16-HDRI\\convert" -delay 50 *.png CorrelationMatrix_01.mp4')



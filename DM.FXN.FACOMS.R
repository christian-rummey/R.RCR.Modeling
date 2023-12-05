# require -----------------------------------------------------------------
# fxn.1 is the less organized sheet (althhough it may have more data)
# fxn.1 <- read_excel("DATA/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN Blood Data 2018", na=c("?","X")) %>% 
#   rename(
#     sjid    = 'FACOMS',
#     cdt     = "Date of Collection"
#   ) %>% 
#   mutate(cdt = as.Date(as.numeric(cdt), origin = as.Date('1899-12-30'))) %>% 
#   select(sjid, cdt, "Loaded", "FXN mABS", "GAM mABS", "FXN Corrected", "% of Average ctl", "Average...10")
# require 2 ---------------------------------------------------------------
# Only looking at patients from FA-COMS - to compare clinical data. 
# using Average of % of average control (for plate differences) 
# I am using only frataxin columns, although there are more patients with data in here. 

rm(list=ls())

types <- c('text','text','date','text','text','text','text','text','text','text',
           'text','text','text','text','text','text','text','text','text','text',
           'text','text','text','text','text','text','text')

fxn.blood   <- readxl::read_excel("DATA/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN Blood Data 2018") %>% 
  rename(
    sjid    = 'FACOMS',
    cdt     = "Date of Collection"
  )


fxn.bucc   <- readxl::read_excel("DATA/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN BC Data 2018", na=c("?","X"),
                             col_types = types) %>% 
  rename(
    sjid    = 'FACOMS ID',
    cdt     = "Date of Collection"
  )


# AVERAGING of values needs to happen before LOG!! ------------------------

fxn.bucc %<>% 
  mutate(Status = ifelse(Status == 'control','Control', Status)) %>% 
  # filter((sjid %in% demo.$sjid) | (Status != 'Patient')) %>% 
  select(Status, sjid, cdt, "ug/ul", "FXN mABS", "GAM mABS", "FXN Corrected", "% of Average Ctl", "Average") %>%
  rename(ug.lu    = `ug/ul`,
         fxn.abs  = `FXN mABS`, 
         gam.abs  = `GAM mABS`,
         fxn.corr = `FXN Corrected`,
         fxn.actl = `% of Average Ctl`) %>% 
  # rename(fxn = fxn.corr) %>% # NOV 2023
  rename(fxn = fxn.actl) %>% 
  select(-Average) %>% 
  mutate(cdt = as.Date(cdt)) %>% 
  mutate_at(c('fxn'), as.numeric) %>% 
  filter(!is.na(fxn)) %>% 
  # mutate(fxn.l = log10(fxn)) %>% 
  # filter(is.na(fxn.l))
  select(Status, sjid, cdt, fxn)

filter(fxn.bucc, is.na(fxn))

fxn.bucc %<>% 
  # filter(sjid == 20) %>%
  # filter(is.na(cdt))
  # filter(sjid == 6007) %>% 
  group_by(Status, sjid) %>%
  # mutate ( earliest = min(cdt, na.rm=T), latest = max(cdt, na.rm=T)) %>% 
  # mutate ( d.range = as.numeric( latest - earliest )) %>% 
  # group_by( Status, sjid, earliest, d.range ) %>%
  group_by( sjid, Status ) %>%
  select(-cdt) %>% 
  summarise(
    # n     = n(),
    fxn   = mean(fxn  , na.rm=T),
  ) %>% 
  mutate(fxn.l = log10(fxn))

fxn.bucc %<>% filter(n()==1)

fxn.bucc %<>% 
  left_join(.dd('demo'))
# %>% 
#   mutate(age = as.numeric(earliest-birthdt)/365.25)

filter(fxn.bucc, sjid == 4853 )


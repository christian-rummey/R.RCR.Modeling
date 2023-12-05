
# provides current HTz classificaion

# final classification ----------------------------------------------------

htz.class <- read_excel('DATA/2023-12-04 Heterozygous Classification.xlsx') %>% 
  filter(!is.na(pm)) %>%
  filter(!(pm %in% c('NON CHOP','pm'))) %>%
  # print(n=50)
  select(pm, pm.grp, type, sjid) %>% 
  separate_rows(sjid, sep = ",\\s*")

htz.class %>% 
  select(-sjid) %>% 
  unique %>% 
  arrange(pm) %>% 
  print(n=28)
  
  


# read_excels -------------------------------------------------------------

# CHOP.class.0 <- read_excel('DATA/2023-10-26 FA-PM_import.xlsx') %>%
#   select(sjid = `FACOMS ID`, pm.excel = `Pt Mut`, pm.grp = Mut, type = Type) %>%
#   mutate(sjid = as.character(sjid)) %>%
#   mutate(pm.grp = case_when(
#     pm.grp == '0' ~ 'homozygous',
#     pm.grp == '1' ~ 'missense',
#     pm.grp == '2' ~ 'null',
#     TRUE ~ 'other'
#   )) %>%
#   mutate(type = case_when(
#     type == '1' ~ 'deletion',
#     type == '2' ~ 'start codon or early mutation',
#     type == '3' ~ 'destabilizing (L106S, L165P)',
#     type == '4' ~ 'G130V',
#     type == '5' ~ 'other missense',
#     TRUE ~ 'other'
#   ))

# CHOP.class <- read_excel('DATA/2023-12-04 FA-PM_import.xlsx') %>%
#   select(sjid = `FACOMS ID`, pm.excel = `Pt Mut`, pm.grp = Mut, type = Type) %>%
#   mutate(sjid = as.character(sjid)) %>% 
#   mutate(pm.grp = case_when(
#     pm.grp == '0' ~ 'homozygous',
#     pm.grp == '1' ~ 'missense',
#     pm.grp == '2' ~ 'null',
#     TRUE ~ 'unknown'
#   )) %>% 
#   mutate(type = case_when(
#     type == '1' ~ 'deletion',
#     type == '2' ~ 'start codon or early mutation',
#     type == '3' ~ 'destabilizing (L106S, L165P)',
#     type == '4' ~ 'G130V',
#     type == '5' ~ 'other missense',
#     TRUE ~ 'unknown'
#   ))


# # with(CHOP.class.0, table(pm.grp, type))
# with(CHOP.class  , table(pm.grp, type))
# 
# CHOP.class %>% 
#   group_by(pm.excel, pm.grp, type) %>% 
#   mutate(n = n()) %>% 
#   group_by(pm.excel, pm.grp, type, n) %>%
#   arrange(sjid) %>% 
#   summarise_all( ~toString(na.omit(.)) ) %>% 
#   arrange(pm.grp, type, -n) %>% 
#   .ct
# 
# dt.demo. <- .dd('demo') %>% 
#   select(study, sjid, site, aoo, gaa1, gaa2, pm) %>% 
#   filter(study == 'FACOMS') %>% 
#   left_join(CHOP.class)
# 
# # 26 pms from other sites undefined ---------------------------------------
# 
# dt.demo. %>% 
#   filter(pm!=0) %>% 
#   filter(!(pm=='' & is.na(gaa1))) %>% #21 that are waiting or unknown
#   # filter(!(sjid %in% c(4060, 4778,4906,4908,4964,4994,5041,5165,5184,5191,5236,5376,5440,5468,5480,5509,5513,81,82,83))) %>% #print(n=30) %>% 
#   filter(is.na(pm.grp)) %>%
#   filter(site!='CHOP') %>% 
#   select(-c(pm.grp, pm.excel, type) ) %>% 
#   left_join(
#     CHOP.class %>% select(-sjid) %>% rename(pm = pm.excel) %>% unique
#   ) %>% 
#   select(sjid, site, pm, pm.grp, type) %>% 
#   group_by(pm, pm.grp, type) %>% 
#   mutate(n = n()) %>% 
#   group_by(sjid, pm, pm.grp, type, n) %>%
#   arrange(sjid) %>% 
#   group_by(pm, pm.grp, type, n) %>%
#   count(site) %>% 
#   mutate(site = paste(site, "(", nn, ")", sep = "")) %>% select(-nn) %>% 
#   summarise_all( ~toString(na.omit(.)) ) %>% 
#   arrange(pm.grp, type, -n) %>% 
#   .ct
# 

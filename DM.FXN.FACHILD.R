
# fxn <- readRDS('DATA derived/fxn.rds') %>% 
#   mutate(pms = factor(ifelse(pm == 0, 0, 1)))

# .mycol <- scale_color_brewer(palette = 'Set1')
# .mycol <- scale_color_manual(values = c(RColorBrewer::brewer.pal(3, 'Set1'), 'darkgrey'))

fxn.fachild <- .rt('DATA/fxn.txt') %>%
  # filter(grepl('C', sjid)) %>%
  # select(status, pm) %>% table
  droplevels()

# fxn.fachild %>% 
#   group_by(sjid) %>% arrange(sjid) %>% 
#   filter(n()>1)

fxn.1 <- fxn.fachild %>% 
  # filter(status == 'control') %>% 
  mutate(study = 'FACHILD') %>% 
  mutate(gaa1 = ifelse(is.na(gaa1), gaa2, gaa1)) %>% 
  select(-gaa2) %>% 
  # filter(!is.na(gaa1)) %>% 
  mutate(
    mature.l = log10(mature), 
    isoform.e.l = log(isoform.e),
    total.l = log(total),
  ) %>% 
  mutate(mature.l = log10(mature), isoform.e.l = log(isoform.e)) %>% 
  rename(aoo = symp) %>% 
  # mutate(aoo = log(aoo)) %>% 
  gather(type, value, mature, mature.l, isoform.e, isoform.e.l, total, total.l) %>% 
  select(study, status, sjid, type, value)

fxn.2 <- fxn.bucc %>% ungroup %>% 
  # mutate(aoo = log(aoo)) %>% 
  gather(type, value, fxn, fxn.l) %>% 
  mutate(study = 'FACOMS') %>% 
  select(study, status = Status, sjid, type, value)

fxn <- bind_rows(fxn.1, fxn.2)

fxn %<>% 
  left_join( .dd('demo') %>% select(sjid, sex, aoo, diag, gaa1, pm) ) 

rm(fxn.1, fxn.2)

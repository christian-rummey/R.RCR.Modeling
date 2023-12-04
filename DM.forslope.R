
# FARS E & mFARS ----------------------------------------------------------

fars. <- .dd('fars', c = T) %>% 
  filter(paramcd %in% c('FARS.E','mFARS','FARS.B')) %>% 
  left_join(.dd('steps', c = T) %>% select(study, sjid, avisitn, amb)) %>% 
  .duplicate_phase_visits() %>% 
  group_by(study, sjid) %>% 
  ungroup

fars. %<>%
  group_by(study, sjid, paramcd) %>% 
  mutate( time. = as.numeric( adt-min(adt) ) / 365.25 ) %>% 
  select(-c(avisit, avisitn, hpf, fpf)) %>% ungroup %>%  
  left_join(.dd('demo') %>% select(study, sjid, site, sev.o, aoo, gaa1, pm)) %>% 
  mutate(gaa100 = gaa1/100)

bl <- fars. %>% 
  group_by(study, sjid, paramcd) %>% 
  filter(time. == min(time.)) %>% 
  select(study, sjid, bl = aval)

fars. %<>%
  left_join(bl) %>% 
  select(c(study, site, sjid, sev.o, aoo, gaa100, pm, adt, time., paramcd, bl, aval, amb, phase.n, dupline))

fars. %>% 
  saveRDS('DATA derived/fars.forslope.rds')

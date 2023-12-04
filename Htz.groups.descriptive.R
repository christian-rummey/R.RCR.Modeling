# lets model --------------------------------------------------------------
require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)

.lme4.ctrl      <- lmerControl( 
  optimizer = "optimx", 
  calc.derivs = FALSE, 
  optCtrl = list(method = "L-BFGS-B" , 
                 starttests = FALSE, 
                 kkt = FALSE, 
                 # maxiter = 10000, 
                 save.failures = T)) # def iterations 500
# dataset ----------------------------------------------------------------------

rm(list=ls())

fars. <- readRDS('DATA derived/fars.forslope.rds')

fars. %<>% 
  filter   ( phase.n == 1 ) %>%
  select   ( -amb, -dupline, -phase.n) %>%
  # filter   ( paramcd == 'FARS.E') %>%
  filter   ( study == 'FACOMS' ) %>%
  droplevels ()

fars. %<>% 
  group_by( study, sjid, paramcd ) %>%
  filter  ( n()> 1 )

# fars. %>% 
#   filter(site == 'CHOP') %>%
#   filter(sjid != 4988) %>%
#   filter(sjid != 4327) %>%
#   lmer(aval ~ bl + pm.grp:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
#   broom::tidy() %>% .fixmod()

slopes.descr.phase.1 <- fars. %>% 
  mutate( obs = n() ) %>% 
  group_by(study, sjid, paramcd, obs, pm.grp) %>%
  do ( tidy ( lm (aval ~ time., data = . ))) %>%
  filter(term == 'time.') %>% 
  select(study, sjid, paramcd, slope = estimate, pm.grp)

# fars. %>% 
#   group_by ( study, sjid, paramcd ) %>% filter( n()>1 ) %>% ungroup

# fars. %>%
  # filter(sjid == 4841) %>%
  # filter(sjid == 4988) %>%
  # # filter(sjid == 11) %>%
  # filter(site == 'CHOP') %>%
  # ungroup()

with(fars., table(pm, pm.grp))

lmers <- fars. %>%
  group_by( study, paramcd ) %>% 
  nest() %>% 
  mutate( mod.rates = map( data, ~ lmer(aval ~ bl + pm.grp:time.         + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) ) %>% 
  mutate( mod.diffs = map( data, ~ lmer(aval ~ bl + pm.grp:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) )%>%
  select( -data )

lme  
  

lmer(aval ~ bl + pm.grp:time. + (1 + time.|sjid) , control = .lme4.ctrl, 
                     data = fars. %>% filter(paramcd == 'FARS.E') 
                     ) %>% 
  broom::tidy   ( effects = "ran_vals" ) %>% 
  filter(term!='(Intercept)') %>% 
  # rename(sjid = level) %>% 
  select ( sjid = level, mod.slope = estimate )


slopes.descr.phase.1 %>% 
  left_join(.dd('demo.l', c =T) %>% select(study, sjid, site)) %>% 
  mutate(site = ifelse(site == 'CHOP', 'CHOP', 'other' )) %>% 
  # filter(paramcd == 'FARS.E', study == 'FACOMS') %>% 
  left_join ( slopes.model ) %>% 
  mutate(obs.grp = cut(obs, c(0,2,3,10,20)))

dt.tmp %>% 
  ggplot()+geom_point()+
  aes( y = mod.slope, x = slope)+
  aes(color = obs.grp)+
  aes(shape = site)+.ssmA+
  geom_abline(slope = 1)+
  geom_smooth(method = 'lm', aes(group = '1')) +
  geom_text(aes(label = sjid), data = dt.tmp %>% filter(slope>5))+
  facet_wrap(pm.grp~obs.grp, nrow = 3)



slopes.descr.phase.1 %>%
  filter(sjid == 4327)
  filter(paramcd == 'FARS.E', slope > 5)
  # filter(paramcd == 'FARS.E', study == 'FACOMS.CHOP') %>% 
  # filter(!(sjid %in% c(4988,4327))) %>%
  # filter(slope > 5)
  group_by(paramcd, pm.grp) %>% 
  summarise(mean(slope))

fars. %>%
  filter(sjid == 4327) %>% 
  filter(pm=='G130V') %>% filter(paramcd == 'FARS.E') %>% 
  # left_join(slopes.descr.phase.1) %>% 
  # filter(time. > 0, time. < .740) %>% 
  # filter(slope > 10) %>% 
  ggplot()+geom_point()+
  aes( y = aval, x = time.)+
  aes(color = pm.grp)+
  geom_smooth(method = 'lm', se=F) +
  facet_wrap(paramcd~sjid)
  

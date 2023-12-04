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
# rd ----------------------------------------------------------------------

rm(list=ls())

fars. <- readRDS('DATA derived/fars.forslope.rds')

fars. %<>% 
  filter   ( phase.n == 1 ) %>%
  # filter   ( amb == 'ambulatory' ) %>%
  # filter   ( dupline == F ) %>% 
  select   ( -amb, -dupline, -phase.n) %>%
  droplevels ()

with(fars., table(pm, pm.grp))

# limit dataset ONLY here

fars. %<>% 
  group_by ( study, sjid, paramcd ) %>% filter( n()>2 ) %>% ungroup %>% 
  filter(study == 'FACOMS')

fars. %<>%
  bind_rows(
    fars. %>% filter(site == 'CHOP') %>% mutate( study = paste0(study, '.CHOP') )
    ) %>% 
  # filter ( sjid != 4988 ) %>%
  # filter ( sjid != 4327) %>%
  # # filter(sjid == 11) %>%
  # filter(site == 'CHOP') %>%
  ungroup()

# . -----------------------------------------------------------------------

# I could not use pm as primary, as average avals are probably very similar. That could drain 
# effects from interaction term.  
# including time. or not changes the way results are presented (p.value for change or p-value for interaction)

models. <- fars. %>%
  group_by( study, paramcd ) %>% 
  nest() %>% 
  mutate( model = 'pm') %>% 
  mutate( mod.rates = map( data, ~ lmer(aval ~ bl + gaa100 + pm.grp:time.         + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) ) %>% 
  mutate( mod.diffs = map( data, ~ lmer(aval ~ bl + gaa100 + pm.grp:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) )%>%
  select( -data )

# . -----------------------------------------------------------------------

# rates <- models. %>%  
#   mutate( slp.rates = map( mod.rates, ~ tidy   ( .x, effects = "fixed", conf.int = T))) %>%
#   # select( -c(mod.rates, mod.diffs) ) %>% 
#   unnest( slp.rates ) %>% 
#   mutate(  estimate.rate =  ( sprintf('%0.1f', estimate) ) ) %>%
#   mutate( CI.rate = paste0(' (', sprintf('%0.1f', conf.low), ', ', sprintf('%0.1f', conf.high), ')') ) %>% 
#   select( -c(std.error, statistic, df, conf.low, conf.high, estimate )) %>% 
#   .fixmod() %>% 
#   rename(p.rate = p.value)
# 
# result <- rates %>% 
#   left_join(
#     models. %>%  
#       mutate( slp.diffs = map( mod.diffs, ~ tidy   ( .x, effects = "fixed", conf.int = T))) %>%
#       select( -c(mod.diffs, mod.rates) ) %>% 
#       unnest( slp.diffs ) %>% 
#       mutate(  estimate.diff =  ( sprintf('%0.1f', estimate) ) ) %>% 
#       mutate( CI.diff = paste0(' (', sprintf('%0.1f', conf.low), ', ', sprintf('%0.1f', conf.high), ')') ) %>% 
#       select( -c(std.error, statistic, df, conf.low, conf.high )) %>% 
#       .fixmod()%>% 
#       rename(p.diff = p.value)
#     )
# 
# result %>% 
#   select(study, paramcd, term, estimate.rate, CI.rate, p.rate, estimate.diff, CI.diff, p.diff) %>% 
#   filter(!(term %in% c('(Intercept)','bl'))) %>% 
#   mutate( term = gsub('pm.grp','',term)) %>% 
#   mutate( term = gsub(':time.','',term)) %>% 
#   flextable %>% 
#   hline(c(3,6,9,12,15)) %>% 
#   # hline(c(3,6,9,12,15,18,21)) %>% 
#   autofit


# Figure ------------------------------------------------------------------

rates <- models. %>%  
  filter(paramcd != 'FARS.B') %>% 
  mutate( slp.rates = map( mod.rates, ~ tidy   ( .x, effects = "fixed", conf.int = T))) %>%
  # select( -c(mod.rates, mod.diffs) ) %>% 
  unnest( slp.rates ) %>% 
  mutate( term = gsub('pm.grp','',term)) %>% 
  mutate( term = gsub(':time.','',term)) %>% 
  mutate(  estimate.rate =  ( sprintf('%0.1f', estimate) ) ) %>%
  # mutate( CI.rate = paste0(' (', sprintf('%0.1f', conf.low), ', ', sprintf('%0.1f', conf.high), ')') ) %>% 
  # select( -c(std.error, statistic, df, conf.low, conf.high, estimate )) %>% 
  .fixmod() %>% 
  rename(p.rate = p.value)

differences <- models. %>%
  filter(paramcd != 'FARS.B') %>% 
  mutate( slp.diffs = map( mod.diffs, ~ tidy   ( .x, effects = "fixed", conf.int = T))) %>%
  select( -c(mod.diffs, mod.rates) ) %>%
  unnest( slp.diffs ) %>% 
  mutate( term = gsub('pm.grp','',term)) %>% 
  mutate( term = gsub(':time.','',term)) %>% 
  .fixmod()%>%
  rename(p.diff = p.value)

rates %>% 
  filter(!c(term %in% c('bl', '(Intercept)' ))) %>% 
  ggplot()+geom_col(fill = 'lightgray')+
  aes(x = term, y= estimate)+
  geom_errorbar(aes(ymin=conf.low, ymax = conf.high), width = .1)+
  aes()+
  facet_grid(study~paramcd)+
  geom_text(aes(label = sprintf('%.2f', estimate)), nudge_y = -.15)+
  geom_text(aes(y = .2, label = paste('p =', p.diff)), data = filter(differences, !c(term %in% c('bl', '(Intercept)', 'time.' ))))+
  guides(fill = 'none')+
  theme_minimal(base_size = 10)+
  xlab('Mutation Group')+
  ylab('Estimated Yearly Rate of Change (95%CI)')


rates %>% 
  select(study, paramcd, term, estimate, conf.high, conf.low, p.rate) %>% 
  left_join(
    differences %>% 
      select(study, paramcd, term, estimate, conf.high, conf.low, p.diff) %>% 
      rename_at(vars(estimate, conf.high, conf.low), ~ paste0(., '.diff'))
    ) %>% 
  filter(study == 'FACOMS.CHOP') %>% 
  filter(estimates %in% c('(Intercept)','bl')) %>% 
  ggplot()+geom_col(fill = 'lightgray')+
  aes(y = term, x= estimate)+
  geom_errorbar(aes(xmin=conf.low, xmax = conf.high), width = .1)+
  aes()+
  facet_grid(study~paramcd)+
  geom_text(aes(label = sprintf('%.2f', estimate)), nudge_y = -.15)+
  geom_text(aes(y = .2, label = paste('p =', p.diff)))+
  guides(fill = 'none')+
  theme_minimal(base_size = 10)+
  xlab('Mutation Group')+
  ylab('Estimated Yearly Rate of Change (95%CI)')


# # A tibble: 8 × 12
# data                  model mod.lmer   effect term                 estimate std.error statistic    df p.value conf.low conf.high
# 1 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  (Intercept)             2.40     0.454      5.27   275. <0.0001   1.50      3.29  
# 2 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  bl                      0.808    0.0191    42.4    385. <0.0001   0.771     0.846 
# 3 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  gaa.100                 0.204    0.0567     3.60   264. 0.0004    0.0922    0.315 
# 4 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  pmG130V, I145F          0.416    0.611      0.681  264. 0.4966   -0.787     1.62  
# 5 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  pmother pm              0.554    0.585      0.947  272. 0.3447   -0.598     1.71  
# 6 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  time.                   1.41     0.0473    29.9    172. <0.0001   1.32      1.51  
# 7 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  pmG130V, I145F:time.   -0.535    0.223     -2.41   149. 0.0174   -0.975    -0.0955
# 8 <tibble [2,066 × 16]> pm    <lmrMdLmT> fixed  pmother pm:time.        0.200    0.233      0.855  191. 0.3935   -0.261     0.660

# slopes.descr <- dt.nonest %>% 
#   filter(!is.na(gaa1)) %>% 
#   group_by(sjid) %>%
#   mutate( obs = n()) %>% 
#   # filter( obs > 2) %>%
#   group_by(sjid, obs, pm) %>%
#   do ( tidy ( lm (aval ~ time., data = . ))) %>%
#   filter(term == 'time.') %>% 
#   select(sjid, slope = estimate, pm)
# 
# lmer(aval ~ bl + gaa.100 + pm*time. + (1 + time.|sjid) , data = dt.nonest , control = .lme4.ctrl ) %>% 
#   broom::tidy() %>% .fixmod()
# 
# slopes.model <- lmer(aval ~ bl + gaa.100 + pm*time. + (1 + time.|sjid) , data = dt.nonest , control = .lme4.ctrl ) %>% 
#   broom::tidy   ( effects = "ran_vals" ) %>% 
#   filter(term!='(Intercept)') %>% 
#   rename(sjid = level) %>% 
#   select ( sjid, estimate )
#   
# slopes.descr.phase.1 %>% 
#   filter(sjid != 4988) %>% 
#   group_by(pm.grp) %>% 
#   summarise(mean(slope, na.rm=T))
# 
# slopes.descr.phase.1 %>% 
#   # left_join(slopes.model) %>% 
#   ggplot()+geom_point()+
#   aes(color = pm)+
#   aes(slope, estimate)+
#   geom_abline(slope = 1)+
#   geom_smooth(method = 'lm', se=F)
# 
# .Last.value+
# # graphthem ---------------------------------------------------------------
# 
# slopes.descr.phase.1 %<>% 
#   left_join(.dd('demo') %>% select(sjid, site, sev.o, aoo, gaa1, pm)) %>% 
#   mutate ( pm = ifelse(pm %in% c('G130V', 'I145F'), 'G130V, I145F', pm)) %>% 
#   mutate ( pm = ifelse(pm %in% c('0', 'G130V, I145F') , pm, 'other pm' ) )
# 
# slopes.descr %>% 
#   group_by(pm) %>% summarise(m = mean(slope)) %>% 
#   ggplot()+geom_col()+
#   aes(x = pm, y = m)
# 
# 
# # . -----------------------------------------------------------------------
# 
#   
# 
# 
# 
# lmer.i <- bind_rows(
#   # dt.lmer %>% mutate(model = 'aoo') %>% mutate( mod.lmer = map( data, ~ lmer(aval ~ bl + aoo  + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
#   # dt.lmer %>% mutate(model = 'gaa1') %>% mutate( mod.lmer = map( data, ~ lmer(aval ~ bl + gaa1 + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
#   dt.lmer %>% mutate(model = 'pm') %>% mutate( mod.lmer = map( data, ~ lmer(aval ~ bl + gaa.100 + pm:time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl )))
#   ) %>% 
#   droplevels()
# 
# coef.i <- lmer.i %>% 
#   mutate( slp.lmer = map( mod.lmer, ~ tidy   ( .x, effects = "fixed", conf.int = T))) %>% 
#   unnest( slp.lmer) %>%
#   filter ( term !='(Intercept)') %>%
#   # filter ( term != 'bl') %>%
#   # arrange( paramcd, sev.o, amb ) %>% 
#   mutate ( p = sprintf('%.4f', p.value))
# 
# coef.i %>%
#   filter(model == 'pm') %>% .fixmod()
#   # filter(term=='gaa1')
#   arrange(term)
# 
# coef.i %>% 
#   filter (!(term %in% c('bl', 'time.'))) %>% 
#   select (type, term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>% 
#   arrange(type)
# 
# .Last.value %>%.ct
#   
# 
# # data --------------------------------------------------------------------
# 
# dt. %>% 
#   filter(type == 'isoform.e.l') %>% 
#   ggplot()+geom_point()+geom_line()+
#   aes(x = time., y = aval)+
#   geom_smooth(method = 'lm')+
#   facet_wrap(~sjid)
# 
# 
# 
# 

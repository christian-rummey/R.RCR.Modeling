
rm(list=ls())

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

source('DM.Htz.R')
read_excel('DATA/2023-12-04 FA-PM_import.xlsx') %>% 
  select  (sjid = `FACOMS ID`) %>% deframe -> CHOP.sjids

fars. <- readRDS('DATA derived/fars.forslope.rds') %>% 
  select(-pm) %>% 
  left_join ( htz.class %>% select(sjid, pm, pm.grp) ) %>% 
  mutate    ( pm.grp = ifelse(is.na(pm.grp), 'homozygous', pm.grp) )

fars. %<>% 
  filter   ( phase.n == 1 ) %>%
  select   ( -amb, -dupline, -phase.n) %>%
  droplevels ()

with(fars., table(pm, pm.grp))

# limit dataset ONLY here

fars. %<>% 
  group_by ( study, sjid, paramcd ) %>% filter( n()>2 ) %>% ungroup %>% 
  filter(study == 'FACOMS')

fars. %<>%
  bind_rows(
    fars. %>% filter(sjid %in% CHOP.sjids) %>% mutate( study = paste0(study, '.CHOP') )
    ) %>% 
  ungroup()

# . -----------------------------------------------------------------------

# since average avals are similar over the groups, I can argue that pm is not necessary as 
# fixed factor. If used as such, it drains effects from interaction term.  
# including time. or not changes the way results are presented (p.value for change or p-value for interaction)
# adjusting by gaa makes the estimates slightly more realistic (although p.value is not significant)

# preview model ------------------------------------------------------------

fars. %>% 
  filter(study == 'FACOMS.CHOP', paramcd == 'mFARS') %>% 
  mutate(pm.grp = factor(pm.grp, c('missense', 'homozygous', 'null'))) %>% 
  lmer(aval ~ bl + gaa100 + pm.grp:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
  broom::tidy(effects = "fixed", conf.int = T) %>% 
  .fixmod()

# run models --------------------------------------------------------------

models. <- fars. %>%
  group_by( study, paramcd ) %>% 
  nest() %>% 
  mutate( model = 'pm') %>% 
  mutate( mod.rates = map( data, ~ lmer(aval ~ bl + gaa100 + pm.grp:time.         + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) ) %>% 
  mutate( mod.diffs = map( data, ~ lmer(aval ~ bl + gaa100 + pm.grp:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl )) )%>%
  select( -data )

# DM Figure ------------------------------------------------------------------

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

# vertical bars -----------------------------------------------------------

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
  theme_minimal(base_size = 16)+
  xlab('Mutation Group')+
  ylab('Estimated Yearly Rate of Change (95%CI)')

# horizontal bars ---------------------------------------------------------

rates %>% 
  select(study, paramcd, term, estimate, conf.high, conf.low, p.rate) %>% 
  left_join(
    differences %>% 
      select(study, paramcd, term, estimate, conf.high, conf.low, p.diff) %>% 
      rename_at(vars(estimate, conf.high, conf.low), ~ paste0(., '.diff'))
  ) %>% 
  filter(study == 'FACOMS.CHOP') %>% 
  filter(!(term %in% c('(Intercept)','bl','gaa100'))) %>%
  droplevels() %>% #.ct
  ggplot()+geom_col(fill = 'lightgray')+
  aes(y = term, x= estimate)+
  geom_errorbar(aes(xmin=conf.low, xmax = conf.high), width = .1)+
  facet_grid(~paramcd)+
  geom_text(aes(label = sprintf('%.2f', estimate)), nudge_y = -.15)+
  geom_text(aes(x = 3.5, label = paste('p =', p.diff)))+
  guides(fill = 'none')+
  theme_minimal(base_size = 20)+
  xlab('Mutation Group')+
  ylab('Estimated Yearly Rate of Change (95%CI)')

# vartical 2 bars ---------------------------------------------------------

rates %>% 
  select(study, paramcd, term, estimate, conf.high, conf.low, p.rate) %>% 
  left_join(
    differences %>% 
      select(study, paramcd, term, estimate, conf.high, conf.low, p.diff) %>% 
      rename_at(vars(estimate, conf.high, conf.low), ~ paste0(., '.diff'))
  ) %>% 
  filter(study == 'FACOMS.CHOP') %>% 
  filter(!(term %in% c('(Intercept)','bl','gaa100'))) %>%
  droplevels() %>% #.ct
  ggplot()+geom_col(fill = 'lightgray')+
  aes(x = term, y= estimate)+
  geom_errorbar(aes(ymin=conf.low, ymax = conf.high), width = .1)+
  facet_grid(~paramcd)+
  geom_text(aes(label = sprintf('%.2f', estimate)), nudge_y = -.15)+
  geom_text(aes(y = 3.5, label = paste('p =', p.diff)))+
  guides(fill = 'none')+
  theme_minimal(base_size = 16)+
  xlab('Mutation Group')+
  ylab('Estimated Yearly Rate of Change (95%CI)')



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



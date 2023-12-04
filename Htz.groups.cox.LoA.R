
LoA <- .dd('steps') %>% filter(study == 'FACOMS') %>% 
  group_by(sjid) %>% 
  filter  ( length(unique(amb))>1 ) %>% 
  filter  ( amb == 'ambulatory') %>% filter(avisitn==max(avisitn)) %>% 
  .add.time(tm='age') %>% 
  mutate  ( LoA.age = age) %>% ungroup %>% 
  select(sjid, LoA.age)

LoA.amb <- .dd('steps') %>% filter(study == 'FACOMS') %>% 
  group_by(sjid) %>% 
  filter  ( amb == 'ambulatory') %>% filter(avisitn==max(avisitn)) %>% 
  .add.time(tm='age') %>% 
  mutate  ( LoA.age = age) %>% 
  select(sjid, LoA.age) %>% 
  left_join(LoA %>% select(sjid) %>% mutate(event = 1)) %>% 
  mutate(event = ifelse(is.na(event), 0, event))

LoA.amb

# cox model? --------------------------------------------------------------

LoA.amb$event %>% table

LoA.amb$sjid %>% unique() %>% length

tte <- LoA.amb %>% 
  # select(-LoA.age) %>%
  left_join(.dd('demo') %>% select(sjid, site, aoo, gaa1, pm)) %>% 
  mutate ( pm = ifelse(pm %in% c('G130V', 'I145F'), 'G130V, I145F', pm)) %>% 
  mutate ( pm = ifelse(pm %in% c('0', 'G130V, I145F') , pm, 'other pm' ) ) %>%
  mutate ( time = LoA.age) %>% 
  filter ( site == 'CHOP') %>%
  mutate ( gaa1.100 = gaa1/100)

coxph(Surv(tte$time, tte$event)~ pm + gaa1.100, data = tte ) %>% broom::tidy(exp = T, conf.int = T) %>% .fixmod
coxph(Surv(tte$time, tte$event)~ pm           , data = tte ) %>% broom::tidy(exp = T, conf.int = T) %>% .fixmod


fit <- survfit(Surv(tte$time, tte$event)~ pm, data = tte)
survdiff(Surv(tte$time, tte$event)~ pm           , data = tte ) %>% summary()
fit %>% 
  ggsurvplot(data = tte, pval = T)

# bind_rows(
#   coxph(Surv(tte$time, tte$event)~ study + aoo, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
#   coxph(Surv(tte$time, tte$event)~ study + gaa1, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
#   coxph(Surv(tte$time, tte$event)~ study + value, data = tte ) %>% broom::tidy(exp = T, conf.int = T)
# )

bind_rows(
  coxph(Surv(tte$time, tte$event)~ aoo, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ gaa1, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ value, data = tte ) %>% broom::tidy(exp = T, conf.int = T)
) %>% .fixmod()

.Last.value %>% .ct








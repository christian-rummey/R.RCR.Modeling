
dt. <- .dd('demo.l') %>%
  select(-pm, -pm.grp) %>% 
  left_join(htz.class %>% select(sjid, pm, pm.grp)) %>% 
  filter(sjid %in% CHOP.sjids) %>% 
  mutate(pm.grp = ifelse(is.na(pm.grp), 'homozygous', pm.grp))

dt. %>%
  group_by(pm.grp) %>%
  summarise(n(), mean(aoo, na.rm=T), mean(gaa1, na.rm=T))

require(tableone)
require(labelled)

# Data Baseline Table --------------------------------------------------------
#' ### Data 


pars <- levels(dt.$paramcd)

dt <- dt. %>% 
  select( sjid, sex, age_bl, aoo, diag, gaa1, pm.grp )

# Demo Table ----------------------------------------------------------------

var_label(dt) <- list(sex = 'Sex',
                      aoo = 'Age at Onset', pm.grp = 'Group', gaa1 = 'GAA*', diag = 'Age at Diagnosis',
                      age_bl  = 'Age (BL)'
                      )

tb <- tableone::CreateTableOne(
  vars       = c( 'sex', 'age_bl', 'gaa1', 'aoo', 'diag' ),
  factorVars = c( 'sex' ),
  strata     = c( 'pm.grp' ),
  data       = dt ,
  test = T,
  addOverall = F
)    

tableone::CreateTableOne(
  vars       = c( 'sex', 'age_bl', 'gaa1', 'aoo', 'diag' ),
  factorVars = c( 'sex' ),
  strata     = c( 'pm.grp' ),
  data       = dt ,
  test = T,
  addOverall = F
) %>% 
  print(varLabels = T, nonnormal  = c( 'aoo', 'gaa1', 'age_bl'), contDigits = 1)%>%
  data.frame() %>% rownames_to_column() %>%
  select(rowname, homozygous, missense, null, p) %>%
  as.data.frame() %>% .ct
  knitr::kable()

tb %>% print(varLabels = T, 
             nonnormal  = c( 'age', 'symp', 'gaa1','gaa2','age','age.last','sinced','fu' ),
             contDigits = 1, 
             catDigits = 0,
             missing = T,
             explain = F, 
             dropEqual= F, 
             add.rownames = T,
             format = 'p'
) %>%
  data.frame() %>% rownames_to_column() %>%
  select(rowname, X0.7y, X8.14y, X15.24y, X.24y, Overall, Missing) %>%
  as.data.frame() %>% 
  knitr::kable()

# Baseline Table ----------------------------------------------------------

# dt. <- readRDS('../DATA derived/lmer.data.paper1.rds')

pars <- c("mFARS", "FARS.Am", "FARS.B","FARS.E", 'FARS.C' , "ADL", "w25.i", "hpt.i",'bva')

dt. %>% 
  filter  (pm==0) %>%
  filter  ( forslope == 1) %>% 
  mutate  ( paramcd = factor(paramcd, pars)) %>% filter(!is.na(paramcd)) %>% 
  ungroup %>% 
  select  ( study, sjid, sev.o, age, amb, paramcd, aval ) %>%
  group_by( sjid, sev.o, paramcd) %>% 
  filter  ( age  == min(age)) %>%
  group_by(        paramcd, amb) %>% 
  mutate  ( N = n() ) %>% 
  group_by(       sev.o, paramcd, amb, N) %>% 
  summarise( m = mean(aval), sd = sd(aval)) %>% 
  mutate_at(c('m', 'sd'), ~sprintf('%#.3g',.)) %>% 
  mutate  ( val = paste(m, '\n(', sd, ')', sep = '') ) %>% 
  select  ( -m, -sd  ) %>% 
  spread  ( sev.o, val) %>% 
  flextable

# %>% 
#   autofit %>% align(align = 'center', part = 'all')


dt %>% 
  group_by(site) %>% 
  summarise(n = n(), range = paste(min(round(age,1)), ' - ', max(round(age,1)), sep= '')) %>% 
  arrange(site)
-
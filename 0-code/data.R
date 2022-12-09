# Staff data
staff_data <- read_csv("0-data/149756_Item2_Data.csv", show_col_types = FALSE)

econ_staff <- staff_data %>%
  filter(F_ACEMPFUN!=4 & F_ACEMPFUN!=9 & F_ACEMPFUN!='X') %>% # Exclude staff not on an academic contract (4), not teaching and/or research (9) and not applicable/not required (X).
  filter(F_CCENTRE==129 | F_CCENTRE==133) %>% # Economics department (129) or business school (133) cost function.
  filter(F_CURACCDIS1=='L1' | F_CURACCDIS2=='L1') %>% # Primary or secondary academic discipline is economics (L1).
  filter(F_LEVELS!=1 & F_LEVELS!=2 & F_LEVELS!="F2" & F_LEVELS!='M0' & F_LEVELS!='N0' & F_LEVELS!='O0' & F_LEVELS!='P0' & F_LEVELS!='UNK') %>% # Exclude senior management (1), head of schools (2), function heads (F2), administrative staff (M0), junior administrative staff (N0), routine task providers (O0), simple task providers (P0) and individuals with no known academic rank (UNK).
  filter(F_TERMS!=3) %>% # Exclude staff on atypical contracts (3).
  mutate(
    F_LEVELS = recode(F_LEVELS, F1="Professor", I0="Reader/SL", J0="Lecturer (B)", K0="Lecturer (A)", L0="TA/RA"),
    F_SEXID = recode(F_SEXID, `1`="Male", `2`="Female"),
    F_TERMS = recode(F_TERMS, `1`="Permanent", `2`="Fixed-term"),
    F_ACEMPFUN = recode(F_ACEMPFUN, `1`="Teaching", `2`="Research", `3`="T&R"),
    F_XMOEMP01 = recode(F_XMOEMP01, `1`="Full-time", `2`="Part-time"),
    Ethnicity = recode(Ethnicity, `21`="Black", `22` = "Black", `29`= "Black", `31`="South Asia", `32`="South Asia", `33`="South Asia", `34`="Other Asia", `39`="Other Asia", UNK="Unknown"),
    F_XSNAT01 = recode(F_XSNAT01, `1`="UK", `2`="EU/EEA", `4`="EU/EEA", `5`="Non-UK/EU", `9`="Unknown"),
    ACYEAR=as.numeric(str_sub(ACYEAR, 1, 4))
  ) %>%
  mutate(
    F_LEVELS = factor(F_LEVELS, levels = c("TA/RA", "Lecturer (A)", "Lecturer (B)", "Reader/SL", "Professor"), ordered=TRUE),
    F_ACEMPFUN = factor(F_ACEMPFUN, levels = c("Research", "Teaching", "T&R"), ordered = TRUE),
    F_XSNAT01 = factor(F_XSNAT01, levels = c("UK", "EU/EEA", "Non-UK/EU", "Unknown"), ordered=TRUE),
    Ethnicity = factor(Ethnicity, levels=c("White", "Black", "South Asia", "Other Asia", "Other"), ordered=TRUE),
    F_XMOEMP01 = factor(F_XMOEMP01, levels=c("Part-time", "Full-time"), ordered=TRUE)
  )

# Survey data.
res_survey <- read_dta("0-data/Resp_Survey_Panel_96-16_2307.dta")

# Student data.
student_data <- read_csv("0-data/149756_Item1_1213to1516_Data.csv") %>%
  bind_rows(read_csv("0-data/149756_Item1_1617to1819_Data.csv"))

econ_students <- student_data %>%
  filter(F_XJACSA01=="L1") %>% # Include only students studying economics (L1).
  filter(F_SEXID!=3) %>% # Exclude students whose gender is classified as "Other" (3).
  filter(F_XLEV601!=3 & F_XLEV601!=5) %>% # Exclude students who are studying on a non-first-degree undergraduate programme or a non-Masters or non-PhD postgraduate programme.
  filter(F_XMODE301==1) %>% # Include only full-time students (1).
  mutate(
    ACYEAR = as.numeric(str_sub(ACYEAR, 1, 4)),
    F_XLEV601 = recode(F_XLEV601, `4`="First degree"),
    F_SEXID = recode(F_SEXID, `1`="Male", `2`="Female"),
    F_XDOMGR401 = recode(F_XDOMGR401, `1`="UK", `2`="EU", `3`="Other", `9`="Unknown"),
    F_NATION = recode(F_NATION, "Other EU"="EU", "Non-EU"="Other"),
    F_ZSTATE_MARKER = recode(F_ZSTATE_MARKER, `1`="State-funded", `0`="Privately funded", U="Unknown"),
    ECONMKR = recode(ECONMKR, ECONALEV="Econ A-level", NOECONALEV="No econ A-level"),
    Ethnicity = recode(Ethnicity, UNK="Unknown", `21`="Black", `22`="Black", `29`="Black", `31`="South Asia", `32`="South Asia", `33`="South Asia", `34`="Other Asia", `39`="Other Asia", Mixed="Other"),
    BME_MKR = recode(BME_MKR, `Unknown/Not applicable`="Unknown")
  ) %>%
  mutate(
    F_XLEV601 = factor(F_XLEV601, levels=c("First degree", "Masters", "Doctorate"), ordered=TRUE),
    F_XDOMGR401 = factor(F_XDOMGR401, levels=c("UK", "EU", "Other"), ordered=TRUE),
    F_NATION = factor(F_NATION, levels=c("UK", "EU", "Other"), ordered=TRUE),
    Ethnicity = factor(Ethnicity, levels=c("White", "Black", "South Asia", "Other Asia", "Other"), ordered=TRUE)
  )

# Past RES reports.
reports <- read_csv("0-data/reports.csv", show_col_types = FALSE)

# Staff summary data

# Academic rank
contracts <- econ_staff %>%
  filter(ACYEAR==2018 & F_XMOEMP01=="Full-time" & F_ACEMPFUN=="T&R" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA") %>%
  group_by(F_LEVELS, F_SEXID) %>%
  count(wt=FTE) %>%
  pivot_wider(names_from=F_SEXID, values_from=n) %>%
  ungroup() %>%
  mutate(
    dist_male = 100*Male/sum(Male),
    dist_female = 100*Female/sum(Female)
  ) %>%
  adorn_totals(where=c("row")) %>%
  mutate(
    Total = Male+Female,
    percent_fem = 100*Female/(Male+Female)
  ) %>%
  relocate(F_LEVELS, Male, Female, Total, percent_fem, dist_male, dist_female)

hesa_fper <- econ_staff %>%
  filter(F_ACEMPFUN=="T&R" & F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & F_LEVELS!="Lecturer (A)") %>%
  group_by(ACYEAR, F_LEVELS, F_SEXID) %>%
  count(wt=FTE) %>%
  pivot_wider(names_from=F_SEXID, values_from=n) %>%
  mutate(percent = 100*Female/(Male+Female)) %>%
  rename(year=ACYEAR, contract=F_LEVELS) %>%
  mutate(dtype="HESA")

res_fper <- res_survey %>%
  select(year, ft_prof_fem, ft_prof_male, ft_rsl_fem, ft_rsl_male, ft_lec_perm_fem, ft_lec_perm_male) %>%
  replace_na(list(ft_prof_fem=0, ft_prof_male=0, ft_rsl_fem=0, ft_rsl_male=0, ft_lec_perm_fem=0, ft_lec_perm_male=0)) %>%
  pivot_longer(!year, names_to="name", values_to="value") %>%
  mutate(name = str_replace(str_replace(name, "perm_", ""), "ft_", "")) %>%
  separate(name, c("contract", "gender")) %>%
  group_by(year, gender, contract) %>%
  summarise_all(sum) %>%
  group_by(year, contract) %>%
  mutate(
    percent = 100*value/sum(value), dtype="RES",
    contract = recode(contract, lec="Lecturer (B)", rsl="Reader/SL", prof="Professor"),
    gender = recode(gender, fem="Female", male="Male")
  ) %>%
  select(-value)

hesa_rank <- econ_staff %>%
  filter(F_ACEMPFUN=="T&R" & F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="Lecturer (A)" & F_LEVELS!="TA/RA") %>%
  group_by(ACYEAR, F_LEVELS, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(
    percent = 100*n/sum(n),
    dtype = "HESA"
  ) %>%
  rename(year=ACYEAR, contract=F_LEVELS, gender=F_SEXID)

res_rank <- res_survey %>%
  select(year, ft_prof_fem, ft_prof_male, ft_rsl_fem, ft_rsl_male, ft_lec_perm_fem, ft_lec_perm_male) %>%
  replace_na(list(ft_prof_fem=0, ft_prof_male=0, ft_rsl_fem=0, ft_rsl_male=0, ft_lec_perm_fem=0, ft_lec_perm_male=0)) %>%
  pivot_longer(!year, names_to="name", values_to="value") %>%
  mutate(name = str_replace(str_replace(name, "perm_", ""), "ft_", "")) %>%
  separate(name, c("contract", "gender")) %>%
  group_by(year, gender, contract) %>%
  summarise_all(sum) %>%
  group_by(year, gender) %>%
  mutate(
    percent = 100*value/sum(value),
    dtype = "RES",
    contract = recode(contract, lec="Lecturer (B)", rsl="Reader/SL", prof="Professor"),
    gender = recode(gender, fem="Female", male="Male")
  ) %>%
  select(-value)

# Employment function
emp_all <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & ACYEAR==2018) %>%
  group_by(F_ACEMPFUN) %>%
  count(wt=FTE) %>%
  ungroup() %>%
  mutate(percent = n / sum(n))

emp_sex <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & ACYEAR==2018) %>%
  group_by(F_ACEMPFUN, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(F_SEXID) %>%
  mutate(percent = n / sum(n))

emp <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA") %>%
  mutate(F_LEVELS = recode(F_LEVELS, `Lecturer (A)`="Lecturer", `Lecturer (B)`="Lecturer", `Reader/SL`="Reader/SL/Prof", Professor="Reader/SL/Prof")) %>%
  group_by(ACYEAR, F_ACEMPFUN, F_LEVELS, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_ACEMPFUN, F_LEVELS) %>%
  mutate(percent = n/sum(n), total=sum(n)) %>%
  ungroup()

emp_all_fem <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA") %>%
  group_by(ACYEAR, F_ACEMPFUN, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_ACEMPFUN) %>%
  mutate(
    percent_1 = 100*n/sum(n),
    total_1=sum(n)
  ) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(
    percent_2 = 100*n/sum(n),
    total_2=sum(n)
  ) %>%
  ungroup()

# Mode of employment
parttime_all <- econ_staff %>%
  filter(F_ACEMPFUN=="T&R" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA") %>%
  group_by(ACYEAR, F_SEXID, F_XMOEMP01) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent = n/sum(n))

parrtime_rank <- econ_staff %>%
  filter(F_XMOEMP01=="Part-time" & F_TERMS=="Permanent" & F_ACEMPFUN=="T&R" & F_LEVELS!="TA/RA") %>%
  mutate(F_LEVELS = recode(F_LEVELS, `Lecturer (A)`="Lect/SL/reader", `Lecturer (B)`="Lect/SL/reader", `Reader/SL`="Lect/SL/reader")) %>%
  group_by(ACYEAR, F_LEVELS, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_LEVELS) %>%
  mutate(percent_fem = 100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist = 100*n/sum(n))

parttime_emp <- econ_staff %>%
  filter(F_XMOEMP01=="Part-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & F_ACEMPFUN!="Research") %>%
  group_by(ACYEAR, F_ACEMPFUN, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_ACEMPFUN) %>%
  mutate(percent_fem = 100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist = 100*n/sum(n))

# Terms of employment
all_temps <- econ_staff %>%
  group_by(ACYEAR, F_SEXID, F_TERMS) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_TERMS) %>%
  mutate(percent_fem = n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(dist_sex = n/sum(n))

temp_contracts <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_ACEMPFUN=="T&R") %>%
  group_by(ACYEAR, F_SEXID, F_TERMS) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_TERMS) %>%
  mutate(percent_fem = n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(dist_sex = n/sum(n))

temp_rank <- econ_staff %>%
  filter(F_TERMS=="Fixed-term") %>%
  mutate(F_LEVELS = recode(F_LEVELS, "Lecturer (A)"="Lect/TA/RA", "Lecturer (B)"="Lect/TA/RA", "TA/RA"="Lect/TA/RA")) %>%
  group_by(F_LEVELS, F_XMOEMP01, F_ACEMPFUN) %>%
  count(wt=FTE) %>%
  group_by(F_LEVELS) %>%
  mutate(percent = n/sum(n))

temp_sl <- econ_staff %>%
  filter(F_LEVELS=="Reader/SL" & ACYEAR==2018) %>%
  group_by(F_TERMS) %>%
  count(wt=FTE) %>%
  ungroup() %>%
  mutate(percent = n/sum(n))

temp_parttime <- econ_staff %>%
  filter(F_TERMS=="Fixed-term") %>%
  group_by(F_SEXID, F_XMOEMP01, ACYEAR) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_XMOEMP01) %>%
  mutate(percent_fem=100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist=100*n/sum(n))

temp_emp <- econ_staff %>%
  filter(F_TERMS=="Fixed-term") %>%
  group_by(F_SEXID, F_ACEMPFUN, ACYEAR) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_ACEMPFUN) %>%
  mutate(percent_fem = 100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist=100*n/sum(n))

temp <- econ_staff %>%
  filter(F_TERMS=="Fixed-term") %>%
  mutate(F_LEVELS = recode(F_LEVELS, "Professor"="Prof/Reader/SL", "Reader/SL"="Prof/Reader/SL", "Lecturer (A)"="Lecturer", "Lecturer (B)"="Lecturer")) %>%
  group_by(F_SEXID, F_LEVELS, ACYEAR) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_LEVELS) %>%
  mutate(percent_fem = 100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist = 100*n/sum(n))

# Nationality
nationality <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_ACEMPFUN=="T&R" & F_LEVELS!="TA/RA" & F_XSNAT01!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_XSNAT01) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_XSNAT01) %>%
  mutate(percent_fem=100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_nat=100*n/sum(n))

nationality_rank <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_ACEMPFUN=="T&R" & F_LEVELS!="TA/RA" & F_XSNAT01!="Unknown") %>%
  mutate(F_LEVELS = recode(F_LEVELS, `Lecturer (B)`="Lecturer", `Lecturer (A)`="Lecturer")) %>%
  group_by(ACYEAR, F_SEXID, F_XSNAT01, F_LEVELS) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_XSNAT01, F_LEVELS) %>%
  mutate(percent_fem=100*n/sum(n))

nationality_efunc <- econ_staff  %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & F_XSNAT01!="Unknown") %>%
  mutate(F_ACEMPFUN = recode(F_ACEMPFUN, Teaching="Teaching-/research-only", Research="Teaching-/research-only")) %>%
  group_by(ACYEAR, F_SEXID, F_XSNAT01, F_ACEMPFUN) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, F_XSNAT01, F_ACEMPFUN) %>%
  mutate(percent_fem=100*n/sum(n))

# Ethnicity
bme <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_ACEMPFUN=="T&R" & F_LEVELS!="TA/RA" & BME_MKR!="Unknown") %>%
  group_by(ACYEAR, BME_MKR, F_SEXID) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR) %>%
  mutate(percent=n/sum(n))

ethnicity_efunc <- econ_staff  %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_LEVELS!="TA/RA" & BME_MKR!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, BME_MKR, F_ACEMPFUN) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, BME_MKR, F_ACEMPFUN) %>%
  mutate(percent_fem=n/sum(n))

ethnicity_rank <- econ_staff %>%
  filter(F_XMOEMP01=="Full-time" & F_TERMS=="Permanent" & F_ACEMPFUN=="T&R" & F_LEVELS!="TA/RA" & Ethnicity!="Unknown" & Ethnicity!="Mixed" & Ethnicity!="Other") %>%
  mutate(F_LEVELS = recode(F_LEVELS, `Lecturer (A)`="Lecturer", `Lecturer (B)`="Lecturer")) %>%
  mutate(Ethnicity = recode(Ethnicity, `Other Asia`="Asia", `South Asia`="Asia")) %>%
  group_by(ACYEAR, F_SEXID, Ethnicity, F_LEVELS) %>%
  count(wt=FTE) %>%
  group_by(ACYEAR, Ethnicity, F_LEVELS) %>%
  mutate(percent_fem=100*n/sum(n))

# Students.

# Level of study
levels <- econ_students %>%
  filter(ACYEAR==2018) %>%
  group_by(ACYEAR, F_XLEV601, F_SEXID) %>%
  count(wt=counter) %>%
  pivot_wider(names_from=F_SEXID, values_from=n) %>%
  ungroup() %>%
  mutate(dist_male = 100*Male/sum(Male), dist_female=100*Female/sum(Female)) %>%
  adorn_totals(where="row", name="Total") %>%
  mutate(Total=Male+Female, percent_fem=100*Female/(Male+Female), F_XLEV601=recode(F_XLEV601, "-"="Total")) %>%
  relocate(F_XLEV601, Male, Female, Total, percent_fem, dist_male, dist_female) %>%
  select(-ACYEAR)

levels_time <- econ_students %>%
  group_by(ACYEAR, F_XLEV601, F_SEXID) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_XLEV601) %>%
  mutate(percent_fem=100*n/sum(n)) %>%
  group_by(ACYEAR, F_SEXID) %>%
  mutate(percent_dist=100*n/sum(n))

# Domicile/nationality
domicile <- econ_students %>%
  filter(F_XDOMGR401!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_XDOMGR401) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_XDOMGR401) %>%
  mutate(percent_fem = 100*n/sum(n))

domicile_levels <- econ_students %>%
  filter(F_XDOMGR401!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_XDOMGR401, F_XLEV601) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_XDOMGR401, F_XLEV601) %>%
  mutate(percent_fem = 100*n/sum(n))

nationality_students <- econ_students %>%
  filter(F_NATION!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_NATION) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_NATION) %>%
  mutate(percent_fem = 100*n/sum(n))

nationality_levels <- econ_students %>%
  filter(F_NATION!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_NATION, F_XLEV601) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_NATION, F_XLEV601) %>%
  mutate(percent_fem = 100*n/sum(n))

# Secondary education
school <- econ_students %>%
  filter(F_XDOMGR401=="UK" & F_XLEV601=="First degree" & F_ZSTATE_MARKER!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_ZSTATE_MARKER) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_ZSTATE_MARKER) %>%
  mutate(percent_fem=100*n/sum(n))

alevels <- econ_students %>%
  filter(F_XDOMGR401=="UK" & F_XLEV601=="First degree") %>%
  group_by(ACYEAR, F_SEXID, ECONMKR) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, ECONMKR) %>%
  mutate(percent_fem=100*n/sum(n))

# Ethnicity
school_bme <- econ_students %>%
  filter(F_XLEV601=="First degree" & F_ZSTATE_MARKER!="Unknown" & F_XDOMGR401=="UK" & BME_MKR!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_ZSTATE_MARKER, BME_MKR) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_ZSTATE_MARKER, BME_MKR) %>%
  mutate(percent_fem=100*n/sum(n))

alevels_bme <- econ_students %>%
  filter(F_XLEV601=="First degree" & F_XDOMGR401=="UK" & BME_MKR!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, ECONMKR, BME_MKR) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, ECONMKR, BME_MKR) %>%
  mutate(percent_fem=100*n/sum(n))

ethnicity_undergrad <- econ_students %>%
  filter(F_XLEV601=="First degree" & Ethnicity!="Unknown" & Ethnicity!="Other" & Ethnicity!="Mixed") %>%
  group_by(ACYEAR, F_SEXID, Ethnicity) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, Ethnicity) %>%
  mutate(percent_fem=100*n/sum(n))

bme_level <- econ_students %>%
  filter(BME_MKR!="Unknown") %>%
  group_by(ACYEAR, F_SEXID, F_XLEV601, BME_MKR) %>%
  count(wt=counter) %>%
  group_by(ACYEAR, F_XLEV601, BME_MKR) %>%
  mutate(percent_fem=100*n/sum(n))

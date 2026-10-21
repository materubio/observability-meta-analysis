library(dplyr)
library(tibble)

# Convert t-statistic to Pearson correlation
t_to_r <- function(t, n){
  df <- n - 2
  r <- t / sqrt(t^2 + df)
  return(r)
}

# p-value (two-sided) to r, given n and sign of beta
p_to_r <- function(p, n, sign){
  zmag <- qnorm(1 - p/2)       # |z|
  tval <- zmag                 # large-sample Wald → z ~ t
  r    <- sign * tval / sqrt(tval^2 + (n - 2))
  return(r)
}

# Convert Pearson r to Fisher's z
r_to_z <- function(r){
  z <- 0.5 * log((1 + r) / (1 - r))
  return(z)
}

# Standard error of Fisher's z
z_se <- function(n){
  se <- 1 / sqrt(n - 3)
  return(se)
}

# Enikolopov (2024) – Political Correctness, Social Image, and Information Transmission

political2024 <- tribble(
  ~study_id,              ~contrast,               ~beta, ~se,  ~n, ~has_prosocial_outcome, ~decision_format,
  "Political2024",        "Pooled (all waves)",     0.13, 0.05, 1699, 0, "multiple decisions",
) %>%
  mutate(
    # Two-sided test: derive t and p from beta / se
    t     = beta / se,
    p     = 2 * (1 - pnorm(abs(t))),
    r     = t_to_r(t, n),     # convert to Pearson's r
    z     = r_to_z(r),        # Fisher's z transformation
    var_z = z_se(n)^2,        # variance of Fisher's z
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, se, p, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(political2024)

# Friedrichsen & Engelmann (2018) - Who cares about social image?

cares2018 <- tribble(
  ~study_id,     ~contrast,            ~beta,  ~se,   ~n, ~has_prosocial_outcome, ~decision_format,
  "Cares2018",   "Public vs Private",  0.292,  0.122, 121, 1, "WTP",
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n),      # Pearson r
    z     = r_to_z(r),         # Fisher's z
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, se, n, r, z, var_z,r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(cares2018)

# Andreoni & Bernheim (2009) – Social Image and the 50–50 Norm
# Flipped the sign because more p means less observability

andreoni2009 <- tribble(
  ~study_id,     ~contrast,               ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Andreoni2009","visibility (p>=0.25)",  0.532,  0.124, 236, 1, "continuous choice",
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n), 
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, se, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(andreoni2009)

# Karlan & McConnell (2012) – Hey Look at Me: The Effect of Giving While Observed

giving_circles <- tribble(
  ~study_id,                   ~contrast,                             ~beta,  ~se,   ~n, ~has_prosocial_outcome, ~decision_format,
  "KarlanMcConnell2012_field", "Any treatment, any gift",              0.027, 0.012, 4168, 1, "continuous choice",
  "KarlanMcConnell2012_field", "Any treatment, gift ≥ 100",            0.018, 0.009, 4168, 1, "continuous choice",
  "KarlanMcConnell2012_field", "Any treatment, gift ≥ 500",            0.005, 0.003, 4168, 1, "continuous choice",
  "KarlanMcConnell2012_field", "Any treatment, log(1+gift)",           0.140, 0.052, 4168, 1, "continuous choice",
  "KarlanMcConnell2012_lab",   "Signaling vs Image, round 2 gift",     0.126, 0.220,   94, 1, "continuous choice"
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, se, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(giving_circles)

# Ariely, Bracha & Meier (2009) – Doing Good or Doing Well?

dgw2009 <- tribble(
  ~study_id,                   ~contrast,       ~t,     ~n, ~has_prosocial_outcome, ~decision_format,
  "ABM2009_lab_image_panelA",  "Public sphere",  1.80, 161, 1, "continuous choice",
  "ABM2009_lab_image_panelB",  "Public sphere", 0.25, 161, 1, "continuous choice",
  "ABM2009_field_image",       "Public vs Private", 1.34, 151, 1, "continuous choice",
) %>%
  mutate(
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, t, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(dgw2009)

# Dannenberg (2024) – The effects of social norms and observability on food choice

food2024 <- tribble(
  ~study_id,          ~contrast,     ~beta,   ~p,     ~n, ~has_prosocial_outcome, ~decision_format,
  "Dannenberg2024",   "Meat Public", 0.011,  0.863,  931, 0.5, "DE",
  "Dannenberg2024",   "Meatless Public", -0.014,  0.653,  931, 0.5, "DE",
  "Dannenberg2024",   "No Voucher Public", 0.003,  0.960,  931, 0.5, "DE"
) %>%
  mutate(
    r     = mapply(p_to_r, p, n, sign(beta)),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, p, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(food2024)

# Friedrichsen, König & Schmacker (2018) — Social image concerns and welfare take-up

welfare <- tribble(
  ~study_id,         ~contrast,                   ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Friedrichsen2018","Quiz Redistribution",       0.303,  0.038,  165, 0.5, "DE",
  "Friedrichsen2018","Random Redistribution",     0.189,  0.032,  159, 0.5, "DE",
  "Friedrichsen2018","Random Subsidized",         0.021,  0.047,  48, 0.5, "DE",
  "Friedrichsen2018","Quiz Subsidized",           0.174,  0.054,  69, 0.5, "DE"
) %>%
  mutate(
    t = beta / se,
    r = t_to_r(t, n),
    z = r_to_z(r),
    se_z = z_se(n),
    var_z = se_z^2,
    z_lo = z - 1.96 * se_z,
    z_hi = z + 1.96 * se_z,
    r_lo = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1),
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(welfare)

# Teyssier, Etile & Combris (2015) — Social- and self-image concerns in fair-trade consumption

fairtrade <- tribble(
  ~study_id,     ~contrast,          ~beta,  ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Teyssier2015","Public Treatment", 0.529, 0.201, 110, 1, "WTP",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo = z - 1.96 * se_z,
    z_hi = z + 1.96 * se_z,
    r_lo = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(fairtrade)

# Ewers & Zimmermann (2015) — IMAGE AND MISREPORTING

misreporting <- tribble(
  ~study_id,            ~contrast,             ~beta,  ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "EwersZimmermann2015","Audience vs Private", 0.466, 0.218, 143, 0, "DE",
) %>%
  mutate(
    t  = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo = z - 1.96 * se_z,
    z_hi = z + 1.96 * se_z,
    r_lo = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(misreporting)

# Hofmann (2025) — How interpersonal closeness and social image concerns shape Pay-What-You-Want decisions independently: evidence from two large-scale online experiments

pwyw_hofmann <- tribble(
  ~study_id,   ~contrast,               ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Hofmann2025","Audience Study 1",     0.85,    0.34,      1034, 0, "WTP",
  "Hofmann2025","Audience Study 2",     0.59,    0.34,      995, 0, "WTP",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, t, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(pwyw_hofmann)

# Bašić & Quercia (2022)- The Influence of Self and Social Image Concerns on Lying
# Flipped the sign of Social vs Control because a negative beta means less "lying" when more observability.
lying <- tribble(
  ~study_id,          ~contrast,                   ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "BasicQuercia2022", "Social vs Control, Exp1",    0.353,  0.171,  531, 1, "DE",
  "BasicQuercia2022", "Social vs Self-Call, Exp2)", 0.413,  0.178,  306, 1, "DE"
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(lying)

# Observability, Honesty, and the Social Image Costs of Lying
#I flipped the sign of the free contrasts because less reported values means less lying when more observability
obs_costs <- tribble(
  ~study_id,       ~contrast,                       ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "BoschRosa2025", "Observable Restricted",         0.255,  0.111,  1183, 1, "DE",
  "BoschRosa2025", "Observable Unrestricted",       0.611,  0.219,  700, 1, "DE",
  "BoschRosa2025", "Observable Missing Indicator",  0.313,  0.106,  1318, 1, "DE",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(obs_costs)

#Butera et al. (2022) — Measuring the Welfare Effects of Shame and Pride

shame_pride <- tribble(
  ~study_id,           ~contrast,                       ~beta,    ~se,     ~n, ~has_prosocial_outcome, ~decision_format,
  "Butera2022_YMCA",    "Public vs Anonymous",          1.19,     0.46,    360, 0, "continuous choice",
  "Butera2022_Prolific","Public vs Anonymous",          105.01,   12.25,   968, 0, NA_character_,
  "Butera2022_Berkeley","Public vs Anonymous",          134.41,   22.56,   384, 0, NA_character_,
  "Butera2022_BU",      "Public vs Anonymous",          103.61,   45.25,   118, 0, NA_character_
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(shame_pride)

#Andreoni & Petrie (2004) — Public goods experiments without confidentiality

andreoni_petrie <- tribble(
  ~study_id,           ~contrast,                             ~p,       ~n,  ~sign, ~has_prosocial_outcome, ~decision_format,
  "AndreoniPetrie2004","Info+Photos vs Information",          0.0001,   80,   +1, 1, "continuous choice",
  "AndreoniPetrie2004","Info+Photos vs Photos",               0.0423,   80,   +1, 1, "continuous choice",
  "AndreoniPetrie2004","Optional Reporting vs Info+Photos",   0.1284,   80,   +1, 1, "continuous choice"
) %>%
  mutate(
    r      = mapply(p_to_r, p, n, sign),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  transmute(study_id, contrast, p, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(andreoni_petrie)

#Filiz-Ozbay & Ozbay (2013) — Effect of an Audience in Public Goods Provision

audience <- tribble(
  ~study_id,         ~contrast,                                      ~p,      ~n,  ~sign, ~has_prosocial_outcome, ~decision_format,
  "FilizOzbay2013",  "Effort+Observer vs No Observer, 1-shot", 0.001, 84, +1, 1, "continuous choice",
  "FilizOzbay2013",  "Effort+Observer vs No Observer, repeated", 0.001, 60, +1, 1, "continuous choice",
  "FilizOzbay2013",  "NoEffort+Observer vs No Observer, 1-shot",     0.842, 80, +1, 1, "continuous choice",
  "FilizOzbay2013",  "NoEffort+Observer vs No Observer, repeated",   0.600, 64, +1, 1, "continuous choice"
) %>%
  mutate(
    r     = mapply(p_to_r, p, n, sign),
    z     = r_to_z(r),
    se_z  = z_se(n),
    var_z = se_z^2,
    z_lo  = z - 1.96 * se_z,
    z_hi  = z + 1.96 * se_z,
    r_lo  = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  transmute(study_id, contrast, p, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(audience)

# Savikhin Samek & Sheremeta (2014) - Recognizing contributors: an experiment on public goods

contributors <- tribble(
  ~study_id,            ~contrast,                           ~beta, ~se,   ~n, ~has_prosocial_outcome, ~decision_format,
  "SavikhinSamek2014",  "ALL vs NONE",        19.29, 2.88,   3200, 1, "continuous choice",
  "SavikhinSamek2014",  "TOP vs NONE",         4.84, 2.89,   3200, 1, "continuous choice",
  "SavikhinSamek2014",  "BOTTOM vs NONE",     27.25, 2.88,   3200, 1, "continuous choice"
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    var_z  = z_se(n)^2,
    z_lo   = z - 1.96 * sqrt(var_z),
    z_hi   = z + 1.96 * sqrt(var_z),
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(contributors)

# Soetevent (2005) - Anonymity in giving in a natural context—a field experiment in 30 churches

churches <- tribble(
  ~study_id,        ~contrast,             ~beta,  ~se,  ~n, ~has_prosocial_outcome, ~decision_format,
  "Soetevent2005",  "Basket 1st vs bag",   0.028,  0.023, 1582, 1, "continuous choice",
  "Soetevent2005",  "Basket 2nd vs bag",   0.096,  0.028, 1582, 1, "continuous choice"
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    var_z  = z_se(n)^2,
    z_lo   = z - 1.96 * sqrt(var_z),
    z_hi   = z + 1.96 * sqrt(var_z),
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(churches)

# Hofmeier & Strang (2023) – Image concerns and the dynamics of prosocial behavior

dynamics <- tribble(
  ~study_id,         ~contrast,                               ~beta,   ~se,   ~n, ~has_prosocial_outcome, ~decision_format,
  "HofmeierStrang2023", "Public vs private, Stage 1 – Exp A",  0.179,  0.105,   203, 1, "continuous choice",
  "HofmeierStrang2023", "Stage 2 after public exposure – Exp A", 0.257,  0.226,   203, 1, "continuous choice",
  "HofmeierStrang2023", "Public vs private, Stage 1 – Exp B",  0.0199, 0.0382,    77, 1, "continuous choice",
  "HofmeierStrang2023", "Stage 2 after public exposure – Exp B", 0.102,  0.0682,    77, 1, "continuous choice"
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(dynamics)

# Birke(2023) - Anti-Bunching: A New Test for Signaling Motives in Prosocial Behavior

anti_b <- tribble(
  ~study_id,   ~contrast,                          ~beta, ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Birke2023", "badge vs no badge",                1.356,  0.550, 2131, 1, "continuous choice",
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(anti_b)

# Exley (2017) – Incentives for Prosocial Behavior: The Role of Reputations
# Social imagine concern is related with less volunteering

incentives <- tribble(
  ~study_id,        ~contrast,              ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Exley2017_online", "Public Incentives",  -0.76, 0.14, 800, 1, "DE",
  "Exley2017_online", "Public Reputation",  -0.26, 0.14, 800, 1, "DE",
  "Exley2017_lab",    "Public Incentives",  -65.94, 59.48, 130, 1, "continuous effort task",
  "Exley2017_lab",    "Public Reputation",  -83.99, 55.20, 130, 1, "continuous effort task",
) %>%
  mutate(
    t     = beta / se,
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(incentives)

# Gnezzy (2012) - Pay-what-you-want, identity, and self-signaling in markets

pwyw_gnezzy <- tribble(
  ~study_id,    ~contrast,                                      ~t,      ~n, ~has_prosocial_outcome, ~decision_format,
  "Gneezy2012", "PWYW + charity vs PWYW regular – theme park",  43.24,  54231, 0, "continuous choice",
  "Gneezy2012", "PWYW vs fixed price $5 – boat",                2.22,   40, 0, "continuous choice",
  "Gneezy2012", "Anonymous vs observed – restaurant",           0.79/0.35,   235, 0, "continuous choice"
) %>%
  mutate(
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(pwyw_gnezzy)

# Rege & Telle (2004) – The impact of social approval and framing on cooperation in public good situations
rege2004 <- tribble(
  ~study_id,   ~contrast,                                                ~p,     ~n,  ~sign, ~has_prosocial_outcome, ~decision_format,
  "Rege2004",  "Approval vs No-Approval – Non Associative framing)",     0.011,  40,   +1, 1, "continuous choice",
  "Rege2004",  "Approval vs No-Approval – Associative framing)",         0.044,  40,   +1, 1, "continuous choice",
) %>%
  mutate(
    t     = sign * qnorm(1 - p),
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, p, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(rege2004)

# (Karing, 2023) - Social Signaling and Childhood Immunization

karing2023 <- tribble(
  ~study_id,    ~contrast,                     ~beta,  ~se,   ~n, ~has_prosocial_outcome, ~decision_format,
  "Karing2023", "S5 vs Control — timely v5",   0.133,  0.041, 4897, 1, "DE",
  "Karing2023", "S4 vs Control — timely v5",   0.004,  0.041, 4897, 1, "DE",
  "Karing2023", "S4 vs Control — timely v4",   0.020,  0.037, 4897, 1, "DE",
  "Karing2023", "S5 vs Control — timely v4",   0.103,  0.035, 4897, 1, "DE",
  "Karing2023", "S5 vs Control — Total # of vaccines",   0.351,  0.107, 4897, 1, "DE",
  "Karing2023", "S4 vs Control — Total # of vaccines",   0.034,  0.114, 4897, 1, "DE",
) %>%
  mutate(
    t     = beta/se,
    r     = t_to_r(t, n),
    z     = r_to_z(r),
    var_z = z_se(n)^2,
    z_lo  = z - 1.96 * sqrt(var_z),
    z_hi  = z + 1.96 * sqrt(var_z),
    r_lo  = (exp(2*z_lo) - 1)/(exp(2*z_lo) + 1),
    r_hi  = (exp(2*z_hi) - 1)/(exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(karing2023)

# Pallottini (2025) — CARBON OFFSETS VISIBILITY & SOCIAL REWARDS
pallottini_onset <- tribble(
  ~study_id,         ~contrast,           ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Pallottini2025",  "Public vs Private", 0.292,  0.125,  2305, 1, "DE",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(pallottini_onset)

# Carattini et al. (2022) — PEER-TO-PEER SOLAR AND SOCIAL REWARDS

carattini_solar <- tribble(
  ~study_id,       ~contrast,                    ~beta,   ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "Carattini2022", "Green Reports vs No Reports", 0.0006, 0.0002, 176546, 1, "DE",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print(carattini_solar)

# Jee, Karing & Naguib (2025) — KENYA DEWORMING
jee_karing_kenya <- tribble(
  ~study_id,       ~contrast,             ~beta,  ~se,    ~n, ~has_prosocial_outcome, ~decision_format,
  "JeeKaring2025", "Bracelet vs Control", 0.075,  0.029,  9805, 1, "DE",
) %>%
  mutate(
    t      = beta / se,
    r      = t_to_r(t, n),
    z      = r_to_z(r),
    se_z   = z_se(n),
    var_z  = se_z^2,
    z_lo   = z - 1.96 * se_z,
    z_hi   = z + 1.96 * se_z,
    r_lo   = (exp(2*z_lo) - 1) / (exp(2*z_lo) + 1),
    r_hi   = (exp(2*z_hi) - 1) / (exp(2*z_hi) + 1)
  ) %>%
  select(study_id, contrast, beta, n, r, z, var_z, r_lo, r_hi, z_lo, z_hi, has_prosocial_outcome, decision_format)

print (jee_karing_kenya)

# --- put everything in a list ---
data_init <- list(
  political2024    = political2024,
  cares2018        = cares2018,
  andreoni2009     = andreoni2009,
  giving_circles   = giving_circles,
  dgw2009          = dgw2009,
  food2024         = food2024,
  welfare          = welfare,     
  fairtrade        = fairtrade,
  misreporting     = misreporting,
  pwyw_hofmann     = pwyw_hofmann,
  lying            = lying,
  obs_costs        = obs_costs,
  shame_pride      = shame_pride,
  andreoni_petrie  = andreoni_petrie,
  audience         = audience,
  contributors     = contributors,
  churches         = churches,
  dynamics         = dynamics,
  anti_b           = anti_b,
  incentives       = incentives,
  pwyw_gnezzy      = pwyw_gnezzy,
  rege2004         = rege2004,
  karing2023       = karing2023,
  pallottini_onset = pallottini_onset,
  carattini_solar  = carattini_solar,
  jee_karing_kenya = jee_karing_kenya
)

# --- bind into one joint dataframe ---
data_1 <- bind_rows(data_init, .id = "source_df") %>%
  relocate(source_df, .before = study_id)

print(data_1)





# Anwesenheitseffekte
Werden bestimmte Frage bei Anwesenheit Dritter anders beantwortet?


Packages laden
Vorbereitung der Daten
 ALLBUS <- read_sav("Allbus Gesis Daten/ZA5276_v1-0-0.sav/ZA5276_v1-0-0.sav")
 dim(ALLBUS)

Allbus <- filter(ALLBUS, ALLBUS$year == 1996)
dim(Allbus)
View(Allbus)
Sample.All <- Allbus
attach(Sample.All)
Analyse Art der Anwesenheit
Daten mit verheiratete Befragten und Befragten, die mit ihre Partner leben
Surv <- filter(Sample.All, Sample.All$mstat == 1 | Sample.All$mstat == 2 | Sample.All$mstat == 6)

dim(Surv)
View(Surv)
Daten Filtern nach Geschlecht sex = M
Surv_sexM <-  filter(Surv, Surv$sex == 1)
View(Surv_sexM)
Art der Anwesenheit nach männlichen Geschlecht
Surv_sexM1 <- Surv_sexM %>% summarize(Sum_Ehegatten = sum(hh2kin == 1 | hh2kin == 2, na.rm = TRUE),
                                      Sum_Kinder = sum(hh2kin ==3 | hh2kin == 4, na.rm = TRUE),
                                      Sum_Familieang = sum(between(hh2kin, 5, 15), na.rm = TRUE),
                                      Sum_andere = sum(hh2kin == 16, na.rm = TRUE))
View(Surv_sexM1)
Daten Filtern nach Geschlecht sex = F
Surv_sexF <-  filter(Surv, Surv$sex == 2)
View(Surv_sexF)
Art der Anwesenheit nach weiblichen Geschlecht
Surv_sexF1 <- Surv_sexF %>% summarize(Sum_Ehegatten = sum(hh2kin == 1 | hh2kin == 2, na.rm = TRUE),
                                      Sum_Kinder = sum(hh2kin ==3 | hh2kin == 4, na.rm = TRUE),
                                      Sum_Familieang = sum(between(hh2kin, 5, 15), na.rm = TRUE),
                                      Sum_andere = sum(hh2kin == 16, na.rm = TRUE))
View(Surv_sexF1)
Analyse Ursache der Anwesenheit
Deskriptive Analyse
Ursache.Anwesenheit <- Surv %>% summarize(Mean_sex = mean(sex == 1, na.rm = TRUE), Sd_sex = sd(sex ==                                           1,na.rm = TRUE),
                                          Mean_age = mean(age, na.rm = TRUE), Sd_age = sd(age, na.rm =                                           TRUE),
                                          Mean_HHsize = mean(dh04,na.rm = TRUE), Sd_HHsize =                                                    sd(dh04,na.rm = TRUE),
                                          Mean_HHEink = mean(di06, na.rm = TRUE), Sd_HHEink = sd(di06,                                           na.rm = TRUE),
                                          Mean_working = mean(work == 1, na.rm = TRUE), Sd_working =                                            sd(work == 1, na.rm = TRUE),
                                          Mean_notworking = mean(work == 4 | work == 5, na.rm = TRUE),                                           Sd_notworking = sd(work == 4 | work == 5, na.rm = TRUE),
                                          Mean_workingPart = mean(scwork == 1,na.rm = TRUE),                                                    Sd_workingPart = sd(scwork == 1, na.rm = TRUE),
                                          Mean_notworkingPart = mean(scwork == 4| scwork == 5, na.rm =                                           TRUE), Sd_notworkingPart = sd(scwork == 4| scwork == 5,na.rm                                           = TRUE),
                                          Mean_Parteduc = mean(between(sceduc, 2, 6), na.rm = TRUE),                                            Sd_Parteduc = sd(between(sceduc, 2, 6), na.rm = TRUE),
                                          Mean_IntervDauer = mean(xt10, na.rm = TRUE), Sd_IntervDauer                                           = sd(xt10, na.rm = TRUE),
                                          Mean_IntervGesch = mean(xi02 == 1, na.rm = TRUE),                                                     Sd_IntervGesch = sd(xi02 == 1, na.rm = TRUE),
                                          Mean_GGesunhStand = mean(between(hs01, 1,3), na.rm = TRUE),                                           Sd_GGesunhStand = sd(between(hs01, 1,3), na.rm = TRUE),
                                          Mean_SGesunhStand = mean(between(hs01, 4,5), na.rm = TRUE),                                           Sd_SGesunhStand = sd(between(hs01, 4,5), na.rm = TRUE))
View(Ursache.Anwesenheit)
Prädiktion von der Anwesenheit der Ehegatten mit logitische Regression
bevor müssen die Variable kodiert werden.
kodiert variable Geschlecht (sex)
transform1 <- function(x) { 
  x <- ifelse(x == 1, 1, x) # Male
  x <- ifelse(x == 2, 0, x) # Female
  return(as.numeric(x))
}
sex_neu <- as.factor(transform1(Surv$sex))
Alter in numerik Variable umwandeln (age)
age_neu <- as.numeric(Surv$age)
Variable Haushaltsmitglieder (dh04)
dh04_neu <- as.numeric(Surv$dh04)
Variable Haushaltseinkommen (di06)
transform2 <- function(x){
  x <- ifelse(x <= 13, -1 ,x) # niedrige Einkomemn
  x <- ifelse(between(x, 14, 19),0, x) # mittelere Einkommen
  x <- ifelse(x > 19, 1, x) # Hohe Einkommen
  return(as.factor(x))
}
di06_neu <- transform2(Surv$di06)
Interviewer Geschlecht (xi02)
transform3 <- function(x) { 
  x <- ifelse(x == 1, 1, x) # Male
  x <- ifelse(x == 2, 0, x) # Female
  
}
xi02_neu <- factor(transform1(Surv$xi02), levels = c("0", "1"))
eine berufstätige Mutter kann genauso herzliches und vertrauensvolles Verhältnis zu ihren Kindern finden, wie eine Mutter, die nicht berufstätige Mutter ist (fr01).
transform4 <- function(x) { 
  x <- ifelse(x <= 2, 1, x) # stimme zu
  x <- ifelse(x > 2, 0, x) # stimme nicht zu
  return(as.numeric(x))
}

fr01_neu <- transform4(Surv$fr01)
Frauen sollen ihren Mann bei seiner Karriere helfen (fr02).
transform5 <- function(x) { 
  x <- ifelse(x <= 2, 1, x) # stimme zu
  x <- ifelse(x > 2, 0, x) # stimme nicht zu
  return(as.factor(x))
}
fr02_neu <- transform5(Surv$fr02)
Frau muss zu Hause bleiben und auf die Kinder aufpassen" (fr04a).
transform6 <- function(x) { 
  x <- ifelse(x <= 2, 1, x) # stimme zu
  x <- ifelse(x > 2, 0, x) # stimme nicht zu
  return(as.factor(x))
}
fr04a_neu <- transform6(Surv$fr04a)
variable erwerbtätige Befragte (work)
transform7 <- function(x) { 
  x <- ifelse(x <= 2, 1, x) # erwerbstätig
  x <- ifelse(x > 2, 0, x) # nicht erwerbstätig
  return(as.numeric(x))
}
work_neu <- transform7(Surv$work)
work_neu <- factor(work_neu, levels = c("0","1"))
Variable erwerbtätige ehepartner (scwork)
transform8 <- function(x) { 
  x <- ifelse(x <= 2, 1, x) # erwerbstätg
  x <- ifelse(x > 2, 0, x) # nicht erwerbstätig
  return(as.numeric(x))
}
scwork_neu <- transform8(Surv$scwork)
scwork_neu <- factor(scwork_neu, levels = c("0","1"))
Variable ehepartner Bildung (sceduc)
transform9 <- function(x) { 
  x <- ifelse(x <= 2, -1, x) # keine Schulabschluss
  x <- ifelse(x == 3, 0, x) # Intermediär qualifikation
  x <- ifelse(x >= 4, 1, x) # hat Schluabschluss
  return(as.numeric(x))
}
sceduc_neu <- transform9(Surv$sceduc)
sceduc_neu <- factor(sceduc_neu, levels = c("-1","0","1"))
Interviewtime in mn (xt10)
xt10_neu <- as.numeric(Surv$xt10)
Anwesenheit der Ehepartner (xs02)
xs02_neu <- factor(Surv$xs02, levels = c("0", "1"))
logistische Regression Modell
modell0 <- glm(xs02_neu ~ work_neu + scwork_neu + xt10_neu + xi02_neu + dh04_neu  + age_neu + 
                 sex_neu  + sceduc_neu + di06_neu , data = Surv, family = "binomial")
summary(modell0)
Effekt auf die Antwort des Befragten
Modell 1 : logistische Regression
Effekt von Partner Anwesenheit (xs02) auf fr01
mod1 <- glm(fr01_neu ~ xs02_neu , data = Surv, family = "binomial")
summary(mod1)
Effekt von Partner Anwesenheit (xs02) auf fr02
mod2 <- glm(fr02_neu ~ xs02_neu , data = Surv, family = "binomial")
summary(mod2)
Effekt von Partner Anwesenheit (xs02) auf fr04a
mod3 <- glm(fr04a_neu ~ xs02_neu , data = Surv, family = "binomial")
summary(mod3)
Effekte von Selektion Variable auf fr01
modell1 <- glm(fr01_neu ~ work_neu + scwork_neu + xt10_neu + xi02_neu + dh04_neu  + age_neu + 
                 sex_neu  + sceduc_neu + di06_neu , 
               data = Surv, family = "binomial")
summary(modell1)
Effekte von Selektion Variable auf fr02
modell2 <- glm(fr02_neu ~ work_neu + scwork_neu + xt10_neu + xi02_neu + dh04_neu  + age_neu + 
                 sex_neu  + sceduc_neu + di06_neu, 
               data = Surv, family = "binomial")
summary(modell2)
Effekte von Selektion Variable auf fr04a
modell3 <- glm(fr04a_neu ~ work_neu + scwork_neu + xt10_neu + xi02_neu + dh04_neu  + age_neu + 
                 sex_neu  + sceduc_neu + di06_neu , 
               data = Surv, family = "binomial")
summary(modell3)


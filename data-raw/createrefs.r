(load("/media/mandy/Volume/transcend/R/packages/childsds/data/kroref.rda"))
tmpbmi <- kroref@ref$bmi
tmpbmi <- lapply(tmpbmi, function(x) x[x$age >= 0,])
tmpbmi <- lapply(tmpbmi, function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})


kro.bmi <- new("ParTab",
               item = "bmi",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpbmi
               )

tmpheight <- lapply(kroref@ref$height,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kro.height <- new("ParTab",
                  item = "height",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpheight
                  )

tmpweight <- lapply(kroref@ref$weight,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})


kro.weight <- new("ParTab",
                  item = "weight",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpweight
                  )

tmpwaist <- lapply(kroref@ref$waist,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kro.waist <- new("ParTab",
                  item = "waist",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpwaist
                 )


kro.ref <- new("RefGroup",
               name = "Kromeyer-Hauschild",
               refs = list(bmi = kro.bmi,
                           height = kro.height,
                           weight = kro.weight,
                           waist = kro.waist),
               citations = list(
                   "Kromeyer-Hauschild et al. Percentiles of body mass index in children and adolescents evaluated from different regional German studies"
                   ),
               info = list("Reference values 0-18 years of age Kromeyer-Hausschild",
                           "18-92.5 Mikrozensus",
                            "Correction for gestational age: Voigt"))



dir("/media/mandy/Volume/transcend/R/packages/childsds/data")
(load("/media/mandy/Volume/transcend/R/packages/childsds/data/cdcref.rda"))

[1] "bmi"             "height"          "headcircum"      "height2"        
[5] "weigth"          "weigthforlength" "weigthforstat"

tmpbmi <- lapply(cdc.ref@ref$bmi,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.bmi <- new("ParTab",
               item = "bmi",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpbmi
               )


tmpheight <- lapply(cdc.ref@ref$height,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.height <- new("ParTab",
               item = "height",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpheight
               )


tmpheadcircum <- lapply(cdc.ref@ref$headcircum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.headcircum <- new("ParTab",
                      item = "hc",
                      dist = list(male = "BCCG", female = "BCCG"),
                      params = tmpheadcircum
                      )


tmpheight2 <- lapply(cdc.ref@ref$height2,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.height2 <- new("ParTab",
               item = "height2",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpheight2
               )

tmpweigth <- lapply(cdc.ref@ref$weigth,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.weight <- new("ParTab",
               item = "weight",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpweigth
               )



tmpwfl <- lapply(cdc.ref@ref$weigthforlength,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.weigthforlength <- new("ParTab",
               item = "wfl",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpwfl
               )

tmpweigthforstat <- lapply(cdc.ref@ref$weigthforstat,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

cdc.weigthforstat <- new("ParTab",
               item = "wfl2",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpweigthforstat
               )



cdc.ref <- new("RefGroup",
               name = "CDC",
               refs = list(bmi = cdc.bmi,
                           height0_3 = cdc.height,
                           height2_20 = cdc.height2,
                           weight = cdc.weight,
                           hc = cdc.headcircum,
                           wfl = cdc.weigthforlength,
                           wfl2 = cdc.weigthforstat),
               citations = list(
                   "Flegal, Katherine M., and T. J. Cole. Construction of LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Hational health statitics reports 63."
                   ),
               info = list("hc - headcircumference, wfl - weight for length",
                           "wfl and wfls - age must refer to the length variable; the function gives the sds for a given weight conditional on height"
                           ))


library(readxl)
w2 <- read_excel("data-raw/wtage_CDC.xlsx")
w2$Sex <- factor(w2$Sex, levels = 1:2, labels = c("male","female"))
w2$age <- w2$Agemos/12
w2 <- split(w2, w2$Sex)

library(dplyr)

tmpweigth2 <- lapply(w2,function(x){
    x <- rename(x, mu = M, sigma = S, nu = L)
    x <- select(x, age, mu, sigma, nu)
    x
})



cdc.weight2 <- new("ParTab",
               item = "weight2_20",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpweigth2
               )


tprefs <- cdc.ref@refs
tprefs$weight2_20 <- cdc.weight2





cdc.ref <- new("RefGroup",
               name = "CDC",
               refs = tprefs,
               citations = list(
                   "Flegal, Katherine M., and T. J. Cole. Construction of LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Hational health statitics reports 63."
                   ),
               info = list("hc - headcircumference, wfl - weight for length",
                           "wfl and wfls - age must refer to the length variable; the function gives the sds for a given weight conditional on height"
                           ))

cdc.ref@info <- list(
    "bmi - bmi",
    "height0_3 - height 0 - 3 years old",
    "height2_20 - height 2- 20 years old",
    "weight -  - weight 0 - 3 years old",
    "weight2_20 - weight 2- 20 years old",
    "hc - headcircumference",
    "wfl - weight for length",
    "wfl and wfls - age must refer to the length variable; the function gives the sds for a given weight conditional on height"
)


devtools::use_data(cdc.ref, overwrite = T)


(load("/media/mandy/Volume/transcend/R/packages/childsds/data/kiggsref.rda"))

tmpbmi <- kiggsref@ref$bmi
tmpbmi <- lapply(tmpbmi, function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.bmi <- new("ParTab",
               item = "bmi",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpbmi
               )

tmpheight <- lapply(kiggsref@ref$height,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.height <- new("ParTab",
                  item = "height",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpheight
                  )

tmpweight <- lapply(kiggsref@ref$weight,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})


kiggs.weight <- new("ParTab",
                  item = "weight",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpweight
                  )

tmpwaist <- lapply(kiggsref@ref$waist,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.waist <- new("ParTab",
                  item = "waist",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpwaist
                 )


tmphip <- lapply(kiggsref@ref$hip,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.hip <- new("ParTab",
                 item = "hip",
                 dist = list(male = "BCCG", female = "BCCG"),
                 params = tmphip
                 )

tmpwhr <- lapply(kiggsref@ref$whr,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.whr <- new("ParTab",
                  item = "whr",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpwhr
                 )

tmpwhtr <- lapply(kiggsref@ref$whtr,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.whtr <- new("ParTab",
                  item = "whtr",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpwhtr
                 )


tmpbodyfat <- lapply(kiggsref@ref$bodyfat,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.bodyfat <- new("ParTab",
                  item = "bodyfat",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpbodyfat
                 )

tmpskinfoldsum <- lapply(kiggsref@ref$skinfoldsum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.skinfoldsum <- new("ParTab",
                  item = "skinfoldsum",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpskinfoldsum
                 )

tmptricepsskinfold <- lapply(kiggsref@ref$tricepsskinfold,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.tricepsskinfold <- new("ParTab",
                  item = "sftriceps",
                 dist = list(male = "BCCG", female = "BCCG"),
                 params = tmptricepsskinfold
                 )

tmphc <- lapply(kiggsref@ref$headcircum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

kiggs.hc <- new("ParTab",
                  item = "hc",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmphc
                 )

1] "bmi"             "height"          "weight"          "hip"            
 [5] "headcircum"      "waist"           "whr"             "whtr"           
[9] "bodyfat"         "skinfoldsum"     "tricepsskinfold"


kiggs.ref <- new("RefGroup",
                 name = "kiggs",
                 refs = list(bmi = kiggs.bmi,
                             height = kiggs.height,
                             weight = kiggs.weight,
                             hip = kiggs.hip,
                             waist = kiggs.waist,
                             whr = kiggs.whr,
                             whtr = kiggs.whtr,
                             bodyfat = kiggs.bodyfat,
                             skinfoldsum = kiggs.skinfoldsum,
                             sftriceps = kiggs.tricepsskinfold,
                             hc = kiggs.hc
                             ),
               citations = list(
                   "Referenzperzentile für anthropometrische Maßzahlen und Blutdruck aus KiGGS 2003-2006, Robert Koch Institut; link: http://www.rki.de/DE/Content/Gesundheitsmonitoring/Gesundheitsberichterstattung/GBEDownloadsB/KiGGS_Referenzperzentile.html"),
               info = list())


dir("/media/mandy/Volume/transcend/R/packages/childsds/data")
(load("/media/mandy/Volume/transcend/R/packages/childsds/data/ukwhoref.rda"))


tmpbmi <- ukwhoref@ref$bmi
tmpbmi <- lapply(tmpbmi, function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

ukwho.bmi <- new("ParTab",
               item = "bmi",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpbmi
               )

tmpheight <- lapply(ukwhoref@ref$height,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

ukwho.height <- new("ParTab",
                  item = "height",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpheight
                  )

tmpweight <- lapply(ukwhoref@ref$weight,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})


ukwho.weight <- new("ParTab",
                  item = "weight",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpweight
                  )

tmphc <- lapply(ukwhoref@ref$headcircum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

ukwho.hc <- new("ParTab",
                  item = "hc",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmphc
                 )

ukwho.ref <- new("RefGroup",
                 name = "ukwho",
                 refs = list(bmi = ukwho.bmi,
                             height = ukwho.height,
                             weight = ukwho.weight,
                             hc = ukwho.hc
                             ),
               citations = list(
                   "Wright, Charlotte M., et a,Practice pointer: Using the new UK-WHO growth charts. British Medical Journal 340.c1140 (2010): 647-650.\nPreterm British 1990, 0-4 WHO2006, 4-18 British1990"),
               info = list())

dir("/media/mandy/Volume/transcend/R/packages/childsds/data")
(load("/media/mandy/Volume/transcend/R/packages/childsds/data/whoref.rda"))


tmpbmi <- whoref@ref$bmi
tmpbmi <- lapply(tmpbmi, function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.bmi <- new("ParTab",
               item = "bmi",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpbmi
               )

tmpheight <- lapply(whoref@ref$height,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.height <- new("ParTab",
                  item = "height",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpheight
                  )

tmpweight <- lapply(whoref@ref$weight,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})


who.weight <- new("ParTab",
                  item = "weight",
                  dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpweight
                  )

tmphc <- lapply(whoref@ref$headcircum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.hc <- new("ParTab",
                  item = "hc",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmphc
              )


tmparmc <- lapply(whoref@ref$armcircum,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.armc <- new("ParTab",
                  item = "armc",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmparmc
                )


tmpsfsubscap <- lapply(whoref@ref$subskin,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.sfsubscap <- new("ParTab",
                  item = "sfsubscap",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpsfsubscap
                 )


tmpsfsubscap <- lapply(whoref@ref$subskin,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.sfsubscap <- new("ParTab",
                  item = "sfsubscap",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpsfsubscap
                 )



tmpsftriceps <- lapply(whoref@ref$triskin,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.sftriceps <- new("ParTab",
                  item = "sftriceps",
                 dist = list(male = "BCCG", female = "BCCG"),
                  params = tmpsftriceps
                 )

tmpwfh <- lapply(whoref@ref$wfh,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.wfl <- new("ParTab",
               item = "wfl",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpwfh
               )


tmpwfl2 <- lapply(whoref@ref$wfl,function(x){
    x <- rename(x, mu = m, sigma = s, nu = l)
    x
})

who.wfl2 <- new("ParTab",
               item = "wfl2",
               dist = list(male = "BCCG", female = "BCCG"),
               params = tmpwfl2
              )

who.ref <- new("RefGroup",
               name = "who",
               refs = list(bmi = who.bmi,
                           height = who.height,
                           weight = who.weight,
                           hc = who.hc,
                           wfl = who.wfl,
                           wfl2 = who.wfl2,
                           sfsubscap = who.sfsubscap,
                           sftriceps = who.sftriceps,
                           armc = who.armc),
               citations = list(
                   "de Onis, M., Onyango, A., Borghi, E., Siyam, A., Blossner, M., & Lutter, C. (2012). Worldwide implementation of the WHO child growth standards. Public Health Nutr, 12, 1-8.",
                   "Onis, M. WHO child growth standards: length/height for age, weight-for-age, weight-for-length, weight-for-height and body mass index-for-age, methods and development. Geneva: WHO press",
                   "de Onis, M. WHO Child Growth Standards based on length/height, weight and age. Acta paediatrica 95, 76–85 (2006)"

                   ),
               info = list("hc - headcircumference",
                           "wfl - weight for length",
                           "armc - mid-upper arm circumference",
                           "sfsubscap - subscapular skinfold (mm)",
                           "sftriceps - triceps skinfold (mm)", 
                           "wfl and wfl2 - age must refer to the length variable; the function gives the sds for a given weight conditional on height"
                           ))

lits <- list(
                   "de Onis, M., Onyango, A., Borghi, E., Siyam, A., Blossner, M., & Lutter, C. (2012). Worldwide implementation of the WHO child growth standards. Public Health Nutr, 12, 1-8.",
                   "Onis, M. WHO child growth standards: length/height for age, weight-for-age, weight-for-length, weight-for-height and body mass index-for-age, methods and development. Geneva: WHO press",
                   "de Onis, M. WHO Child Growth Standards based on length/height, weight and age. Acta paediatrica 95, 76–85 (2006)"

                   )


who.ref@citations <- lits

devtools::use_data(who.ref, overwrite = T)


(load("../HDL.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.hdl <- new("ParTab",
               item = "hdl",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )

(load("../Cholesterin.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.chol <- new("ParTab",
               item = "chol",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )


(load("../APoA.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.apoa <- new("ParTab",
               item = "apoA",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )



(load("../ApoB.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.apob <- new("ParTab",
               item = "apoB",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )



(load("../LDL.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.ldl <- new("ParTab",
               item = "ldl",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )


(load("../Triglycerin.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


ads.trig <- new("ParTab",
               item = "trig",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )

lipids.ref <- new("RefGroup",
               name = "lipids",
               refs = list(hdl = ads.hdl,
                           ldl = ads.ldl,
                           trig = ads.trig,
                           chol = ads.chol,
                           apoA = ads.apoa,
                           apoB = ads.apob),
              citations = list(
                   "Dathan-Stumpf, A. et al. Pediatric reference data of serum lipids and prevalence of dyslipidemia: Results from a population-based cohort in Germany. Clinical Biochemistry 49, 740–749 (2016).",
                  "Dathan-Stumpf, A. et al. Serum lipid levels were related to socio-demographic characteristics in a German population-based child cohort. Acta Paediatr 105, e360–e367 (2016)."),
              info = list("hdl - HDL Cholesterol (mmol/l)",
                          "ldl - LDL Cholesterol (mmol/l)",
                          "chol - total Cholesterol (mmol/l)",
                          "trig - tryglicerides (mmol/l)",
                          "apoA - apolipoproteins A1 (g/l)",
                          "apoB - apolipoproteins B (g/l)"
                           ))

devtools::use_data(lipids.ref)


(load("../LAB_FERR_S_NUM_VALUEresults.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


rg.ferr <- new("ParTab",
               item = "ferr",
               dist = list(male = "BCPEo", female = "BCPEo"),
               params = list(male = boys,
                             female = girls)
               )


(load("../LAB_GBB_HGBK_E_NUM_VALUEresults.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


rg.hgb <- new("ParTab",
               item = "hgb",
             dist = list(male = "BCPEo", female = "BCPEo"),
             params = list(male = boys,
                           female = girls)
             )


(load("../LAB_GBB_RETI_E_NUM_VALUEresults.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


rg.reti <- new("ParTab",
               item = "reti",
             dist = list(male = "BCPEo", female = "BCPEo"),
             params = list(male = boys,
                           female = girls)
             )


(load("../LAB_TRF_S_NUM_VALUEresults.rdata"))

boys <- unique(perc.sum.boys[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])
girls <- unique(perc.sum.girls[,c("age","mean.mu","mean.sigma","mean.nu","mean.tau")])

names(boys) <- names(girls) <- c("age","mu","sigma","nu","tau")


rg.tfr <- new("ParTab",
               item = "tfr",
             dist = list(male = "BCPEo", female = "BCPEo"),
             params = list(male = boys,
                           female = girls)
             )





iron.ref <- new("RefGroup",
                name = "iron-related blood parameters",
                refs = list(ferr = rg.ferr,
                            hgb = rg.hgb,
                            reti = rg.reti,
                            tfr = rg.tfr),
              citations = list(
                  "Rieger, K. et al. Reference intervals for iron-related blood parameters: results from a population-based cohort study (LIFE Child). LaboratoriumsMedizin 40, (2016)."
              ),
              info = list("ferr - Ferritin (ng/mL)",
                          "hgb - Hemoglobin (mmol/L)",
                          "reti - reticulocytes (per 1000 erythrocytes)",
                          "tfr - Transferrin (g/L)"
                           ))

devtools::use_data(iron.ref)


## dutch sitting

dat <- read.table("data-raw/dutch.txt", header = T)

tmpdat <- split(dat, dat$sex)

tmpsh <- lapply(tmpdat,function(x){
    x <- select(x, age, sh.l, sh.m, sh.s)
    x <- rename(x, mu = sh.m, sigma = sh.s, nu = sh.l)
    x
})

sitheight <- new("ParTab",
                 item = "sitheight",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmpsh
                 )


tmpleg <- lapply(tmpdat,function(x){
    x <- select(x, age, ll.l, ll.m, ll.s)
    x <- rename(x, mu = ll.m, sigma = ll.s, nu = ll.l)
    x
})

leglength <- new("ParTab",
                 item = "leglength",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmpleg
                 )

tmpshh <- lapply(tmpdat,function(x){
    x <- select(x, age, shh.l, shh.m, shh.s)
    x <- rename(x, mu = shh.m, sigma = shh.s, nu = shh.l)
    x
})

ratsith <- new("ParTab",
               item = "sitoverheight",
               dist = list(male = "BCCGo", female = "BCCGo"),
               params = tmpshh
               )


fredriks05.ref <- new("RefGroup",
                    name = "dutch sitting height and leg length",
                    refs = list(sitt = sitheight,
                                leglength = leglength,
                                sittoverheight = ratsith),
                    citations = list(
                  "Fredriks, A. M. et al. Nationwide age references for sitting height, leg length, and sitting height/height ratio, and their diagnostic value for disproportionate growth disorders. Archives of Disease in Childhood 90, 807–812 (2005)"
                  ),
              info = list("sitt - sitting height",
                          "leglength - leg length",
                          "sittoverheight - sitting height/height ratio" 
                           ))

devtools::use_data(fredriks05.ref)

## china wong
library(magrittr)
chinaheight <- read.table("data-raw/wongchinaheight.txt", header = T, skip  = 1, sep = "\t")

chinaheight %<>% select(matches("age|male"))

chinaheight <- as.data.frame(reshape(chinaheight, direction='long', 
                                     varying=c("male_L","male_M","male_S",
                                               "female_L","female_M","female_S"), 
                                     timevar='sex',
                                     sep = "_",
                                     times=c('male', 'female'),
                                     v.names=c('L', 'M', 'S'),
                                     idvar=c('age')))

attr(chinaheight, "reshapeLong") <- NULL

chinaheight$age <- chinaheight$agem/12
chinaheight <- split(chinaheight, chinaheight$sex)


tmpheight <- lapply(chinaheight,function(x){
    x <- select(x, age, L, M, S)
    x <- rename(x, mu = M, sigma = S, nu = L)
    x
})


tmpheight <- new("ParTab",
                 item = "height",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmpheight
               )


chinaweight <- read.table("data-raw/wongchinaweight.txt", header = T, skip  = 1, sep = "\t")

chinaweight %<>% select(matches("age|male"))

chinaweight <- as.data.frame(reshape(chinaweight, direction='long', 
                                     varying=c("male_L","male_M","male_S",
                                               "female_L","female_M","female_S"), 
                                     timevar='sex',
                                     sep = "_",
                                     times=c('male', 'female'),
                                     v.names=c('L', 'M', 'S'),
                                     idvar=c('age')))

attr(chinaweight, "reshapeLong") <- NULL

chinaweight$age <- chinaweight$agem/12
chinaweight <- split(chinaweight, chinaweight$sex)


tmpweight <- lapply(chinaweight,function(x){
    x <- select(x, age, L, M, S)
    x <- rename(x, mu = M, sigma = S, nu = L)
    x
})


tmpweight <- new("ParTab",
                 item = "weight",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmpweight
               )



chinabmi <- read.table("data-raw/wongchinabmi.txt", header = T, skip  = 1, sep = "\t")

chinabmi %<>% select(matches("age|male"))

chinabmi <- as.data.frame(reshape(chinabmi, direction='long', 
                                     varying=c("male_L","male_M","male_S",
                                               "female_L","female_M","female_S"), 
                                     timevar='sex',
                                     sep = "_",
                                     times=c('male', 'female'),
                                     v.names=c('L', 'M', 'S'),
                                     idvar=c('age')))

attr(chinabmi, "reshapeLong") <- NULL

chinabmi$age <- chinabmi$agem/12
chinabmi <- split(chinabmi, chinabmi$sex)


tmpbmi <- lapply(chinabmi,function(x){
    x <- select(x, age, L, M, S)
    x <- rename(x, mu = M, sigma = S, nu = L)
    x
})


tmpbmi <- new("ParTab",
                 item = "bmi",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmpbmi
               )



chinahc <- read.table("data-raw/wongchinaheadcirc.txt", header = T, skip  = 1, sep = "\t")

chinahc %<>% select(matches("age|male"))

chinahc <- as.data.frame(reshape(chinahc, direction='long', 
                                     varying=c("male_L","male_M","male_S",
                                               "female_L","female_M","female_S"), 
                                     timevar='sex',
                                     sep = "_",
                                     times=c('male', 'female'),
                                     v.names=c('L', 'M', 'S'),
                                     idvar=c('age')))

attr(chinahc, "reshapeLong") <- NULL

chinahc$age <- chinahc$agem/12
chinahc <- split(chinahc, chinahc$sex)


tmphc <- lapply(chinahc,function(x){
    x <- select(x, age, L, M, S)
    x <- rename(x, mu = M, sigma = S, nu = L)
    x
})


tmphc <- new("ParTab",
                 item = "hc",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = tmphc
               )





zong13.ref <- new("RefGroup",
                  name = "dutch sitting height and leg length",
                  refs = list(height = tmpheight,
                              weight = tmpweight,
                              bmi = tmpbmi,
                              hc = tmphc),
                  citations = list(
                      "Zong, X.-N. & Li, H. Construction of a New Growth References for China Based on Urban Chinese Children: Comparison with the WHO Growth Standards. PLOS ONE 8, e59569 (2013)."
                  ),
                  info = list("height - height",
                              "weight - weight",
                              "bmi - bmi",
                              "hc - head circumference"
                              ))

devtools::use_data(zong13.ref)

library(AGD)

nl4.hgt <- nl4.hgt[nl4.hgt$x < 100,]
nl4.hgt$sex <- factor(nl4.hgt$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

heightN <- filter(nl4.hgt, sub == "N")

heightN <- split(heightN, heightN$sex)


heightN <- lapply(heightN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


heightN <- new("ParTab",
                 item = "heightN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = heightN
               )


heightT <- filter(nl4.hgt, sub == "T")

heightT <- split(heightT, heightT$sex)


heightT <- lapply(heightT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


heightT <- new("ParTab",
                 item = "heightT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = heightT
               )


heightM <- filter(nl4.hgt, sub == "M")

heightM <- split(heightM, heightM$sex)


heightM <- lapply(heightM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



heightM <- new("ParTab",
                 item = "heightM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = heightM
               )





## nederland weights


nl4.wgt <- nl4.wgt[nl4.wgt$x < 100,]
nl4.wgt$sex <- factor(nl4.wgt$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

weightN <- filter(nl4.hgt, sub == "N")

weightN <- split(weightN, weightN$sex)


weightN <- lapply(weightN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


weightN <- new("ParTab",
                 item = "weightN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = weightN
               )


weightT <- filter(nl4.hgt, sub == "T")

weightT <- split(weightT, weightT$sex)


weightT <- lapply(weightT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


weightT <- new("ParTab",
                 item = "weightT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = weightT
               )


weightM <- filter(nl4.hgt, sub == "M")

weightM <- split(weightM, weightM$sex)


weightM <- lapply(weightM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



weightM <- new("ParTab",
                 item = "weightM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = weightM
               )


## nederlands bmi

nl4.bmi <- nl4.bmi[nl4.bmi$x < 100,]
nl4.bmi$sex <- factor(nl4.bmi$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

bmiN <- filter(nl4.bmi, sub == "N")

bmiN <- split(bmiN, bmiN$sex)


bmiN <- lapply(bmiN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


bmiN <- new("ParTab",
                 item = "bmiN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = bmiN
               )


bmiT <- filter(nl4.hgt, sub == "T")

bmiT <- split(bmiT, bmiT$sex)


bmiT <- lapply(bmiT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


bmiT <- new("ParTab",
                 item = "bmiT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = bmiT
               )


bmiM <- filter(nl4.hgt, sub == "T")

bmiM <- split(bmiM, bmiM$sex)


bmiM <- lapply(bmiM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



bmiM <- new("ParTab",
                 item = "bmiM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = bmiM
               )

## nederlands headcircum


nl4.hdc <- nl4.hdc[nl4.hdc$x < 100,]
nl4.hdc$sex <- factor(nl4.hdc$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

hdcN <- filter(nl4.hdc, sub == "N")

hdcN <- split(hdcN, hdcN$sex)


hdcN <- lapply(hdcN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


hdcN <- new("ParTab",
                 item = "hdcN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hdcN
               )


hdcT <- filter(nl4.hgt, sub == "T")

hdcT <- split(hdcT, hdcT$sex)


hdcT <- lapply(hdcT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


hdcT <- new("ParTab",
                 item = "hdcT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hdcT
               )


hdcM <- filter(nl4.hgt, sub == "T")

hdcM <- split(hdcM, hdcM$sex)


hdcM <- lapply(hdcM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



hdcM <- new("ParTab",
                 item = "hdcM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hdcM
               )


## nederlands hip 

nl4.hip <- nl4.hip[nl4.hip$x < 100,]
nl4.hip$sex <- factor(nl4.hip$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

hipN <- filter(nl4.hip, sub == "N")

hipN <- split(hipN, hipN$sex)


hipN <- lapply(hipN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


hipN <- new("ParTab",
                 item = "hipN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hipN
               )


hipT <- filter(nl4.hgt, sub == "T")

hipT <- split(hipT, hipT$sex)


hipT <- lapply(hipT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


hipT <- new("ParTab",
                 item = "hipT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hipT
               )


hipM <- filter(nl4.hgt, sub == "T")

hipM <- split(hipM, hipM$sex)


hipM <- lapply(hipM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



hipM <- new("ParTab",
                 item = "hipM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = hipM
               )



## nederlands waist

nl4.wst <- nl4.wst[nl4.wst$x < 100,]
nl4.wst$sex <- factor(nl4.wst$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

wstN <- filter(nl4.wst, sub == "N")

wstN <- split(wstN, wstN$sex)


wstN <- lapply(wstN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


wstN <- new("ParTab",
                 item = "wstN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = wstN
               )


wstT <- filter(nl4.hgt, sub == "T")

wstT <- split(wstT, wstT$sex)


wstT <- lapply(wstT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


wstT <- new("ParTab",
                 item = "wstT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = wstT
               )


wstM <- filter(nl4.hgt, sub == "T")

wstM <- split(wstM, wstM$sex)


wstM <- lapply(wstM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



wstM <- new("ParTab",
                 item = "wstM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = wstM
               )

## nederlands waist hip
nl4.whr <- nl4.whr[nl4.whr$x < 100,]
nl4.whr$sex <- factor(nl4.whr$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

whrN <- filter(nl4.whr, sub == "N")

whrN <- split(whrN, whrN$sex)


whrN <- lapply(whrN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


whrN <- new("ParTab",
                 item = "whrN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = whrN
               )


whrT <- filter(nl4.hgt, sub == "T")

whrT <- split(whrT, whrT$sex)


whrT <- lapply(whrT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


whrT <- new("ParTab",
                 item = "whrT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = whrT
               )


whrM <- filter(nl4.hgt, sub == "T")

whrM <- split(whrM, whrM$sex)


whrM <- lapply(whrM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



whrM <- new("ParTab",
                 item = "whrM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = whrM
               )


## nederlands sitting height

nl4.sit <- nl4.sit[nl4.sit$x < 100,]
nl4.sit$sex <- factor(nl4.sit$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

sitN <- filter(nl4.sit, sub == "N")

sitN <- split(sitN, sitN$sex)


sitN <- lapply(sitN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


sitN <- new("ParTab",
                 item = "sitN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = sitN
               )


sitT <- filter(nl4.hgt, sub == "T")

sitT <- split(sitT, sitT$sex)


sitT <- lapply(sitT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


sitT <- new("ParTab",
                 item = "sitT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = sitT
               )


sitM <- filter(nl4.hgt, sub == "T")

sitM <- split(sitM, sitM$sex)


sitM <- lapply(sitM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



sitM <- new("ParTab",
                 item = "sitM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = sitM
               )


## nederlands shh
nl4.shh <- nl4.shh[nl4.shh$x < 100,]
nl4.shh$sex <- factor(nl4.shh$sex,
                      levels = c("M","F"),
                      labels = c("male","female"))

shhN <- filter(nl4.shh, sub == "N")

shhN <- split(shhN, shhN$sex)


shhN <- lapply(shhN,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


shhN <- new("ParTab",
                 item = "shhN",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = shhN
               )


shhT <- filter(nl4.hgt, sub == "T")

shhT <- split(shhT, shhT$sex)


shhT <- lapply(shhT,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})


shhT <- new("ParTab",
                 item = "shhT",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = shhT
               )


shhM <- filter(nl4.hgt, sub == "T")

shhM <- split(shhM, shhM$sex)


shhM <- lapply(shhM,function(x){
    x <- select(x, x, L, M, S)
    x <- rename(x, age = x, mu = M, sigma = S, nu = L)
    x
})



shhM <- new("ParTab",
                 item = "shhM",
                 dist = list(male = "BCCGo", female = "BCCGo"),
                 params = shhM
               )

nl4.ref <- new("RefGroup",
               name = "dutch sitting height and leg length",
               refs = list(heightN = heightN,
                           heightT = heightT,
                           heightM = heightM,
                           weightN = weightN,
                           weightT = weightT,
                           weightM = weightM,
                           bmiN = bmiN,
                           bmiT = bmiT,
                           bmiM = bmiM,
                           hipN = hipN,
                           hipT = hipT,
                           hipM = hipM,
                           wstN = wstN,
                           wstT = wstT,
                           wstM = wstM,
                           whrN = whrN,
                           whrT = whrT,
                           whrM = whrM,
                           sitN = sitN,
                           sitT = sitT,
                           sitM = sitM,
                           shhN = shhN,
                           shhT = shhT,
                           shhM = shhM,
                           hcN  = hdcN,
                           hcT  = hdcT,
                           hcM  = hdcM),
               citations = list(
                   "Fredriks, A. M. et al. Nationwide age references for sitting height, leg length, and sitting height/height ratio, and their diagnostic value for disproportionate growth disorders. Archives of Disease in Childhood 90, 807–812 (2005).",
"Fredriks, A. M. et al. Height, weight, body mass index and pubertal development references for children of Moroccan origin in The Netherlands. Acta Paediatr. 93, 817–824 (2004).",
"Fredriks, A. M. et al. Continuing positive secular growth change in The Netherlands 1955–1997. Pediatric research 47, 316–323 (2000).",
"Fredriks, A. M. et al. Height, weight, body mass index and pubertal development reference values for children of Turkish origin in the Netherlands. Eur. J. Pediatr. 162, 788–793 (2003).",
"Fredriks, A. M., van Buuren, S., Wit, J. M. & Verloove-Vanhorick, S. P. Body index measurements in 1996–7 compared with 1980. Archives of disease in childhood 82, 107–112 (2000).",
"R package: AGD, Stef van Buuren, http://www.stefvanbuuren.nl/"
               ),
               info = list("heightN, heightT, heightM - height for Dutch, Turkish, Morrocon origins resp",
                           "weightN, heightT, heightM - weight for Dutch, Turkish, Morrocon origins resp",
                           "bmiN, bmiT, bmiM - bmi for Dutch, Turkish, Morrocon origins resp",
                           "hcN, hcT, hcM - head circumference for Dutch, Turkish, Morrocon origins resp",
                           "hipN, hipT, hipM - hip circumference for Dutch, Turkish, Morrocon origins resp",
                           "wstN, wstT, wstM - waist circumference for Dutch, Turkish, Morrocon origins resp",
                           "whrN, whrT, whrM - waist to hip ratio for Dutch, Turkish, Morrocon origins resp",                           
                           "sitN, sitT, sitM - sitting height for Dutch, Turkish, Morrocon origins resp",
                           "shhN, shhT, shhM - sitting height/height ration for Dutch, Turkish, Morrocon origins resp"))
                           

    
devtools::use_data(nl4.ref)



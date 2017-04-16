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
                   "de Onis, M., Onyango, A., Borghi, E., Siyam, A., Blossner, M., & Lutter, C. (2012). Worldwide implementation of the WHO child growth standards. Public Health Nutr, 12, 1-8."
                   ),
               info = list("hc - headcircumference",
                           "wfl - weight for length",
                           "armc - mid-upper arm circumference",
                           "sfsubscap - subscapular skinfold (mm)",
                           "sftriceps - triceps skinfold (mm)", 
                           "wfl and wfl2 - age must refer to the length variable; the function gives the sds for a given weight conditional on height"
                           ))

devtools::use_data(who.ref)


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

#library(memisc)
#library(tidyverse)
#library(RM.weights)
#library(survey)
# Functions to simplify the analysis
source(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_moe_complex survey design_rev.R"))
source(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_TermometerFIES2.R"))
source(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_FIEStableNEW.R"))
source(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_fies4w29092021_REV.R"))
#

sav = memisc::spss.system.file(hh_path)
hh <- as.data.frame(memisc::as.data.set(sav), use.value.labels=TRUE)
rm(sav)
hh <- hh[hh$HH17=="INTERVIEWED, WITH CONSENT FOR THE NEXT WAVE" | hh$HH17=="INTERVIEWED, NO CONSENT FOR THE NEXT WAVE",]

hh$hhweightHLnum = hh$HLnum * hh$hhweight
wgt = hh$hhweightHLnum

FIES30D <- subset(hh, select = c(FI1:FI8))
F30D <- matrix(NA,nrow=nrow(FIES30D),ncol=ncol(FIES30D))
F30D[FIES30D=="NO"] <- 0
F30D[FIES30D=="YES"] <- 1
RS30D <- rowSums(F30D)
table(RS30D)
apply(F30D, 2, table)
apply(F30D, 2, function(i) sum(is.na(i))) #NA

load(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_FIES_glob_st.RData"))
colnames(F30D) <- names(fies.global.st)
### Checking items for infit values.
res30D <- RM.w(as.matrix(F30D), write.file = T, country="JAM MICSPlus Wave2 - Table 4.1_unweighted")
cbind(res30D$infit)
res30D_w <- RM.w(as.matrix(F30D),wgt, write.file = T, country="JAM MICSPlus Wave2 - Table 4.1_weighted")
plot(res30D$b/sd(res30D$b),res30D_w$b/sd(res30D_w$b))
abline(0,1)
## preparing classifiers
hh <- 
  hh %>% 
  mutate(
    headage = case_when(
      headage < 18 ~ "<18",
      headage >= 18 & headage <=34 ~ "18-34",
      headage >= 35 & headage <=64 ~ "35-64",
      headage >= 65 & headage <=84 ~ "65-84",
      headage >= 85 & headage <=95 ~ '85+',
      headage >= 98 & headage <=99 ~ 'DK / Missing'),
    
    headage = as.factor(headage))

##hh$headed <- factor(hh$headed,levels = c("Primary or less","Primary","Basic (lower secondary)","Upper secondary","Vocational","College, university"))

classifier = list()
classifier[[1]] = hh$area
classifier[[2]] = hh$degubra
classifier[[3]] = hh$headsex
classifier[[4]] = hh$headage
classifier[[5]] = hh$headed
classifier[[6]] = hh$windex5

png("JAM MICSPlus Wave2 - Table 4.1_Boxplots.png")
par(mfrow=c(2,3))

boxplot(RS30D~hh$area)
boxplot(RS30D~hh$degubra)
boxplot(RS30D~hh$headsex)
boxplot(RS30D~hh$headage)
boxplot(RS30D~hh$headed)
boxplot(RS30D~hh$windex5)

dev.off()

test_res = list()
for (case in levels(hh$windex5)) {
  test_res[[case]] <- RM.w(F30D[hh$windex5 == case,])
}

pdf("JAM MICSPlus Wave2 - Table 4.1_DIF_by_wealth.pdf")
plot(res30D$b/sd(res30D$b),test_res$Poorest$b/sd(test_res$Poorest$b), ylab="Sev by Income",
     xlab="Sev")
points(res30D$b/sd(res30D$b),test_res$Second$b/sd(test_res$Second$b),col=2)
points(res30D$b/sd(res30D$b),test_res$Middle$b/sd(test_res$Middle$b),col=3)
points(res30D$b/sd(res30D$b),test_res$Fourth$b/sd(test_res$Fourth$b),col=4)
points(res30D$b/sd(res30D$b),test_res$Richest$b/sd(test_res$Richest$b),col=5)
abline(0,1)
legend("topleft", col=c(1:5), pch=16, legend=c("Poorest", "Second", "Middle", "Fourth",
                                               "Richest"),
       cex=.6, bty="n")
dev.off()

## Preparing for 4W analysis
fies4w.df=as.data.frame(F30D)
fies4w.df$wt=wgt
fies4w.df$area=hh$area
fies4w.df$degubra=hh$degubra
fies4w.df$headsex=hh$headsex
fies4w.df$headage=hh$headage
fies4w.df$headed=hh$headed
fies4w.df$windex5=hh$windex5
fies4w.df$id=rep(NA, nrow(fies4w.df))
fies4w.df$wt.person=wgt
write.csv(fies4w.df[, c("WORRIED", "HEALTHY", "FEWFOOD", "SKIPPED",
                        "ATELESS", "RUNOUT", "HUNGRY", "WHLDAY",
                        "wt", "wt.person", "area", "windex5", "id")],
          "./JAM MICSPlus Wave2 - Table 4.1_Shiny 4w.csv", row.names = FALSE)

res.df <- fies4w(global.4w.common = c(1:3,6:8),
                 fies.df = fies4w.df[,1:8],
                 UID = rep(1:nrow(fies4w.df)),
                 wt = fies4w.df$wt,
                 wt.person = fies4w.df$wt.person,
                 model = c("dichotomous"),
                 tol = 0.97,
                 max.cor.meth = F,
                 uniqueitems = NULL,
                 disaggregation.df = fies4w.df[, c('area', 'degubra', 'headsex', 
                                                   'headage', 'headed', 'windex5')],
                 surveyName = "JAM MICSPlus Wave2 - Table 4.1_",
                 FIEScatplot = TRUE)


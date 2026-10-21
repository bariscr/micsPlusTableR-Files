# ---- Standalone survey and paths ----
# In the app, its selected paths and survey label are used automatically.
country <- "JAM"
wave <- 2
output_prefix <- paste0(country, " MICSPlus Wave", wave)
.fies_app_paths <- get0(".mics_fies_paths", envir = environment(), inherits = FALSE)
if (is.null(.fies_app_paths)) {
  path_fies <- NULL # NULL: this script's folder; or enter its full path
  hh_path <- file.path("..", "..", paste0(country, "_W", wave, "_hh.sav"))
  path_fies_output <- "../FIES-outputs"
} else {
  path_fies <- .fies_app_paths$inputs
  hh_path <- .fies_app_paths$hh
  path_fies_output <- .fies_app_paths$output
  if (!is.null(.fies_app_paths$survey_label)) {
    output_prefix <- .fies_app_paths$survey_label
  }
}
# Relative data/output paths are resolved from path_fies, never by changing getwd().
if (is.null(path_fies)) {
  .source_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.character(value) && length(value) == 1L) value else NA_character_
  }, character(1))
  .source_files <- .source_files[!is.na(.source_files)]
  .script_file <- if (length(.source_files)) tail(.source_files, 1L) else
    gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), fixed = TRUE)
  if (!length(.script_file)) {
    stop("Source this script, run it with Rscript, or set path_fies to the helper folder.")
  }
  path_fies <- dirname(normalizePath(.script_file, mustWork = TRUE))
}
path_fies <- normalizePath(path_fies, winslash = "/", mustWork = TRUE)
base::sys.source(file.path(path_fies, "FIES-runtime.R"), envir = environment())
fies_load_dependencies(environment())
.fies_paths <- fies_resolve_paths(path_fies, hh_path, path_fies_output)
path_fies <- .fies_paths$inputs
hh_path <- .fies_paths$hh
path_fies_output <- .fies_paths$output

# Helper discovery uses stable suffixes, so changing the survey label does not
# require renaming the supplied helpers or their reference-data file.
for (.helper in c("moe_complex survey design_rev.R", "TermometerFIES2.R",
                  "FIEStableNEW.R", "fies4w29092021_REV.R")) {
  base::sys.source(fies_input_file(path_fies, .helper), envir = environment())
}

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

load(fies_input_file(path_fies, "FIES_glob_st.RData"), envir = environment())
colnames(F30D) <- names(fies.global.st)
### Checking items for infit values.
res30D <- fies_rm_w(output_dir = path_fies_output, as.matrix(F30D), write.file = T, country=paste0(output_prefix, "_unweighted"))
cbind(res30D$infit)
res30D_w <- fies_rm_w(output_dir = path_fies_output, as.matrix(F30D),wgt, write.file = T, country=paste0(output_prefix, "_weighted"))
pdf(file.path(path_fies_output, paste0(output_prefix, "_Weighted comparison.pdf")))
plot(res30D$b/sd(res30D$b),res30D_w$b/sd(res30D_w$b))
abline(0,1)
dev.off()
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

png(file.path(path_fies_output, paste0(output_prefix, "_Boxplots.png")))
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
  test_res[[case]] <- fies_rm_w(output_dir = path_fies_output, F30D[hh$windex5 == case,])
}

pdf(file.path(path_fies_output, paste0(output_prefix, "_DIF_by_wealth.pdf")))
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
          file.path(path_fies_output, paste0(output_prefix, "_Shiny 4w.csv")), row.names = FALSE)

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
                 surveyName = paste0(output_prefix, "_"),
                 FIEScatplot = TRUE)


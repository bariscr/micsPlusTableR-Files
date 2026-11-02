# ---- 1. Standalone settings: explicit paths from the project root ----
hh_path <- here::here("Jamaica (2023-24) Wave 1", "JAM_W1_hh.sav")
path_fies <- here::here("FIES-inputs")
path_fies_output <- here::here("FIES-outputs")
output_prefix <- "JAM MICSPlus Wave1"

# ---- 2. App paths: supplied by run_fies_table() ----
.fies_app_paths <- get0(".mics_fies_paths", envir = environment(), inherits = FALSE)
if (!is.null(.fies_app_paths)) {
  hh_path <- .fies_app_paths$hh
  path_fies <- .fies_app_paths$inputs
  path_fies_output <- .fies_app_paths$output
  if (!is.null(.fies_app_paths$survey_label)) {
    output_prefix <- .fies_app_paths$survey_label
  }
}

# Standalone creates FIES-outputs under the project root if needed.
if (is.null(.fies_app_paths)) {
  dir.create(path_fies_output, recursive = TRUE, showWarnings = FALSE)
}
stopifnot(dir.exists(path_fies_output))

# ---- 3. Required packages ----
library(survey)
library(RM.weights)
library(reshape2)
library(dplyr)
library(ggplot2)

# ---- 4. Exact helper files ----
source(file.path(path_fies, "JAM MICSPlus Wave1 - Table 4.1_moe_complex survey design_rev.R"), local = environment())
source(file.path(path_fies, "JAM MICSPlus Wave1 - Table 4.1_TermometerFIES2.R"), local = environment())
source(file.path(path_fies, "JAM MICSPlus Wave1 - Table 4.1_FIEStableNEW.R"), local = environment())
source(file.path(path_fies, "JAM MICSPlus Wave1 - Table 4.1_fies4w29092021_REV.R"), local = environment())

# ---- 5. Household data, responses and weights ----
sav = memisc::spss.system.file(hh_path)
hh <- as.data.frame(memisc::as.data.set(sav), use.value.labels = TRUE)
rm(sav)
hh <- hh[hh$HH17 == "INTERVIEWED, WITH CONSENT FOR THE NEXT WAVE" | hh$HH17 == "INTERVIEWED, NO CONSENT FOR THE NEXT WAVE", ]

hh$hhweightHLnum = hh$HLnum * hh$hhweight
wgt = hh$hhweightHLnum

FIES30D <- subset(hh, select = c(FI1:FI8))
F30D <- matrix(NA, nrow = nrow(FIES30D), ncol = ncol(FIES30D))
F30D[FIES30D == "NO"] <- 0
F30D[FIES30D == "YES"] <- 1
RS30D <- rowSums(F30D)
table(RS30D)
apply(F30D, 2, table)
apply(F30D, 2, function(i) sum(is.na(i))) # NA

load(file.path(path_fies, "JAM MICSPlus Wave1 - Table 4.1_FIES_glob_st.RData"), envir = environment())
colnames(F30D) <- names(fies.global.st)
# ---- 6. Rasch models and item diagnostics ----
res30D <- RM.w(as.matrix(F30D), write.file = T, country = paste0(output_prefix, "_unweighted"))
cbind(res30D$infit)
res30D_w <- RM.w(as.matrix(F30D), wgt, write.file = T, country = paste0(output_prefix, "_weighted"))
pdf(file.path(path_fies_output, paste0(output_prefix, "_Weighted comparison.pdf")))
plot(res30D$b / sd(res30D$b), res30D_w$b / sd(res30D_w$b))
abline(0, 1)
dev.off()
# ---- 7. Household classifiers ----
hh <-
  hh %>%
  mutate(
    headage = case_when(
      headage < 18 ~ "<18",
      headage >= 18 & headage <= 34 ~ "18-34",
      headage >= 35 & headage <= 64 ~ "35-64",
      headage >= 65 & headage <= 84 ~ "65-84",
      headage >= 85 & headage <= 95 ~ '85+',
      headage >= 98 & headage <= 99 ~ 'DK / Missing'
    ),
    headage = as.factor(headage)
  )

## hh$headed <- factor(hh$headed,levels = c("Primary or less","Primary","Basic (lower secondary)","Upper secondary","Vocational","College, university"))

classifier = list()
classifier[[1]] = hh$area
classifier[[2]] = hh$degubra
classifier[[3]] = hh$headsex
classifier[[4]] = hh$headage
classifier[[5]] = hh$headed
classifier[[6]] = hh$windex5

# ---- 8. Diagnostic plots by household group ----
png(file.path(path_fies_output, paste0(output_prefix, "_Boxplots.png")))
par(mfrow = c(2, 3))

boxplot(RS30D ~ hh$area)
boxplot(RS30D ~ hh$degubra)
boxplot(RS30D ~ hh$headsex)
boxplot(RS30D ~ hh$headage)
boxplot(RS30D ~ hh$headed)
boxplot(RS30D ~ hh$windex5)

dev.off()

test_res = list()
for (case in levels(hh$windex5)) {
  test_res[[case]] <- RM.w(F30D[hh$windex5 == case, ])
}

pdf(file.path(path_fies_output, paste0(output_prefix, "_DIF_by_wealth.pdf")))
plot(res30D$b / sd(res30D$b), test_res$Poorest$b / sd(test_res$Poorest$b),
  ylab = "Sev by Income",
  xlab = "Sev"
)
points(res30D$b / sd(res30D$b), test_res$Second$b / sd(test_res$Second$b), col = 2)
points(res30D$b / sd(res30D$b), test_res$Middle$b / sd(test_res$Middle$b), col = 3)
points(res30D$b / sd(res30D$b), test_res$Fourth$b / sd(test_res$Fourth$b), col = 4)
points(res30D$b / sd(res30D$b), test_res$Richest$b / sd(test_res$Richest$b), col = 5)
abline(0, 1)
legend("topleft",
  col = c(1:5), pch = 16, legend = c(
    "Poorest", "Second", "Middle", "Fourth",
    "Richest"
  ),
  cex = .6, bty = "n"
)
dev.off()

# ---- 9. Prevalence estimates and analysis outputs ----
fies4w.df = as.data.frame(F30D)
fies4w.df$wt = wgt
fies4w.df$area = hh$area
fies4w.df$degubra = hh$degubra
fies4w.df$headsex = hh$headsex
fies4w.df$headage = hh$headage
fies4w.df$headed = hh$headed
fies4w.df$windex5 = hh$windex5
fies4w.df$id = rep(NA, nrow(fies4w.df))
fies4w.df$wt.person = wgt
write.csv(
  fies4w.df[, c(
    "WORRIED", "HEALTHY", "FEWFOOD", "SKIPPED",
    "ATELESS", "RUNOUT", "HUNGRY", "WHLDAY",
    "wt", "wt.person", "area", "windex5", "id"
  )],
  file.path(path_fies_output, paste0(output_prefix, "_Shiny 4w.csv")),
  row.names = FALSE
)

res.df <- fies4w(
  global.4w.common = c(1:3, 6:8),
  fies.df = fies4w.df[, 1:8],
  UID = rep(1:nrow(fies4w.df)),
  wt = fies4w.df$wt,
  wt.person = fies4w.df$wt.person,
  model = c("dichotomous"),
  tol = 0.97,
  # max.cor.meth = F,
  # uniqueitems = c("WORRIED", "HUNGRY"),
  disaggregation.df = fies4w.df[, c(
    'area', 'degubra', 'headsex',
    'headage', 'headed', 'windex5'
  )],
  surveyName = paste0(output_prefix, "_"),
  FIEScatplot = TRUE
)

# ---- 10. Standardized table output for the app ----
fies_result <- local({
  groups <- c(
    "area", "degubra", "headsex", "headage", "headed", "windex5"
  )

  rates <- res.df$PrevalenceRates

  # Eligible households and their household-member counts.
  valid <- complete.cases(F30D)
  members <- as.numeric(hh$HLnum)
  member_weights <- as.numeric(hh$hhweightHLnum)

  if (nrow(hh) != nrow(F30D)) {
    stop("Household records and FIES responses are not aligned.")
  }

  if (any(!is.finite(members[valid])) ||
    any(!is.finite(member_weights[valid]))) {
    stop("Eligible households have missing or invalid member counts/weights.")
  }

  # FIES output contains section headings followed by category rows.
  current_group <- "Total"
  output <- list()

  for (i in seq_len(nrow(rates))) {
    category <- trimws(as.character(rates$Disaggregation[i]))

    if (category %in% groups) {
      current_group <- category
      next
    }

    selected <- valid

    if (current_group != "Total") {
      labels <- trimws(as.character(hh[[current_group]]))
      labels[is.na(labels)] <- "Missing"
      selected <- valid & labels == category
    }

    output[[length(output) + 1L]] <- data.frame(
      group = current_group,
      category = category,
      p_mod = as.numeric(as.character(rates$P_mod[i])),
      p_sev = as.numeric(as.character(rates$P_sev[i])),
      count_w = sum(member_weights[selected]),
      count_unw = sum(members[selected]),
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result
})

# ---- Final step: move the reports written by RM.weights ----
# These are the exact model-report names used above and inside fies4w().
# Edit this list if you enable additional model reports.
report_files <- c(
  paste0("Output", output_prefix, "_unweighted.csv"),
  paste0("Output", output_prefix, "_weighted.csv"),
  paste0("Output", output_prefix, "_.csv"),
  paste0(output_prefix, "__Polytomous_Output.csv")
)
report_files <- report_files[file.exists(report_files)]
if (length(report_files) &&
    normalizePath(getwd()) != normalizePath(path_fies_output)) {
  # Copy first, then remove originals only if every copy succeeded.
  stopifnot(all(file.copy(report_files, path_fies_output, overwrite = TRUE)))
  stopifnot(unlink(report_files) == 0L)
}

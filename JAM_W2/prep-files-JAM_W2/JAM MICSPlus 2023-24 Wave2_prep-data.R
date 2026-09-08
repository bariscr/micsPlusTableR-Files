
# Preparation contract: micsPlusTableR supplies hh_path and hl_path, makes its
# declared imports available, and collects hh, hl, and extra-table objects.
# Do not source external package or function bootstrap scripts from this file.

hh <- haven::read_sav(hh_path) 

hl <- haven::read_sav(hl_path)

# Add HHIDs
hl <- 
  hl |> 
  mutate(HHID = HH0*1000000+HH1*1000+HH2*10+HH0A)

hh <- 
  hh |> 
  mutate(HHID = HH0*1000000+HH1*1000+HH2*10+HH0A) |> 
  mutate(respage_gr = case_when(
    respage %in% c(18:34) ~ "18-34",
    respage %in% c(35:64) ~ "35-64",
    respage %in% c(65:84) ~ "65-84",
    respage >= 85 ~ "85+",
    TRUE ~ NA_character_
  ))

# Add total variables
hh <- hh |> mutate(total = 1)
hl <- hl |> mutate(total = 1)

# from hh to hl -------------------------------------------------
hh_add <- hh |> select(HHID, HLnum, ch117num, headed,
                       stratum, 
                       FI1, FI2, FI3, FI4, FI5, FI6, FI7, FI8,
                       headsex, headage) 

# hlweight and HHID ----------
hl <- 
  hl |> 
  left_join(hh_add, by = c("HHID")) |> 
  mutate(hlweight = hhweight / HLnum) |> 
  mutate(headage_gr = case_when(
    headage %in% c(98) ~ NA,
    between(headage, 0, 17) ~ "0-17",
    between(headage, 18, 34)  ~ "18-34",
    between(headage, 35, 64)  ~ "35-64",
    between(headage, 65, 84)  ~ "65-84",
    headage >= 85  ~ "85+"
    )) |> 
  group_by(HH1, HH2) |> 
  mutate(HLnum15more = sum(if_else(HH17 %in% c(1,0) &
                                   HL7 %in% c(1, NA) & 
                                   HL6 >= 15 & 
                                   HL6 != 98, 1, 0))) |> 
  ungroup() 
# --------------------------------------------------------

# from hl to hh ------------------------------------------
hl_add <- 
  hl |> 
  select(HHID, HLnum15more, hlweight) |> 
  distinct()

hh <- 
  hh |> 
  left_join(hl_add, by = c("HHID")) 
# --------------------------------------------------------


# Tables that are not producible with the common logic in the system, are produced with further code and included within the system.

tables_extra <- c("JW2.4.1", "JW2.6.1", "JW2.6.1a")

# Dictionary for extra tables and their corresponding outputs

extra_tables_dict <- list(
  "JW2.4.1" = "tb_41",
  "JW2.6.1" = "tb_61",
  "JW2.6.1a" = "tb_61a"
)

# Table 6.1
#library(labelled)
#hh <- haven::read_sav(here::here("mics-files/JAM_W2_hh.sav"))

df <- hh |> 
  filter(HH17 %in% c(0,1)) |> 
  select(hhweight, MHP1A, MHP1B, MHP1C, MHP1D, MHP1E, MHP1F, 
         MHP1G, MHP1H, MHP1I, MHP1J, MHP1K, MHP1L,
         area, degubra, respsex, respage_gr, resped, windex5) |> 
  mutate(hhweight = ifelse(is.na(hhweight), 0, hhweight))

# ---- shared labels ----
item_labels <- c(
  "Strongly agree" = 1,
  "Agree" = 2,
  "Neither agree nor disagree" = 3,
  "Disagree" = 4,
  "Strongly disagree" = 5,
  "DK/Missing" = 9
)

# ---- spec: item -> newvar + variable label + direction ----
spec <- list(
  MHP1A = list(new = "lackslfdsc", dir = "pos",
               lab = "One of the main causes of mental health problems is a lack of self-discipline and willpower"),
  MHP1B = list(new = "different", dir = "pos",
               lab = "There is something about people living with mental health problems that makes it easy to tell them apart from people without mental health problems"),
  MHP1C = list(new = "devtolatt", dir = "rev",
               lab = "We need to adopt a far more tolerant attitude toward people living with mehtal health problems in our society"),
  MHP1D = list(new = "dntdessymp", dir = "pos",
               lab = "People living with mental health problems don't deserve our sympathy"),
  MHP1E = list(new = "nextdoor", dir = "pos",
               lab = "I would not want to live next door to someone who has mental health problems"),
  MHP1F = list(new = "resneighb", dir = "pos",
               lab = "It is frightening to think of people with mental health problems living in residential neigbourhoods"),
  MHP1G = list(new = "anyothhp", dir = "rev",
               lab = "Mental health problems are like any other health problems"),
  MHP1H = list(new = "anyone", dir = "rev",
               lab = "Virtually anyone can have mental health problems"),
  MHP1I = list(new = "community", dir = "rev",
               lab = "The best therapy for many peiple with mental health problems is to be part of a community"),
  MHP1J = list(new = "lessdang", dir = "rev",
               lab = "People with mental health problems are far less of a danger than most people suppose"),
  MHP1K = list(new = "responsib", dir = "pos",
               lab = "People with mental health problems should not be given any responsibility"),
  MHP1L = list(new = "babysit", dir = "rev",
               lab = "Most women who were once patients in a hospital for mental health problems can be trusted as babysitters")
)

# ---- apply: recode items + create scored vars + attach labels ----
for (v in names(spec)) {
  s <- spec[[v]]
  newv <- s$new

  # 1) recode DK/Missing on the original item: 8..9 -> 9, else copy
  df[[newv]] <- dplyr::if_else(df[[v]] %in% 8:9, 9, df[[v]])

  # 3) variable labels
  #labelled::var_label(df[[v]]) <- s$lab
  labelled::var_label(df[[newv]]) <- s$lab

  # 4) value labels
  labelled::val_labels(df[[newv]]) <- item_labels
  
}

# build a lookup table from your spec (new variable name + direction + label)
spec_lut <- tibble(
  variable = map_chr(spec, ~ .x$new),
  dir      = map_chr(spec, ~ .x$dir),
  item_lab = map_chr(spec, ~ .x$lab)
)

df_long <- df |>
  select(
    hhweight,
    area, degubra, respsex, respage_gr, resped, windex5,
    anyone, devtolatt, community, anyothhp, lessdang, babysit,
    dntdessymp, nextdoor, resneighb, responsib, lackslfdsc, different
  ) |>
  pivot_longer(
    cols      = -c(hhweight, area, degubra, respsex, respage_gr, resped, windex5),
    names_to  = "variable",
    values_to = "label"
  ) |>
  left_join(spec_lut, by = "variable") |>
  mutate(
    score = case_when(
      label == 1 ~ if_else(dir == "pos", 0,   100),
      label == 2 ~ if_else(dir == "pos", 25,   75),
      label == 3 ~ 50,
      label == 4 ~ if_else(dir == "pos", 75,   25),
      label == 5 ~ if_else(dir == "pos", 100,   0),
      TRUE       ~ NA_real_   # includes 9 (DK/Missing) and NA
    )
  )


# -------------------------
# Table prep
# -------------------------

# Local tabulation helpers for the JAM W2 extra tables. Labelled categories
# retain their survey-code order;
# unlabelled categories retain their first-observed order.
prep_factor <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)

  if (!is.null(labels)) {
    ordered_labels <- trimws(
      names(labels)[order(as.numeric(unname(labels)), na.last = TRUE)]
    )
    labelled_values <- haven::as_factor(x, levels = "labels")
    return(factor(
      trimws(as.character(labelled_values)),
      levels = ordered_labels
    ))
  }

  if (is.factor(x)) return(x)
  factor(x, levels = unique(x[!is.na(x)]))
}

weighted_mean <- function(x, weight) {
  keep <- !is.na(x) & !is.na(weight)
  if (!any(keep)) return(NA_real_)

  denominator <- sum(weight[keep])
  if (!is.finite(denominator) || denominator == 0) return(NA_real_)
  sum(x[keep] * weight[keep]) / denominator
}

count_by_stub <- function(design, group_vars) {
  count_data <- design$variables
  count_weight <- as.numeric(stats::weights(design))

  grouped_counts <- lapply(group_vars, function(group_var) {
    stub_values <- prep_factor(count_data[[group_var]])
    stub_levels <- levels(stub_values)

    tibble::tibble(
      group_cat = stub_levels,
      count_unw = unname(vapply(stub_levels, function(stub) {
        as.numeric(sum(!is.na(stub_values) & stub_values == stub))
      }, numeric(1))),
      count_w = unname(vapply(stub_levels, function(stub) {
        sum(count_weight[!is.na(stub_values) & stub_values == stub])
      }, numeric(1)))
    )
  })

  dplyr::bind_rows(
    tibble::tibble(
      group_cat = "Total",
      count_unw = as.numeric(nrow(count_data)),
      count_w = sum(count_weight)
    ),
    grouped_counts
  )
}

df2 <- survey::svydesign(ids = ~1, weights = ~hhweight, data = df_long)

tb_perc <- local({
  table_data <- df2$variables
  table_weight <- as.numeric(stats::weights(df2))
  group_values <- prep_factor(table_data$variable)
  category_values <- prep_factor(table_data$label)
  group_levels <- levels(group_values)
  category_levels <- levels(category_values)

  percentages <- tidyr::expand_grid(
    group_cat = group_levels,
    category = category_levels
  )
  percentages$value <- vapply(seq_len(nrow(percentages)), function(i) {
    in_group <- !is.na(group_values) &
      group_values == percentages$group_cat[[i]] &
      !is.na(category_values)
    denominator <- sum(table_weight[in_group])
    in_category <- in_group & category_values == percentages$category[[i]]
    100 * sum(table_weight[in_category]) / denominator
  }, numeric(1))

  dplyr::bind_rows(
    percentages,
    tibble::tibble(
      group_cat = group_levels,
      category = "Total",
      value = 100
    )
  ) |>
    tidyr::pivot_wider(names_from = category, values_from = value)
})

tb_score <- local({
  table_data <- df2$variables
  table_weight <- as.numeric(stats::weights(df2))
  group_values <- prep_factor(table_data$variable)
  group_levels <- sort(levels(group_values))

  tibble::tibble(
    group_cat = group_levels,
    score = unname(vapply(group_levels, function(group) {
      in_group <- !is.na(group_values) & group_values == group
      weighted_mean(table_data$score[in_group], table_weight[in_group])
    }, numeric(1)))
  )
})

tb_61 <- tb_perc |>
  left_join(tb_score, by = c("group_cat")) 
  
assign("tb_61", tb_61, envir = .GlobalEnv)

# ----------------------------------------------------------
# Table 6.1a ----
# ----------------------------------------------------------

df_long2 <- df_long |> 
  mutate(variable2 = case_when(
    variable %in% c("anyone", "devtolatt", "community", "anyothhp", "lessdang", "babysit") ~ "tolerance",
    variable %in% c("dntdessymp", "nextdoor", "resneighb", "responsib", "lackslfdsc", "different") ~ "prejudice",
    TRUE ~ variable
  ))

df3 <- survey::svydesign(ids = ~1, weights = ~hhweight, data = df_long2)

score_group_vars <- c(
  "area", "degubra", "respsex", "respage_gr", "resped", "windex5"
)

tb_score2 <- local({
  table_data <- df3$variables
  table_weight <- as.numeric(stats::weights(df3))
  score_group <- prep_factor(table_data$variable2)
  score_group_levels <- sort(levels(score_group))

  make_score_row <- function(category, in_category) {
    scores <- c(
      Total = weighted_mean(
        table_data$score[in_category],
        table_weight[in_category]
      ),
      stats::setNames(vapply(score_group_levels, function(group) {
        selected <- in_category & !is.na(score_group) & score_group == group
        weighted_mean(table_data$score[selected], table_weight[selected])
      }, numeric(1)), score_group_levels)
    )
    tibble::as_tibble_row(c(list(category = category), as.list(scores)))
  }

  category_rows <- lapply(score_group_vars, function(group_var) {
    category_values <- prep_factor(table_data[[group_var]])
    lapply(levels(category_values), function(category) {
      make_score_row(
        category,
        !is.na(category_values) & category_values == category
      )
    })
  })

  dplyr::bind_rows(
    make_score_row("Total", rep(TRUE, nrow(table_data))),
    unlist(category_rows, recursive = FALSE)
  )
})

df4 <- survey::svydesign(ids = ~1, weights = ~hhweight, data = df)

tb_n <- count_by_stub(df4, score_group_vars) |>
  dplyr::relocate(count_unw, .after = count_w)

tb_61a <- tb_score2 |> left_join(tb_n, by = c("category" = "group_cat")) 

assign("tb_61a", tb_61a, envir = .GlobalEnv)


# Table 4.1

# The code creates an object named hh that corresponds to out use, so the hh used in our data is saved in name hhx, and after table 4.1 it is recalled from here. Save hh in the project to hhx

hhx <- hh

group_vars    <- c("area",
                   "degubra", 
                  "headsex",
                   "headage_gr",
                  "headed",
                  "windex5"
                    
                 )

# path_fies and path_fies_output are supplied by micsPlusTableR from the
# selected preparation folder and configured main output folder.
stopifnot(dir.exists(path_fies), dir.exists(path_fies_output))

assign("hh_path", hh_path, envir = .GlobalEnv)
assign("hl_path", hl_path, envir = .GlobalEnv)

# Run the script for FIES computations
source(paste0(path_fies, "/JAM MICSPlus Wave2 - Table 4.1_Script.R"))

res.df[[1]] |> 
  dplyr::select(Disaggregation, P_mod, P_sev)

c1 <-
res.df$PrevalenceRates |> 
  dplyr::select(Disaggregation, P_sev, P_mod, N) |> 
  mutate(P_sev = as.numeric(P_sev) * 100,
         P_mod = as.numeric(P_mod) * 100,
         `Moderate food insecurity` = P_mod - P_sev) |> 
  relocate(`Moderate food insecurity`, .after = 1) |> 
  mutate(N = as.numeric(N)) |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |> 
    dplyr::rename(stub = Disaggregation) |> 
    filter(!stub %in% c("area", "degubra", "headsex", "headage", "headed", "windex5"))

# HL
hlx <- hl |> 
  dplyr::filter(HL7 %in% c(1,NA))

hlx_des <- hlx |> # Population level
  as_survey_design(
    weights = hhweight
  )

dfx <- hlx_des |> 
  dplyr::select(FI1:FI8, hhweight, dplyr::all_of(group_vars)) |> 
  mutate(across(FI1:FI8, ~ case_when(
    . %in% c("YES", 1) ~ 1,
    . %in% c("NO", 2) ~ 2,
    . %in% c("DK", 9) ~ NA_real_,
    TRUE ~ NA_real_
  ))) |> 
  # Drop rows where any column from FI1 to FI8 is NA
  drop_na(FI1:FI8)

c2 <- count_by_stub(dfx, group_vars) |>
  dplyr::rename(stub = group_cat)

tb_41 <-
  c1 |> 
  left_join(c2, by = c("stub")) |> 
  select(-c(N)) |> 
  relocate(count_unw, .after = count_w)

assign("tb_41", tb_41, envir = .GlobalEnv)

hh <- hhx

# 1. Define the Destination Path (Make sure this variable is defined)
# The destination directory must exist!

# 2. List the Files to be Moved
files_to_move <- c(
  "JAM MICSPlus Wave2 - Table 4.1_ CorrelationCommonItems.csv",
  "JAM MICSPlus Wave2 - Table 4.1_ Equated parameters.RData",
  "JAM MICSPlus Wave2 - Table 4.1_ Equating plot.pdf",
  "JAM MICSPlus Wave2 - Table 4.1_ FIES categories plot.pdf",
  "JAM MICSPlus Wave2 - Table 4.1_ Household Probabilities.csv",
  "JAM MICSPlus Wave2 - Table 4.1_ Prevalence rates.csv",
  "JAM MICSPlus Wave2 - Table 4.1_Boxplots.png",
  "JAM MICSPlus Wave2 - Table 4.1_DIF_by_wealth.pdf",
  "JAM MICSPlus Wave2 - Table 4.1_Shiny 4w.csv",
  "OutputJAM MICSPlus Wave2 - Table 4.1_.csv",
  "OutputJAM MICSPlus Wave2 - Table 4.1_unweighted.csv",
  "OutputJAM MICSPlus Wave2 - Table 4.1_weighted.csv"
)



# 3. Loop and Move the Files using file.rename()
for (file in files_to_move) {
  
  # Define the full source path (where the file currently is)
  source_path <- here(file) 
  
  # Define the full destination path (where the file should go)
  destination_path <- file.path(path_fies_output, file)
  
  # Check if the source file exists before trying to move it
  if (file.exists(source_path)) {
    file.rename(from = source_path, to = destination_path)
    # Optional: Print confirmation
    # cat("Moved:", file, "\n")
  } else {
    cat("Skipped: File not found at source path:", file, "\n")
  }
}

# All Rplot...*.png files to FIES-outputs

rplot_pngs <- list.files(
  path = here::here(),
  pattern = "^Rplot.*\\.png$",
  ignore.case = TRUE
)

files_to_move2 <- unique(c(files_to_move, rplot_pngs))

for (file in files_to_move2) {
  source_path <- here::here(file)
  destination_path <- file.path(path_fies_output, file)

  if (file.exists(source_path)) {
    file.rename(from = source_path, to = destination_path)
  } else {
    cat("Skipped: File not found at source path:", file, "\n")
  }
}

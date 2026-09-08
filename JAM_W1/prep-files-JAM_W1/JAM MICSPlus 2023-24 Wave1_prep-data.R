
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
  mutate(HHID = HH0*1000000+HH1*1000+HH2*10+HH0A)

# Add total variables
hh <- hh |> mutate(total = 1)
hl <- hl |> mutate(total = 1)

# from hh to hl -------------------------------------------------
hh_add <- hh |> select(HHID, HLnum, ch117num, headed,
                       HH17,
                       stratum, 
                       FI1, FI2, FI3, FI4, FI5, FI6, FI7, FI8,
                       headsex, headage,
                       CVI4, CVI5) 

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

tables_extra <- c("JW1.4.1")

# Dictionary for extra tables and their corresponding outputs

extra_tables_dict <- list(
  "JW1.4.1" = "tb_41"
)

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
source(paste0(path_fies, "/JAM MICSPlus Wave1 - Table 4.1_Script.R"))

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

c2 <- local({
  count_data <- dfx$variables
  count_data$.count_weight <- as.numeric(stats::weights(dfx))

  # Preserve survey value labels and their code order. For unlabelled character
  # variables, preserve the order in which categories first occur.
  as_stub_factor <- function(x) {
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

  grouped_counts <- lapply(group_vars, function(group_var) {
    stub_values <- as_stub_factor(count_data[[group_var]])

    tibble::tibble(
      stub = stub_values,
      count_unw = 1,
      count_w = count_data$.count_weight
    ) |>
      dplyr::filter(!is.na(stub)) |>
      dplyr::group_by(stub, .drop = FALSE) |>
      dplyr::summarise(
        count_unw = sum(count_unw),
        count_w = sum(count_w),
        .groups = "drop"
      ) |>
      dplyr::mutate(stub = as.character(stub))
  })

  dplyr::bind_rows(
    tibble::tibble(
      stub = "Total",
      count_unw = nrow(count_data),
      count_w = sum(count_data$.count_weight)
    ),
    grouped_counts
  )
})

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
  "JAM MICSPlus Wave1 - Table 4.1_ CorrelationCommonItems.csv",
  "JAM MICSPlus Wave1 - Table 4.1_ Equated parameters.RData",
  "JAM MICSPlus Wave1 - Table 4.1_ Equating plot.pdf",
  "JAM MICSPlus Wave1 - Table 4.1_ FIES categories plot.pdf",
  "JAM MICSPlus Wave1 - Table 4.1_ Household Probabilities.csv",
  "JAM MICSPlus Wave1 - Table 4.1_ Prevalence rates.csv",
  "JAM MICSPlus Wave1 - Table 4.1_Boxplots.png",
  "JAM MICSPlus Wave1 - Table 4.1_DIF_by_wealth.pdf",
  "JAM MICSPlus Wave1 - Table 4.1_Shiny 4w.csv",
  "OutputJAM MICSPlus Wave1 - Table 4.1_.csv",
  "OutputJAM MICSPlus Wave1 - Table 4.1_unweighted.csv",
  "OutputJAM MICSPlus Wave1 - Table 4.1_weighted.csv"
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

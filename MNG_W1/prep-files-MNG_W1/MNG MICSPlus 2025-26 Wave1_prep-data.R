
# Preparation dependencies are checked by micsPlusTableR before data are read.
# These dependencies install automatically with micsPlusTableR; no separate setup is needed.
# micsPlusTableR supplies paths, makes these packages available to this script,
# and collects hh, hl, and extra-table objects without a bootstrap script.
prep_dependencies <- c(
  "dplyr", "haven"
)

hh <- haven::read_sav(hh_path) |>
  arrange(HH1, HH2, HH0, HH0A) |>
  mutate(
    hhweightA = ifelse(is.na(hhweightA), 0, hhweightA),
    hhweightB = ifelse(is.na(hhweightB), 0, hhweightB)
  )

hl <- haven::read_sav(hl_path)

hl <- hl |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)
hh <- hh |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)

hh <- hh |> mutate(total = 1)
hl <- hl |> mutate(total = 1)

hh_add <- hh |> select(HHID, HLnum, wm1534num, wm1534age, wm1534ed, headed, headsex, headage, headethnic, CH2)

hl <- hl |> left_join(hh_add, by = "HHID")

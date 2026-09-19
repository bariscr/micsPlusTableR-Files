
# Preparation dependencies are checked by micsPlusTableR before data are read.
# These dependencies install automatically with micsPlusTableR; no separate setup is needed.
# micsPlusTableR supplies paths, makes these packages available to this script,
# and collects hh, hl, and extra-table objects without a bootstrap script.
prep_dependencies <- c(
  "dplyr", "haven"
)

hh <- haven::read_sav(hh_path) |>
  arrange(HH1, HH2, HH0, HH0A) 
  
hl <- haven::read_sav(hl_path)

hl <- hl |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)
hh <- hh |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)

hh <- hh |> mutate(total = 1)
hl <- hl |> mutate(total = 1)

hh_add <- hh |> select(HHID, 
                       HLnum, 
                       headed, 
                       headsex, 
                       headage, 
                       headethnic,
                       headlang)

hl <- hl |> left_join(hh_add, by = "HHID")

# Exclusions to be added to footnote
xxx <- hh |> 
filter(HH17 %in% c(0,1),
       intduration == 97) |> 
nrow()

assign("xxx", xxx, envir = .GlobalEnv)

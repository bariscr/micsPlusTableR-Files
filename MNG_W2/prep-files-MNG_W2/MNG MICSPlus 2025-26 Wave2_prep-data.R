
# Preparation contract: micsPlusTableR supplies hh_path and hl_path, makes its
# declared imports available, and collects hh, hl, and extra-table objects.
# Do not source external package or function bootstrap scripts from this file.

hh <- haven::read_sav(hh_path) |>
  arrange(HH1, HH2, HH0, HH0A) 
  
  
 # |>
 # mutate(
 #   hhweightA = ifelse(is.na(hhweightA), 0, hhweightA),
 #   hhweightB = ifelse(is.na(hhweightB), 0, hhweightB)
 # )

hl <- haven::read_sav(hl_path)

hl <- hl |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)
hh <- hh |> mutate(HHID = HH0*1000000 + HH1*1000 + HH2*10 + HH0A)

hh <- hh |> mutate(total = 1)
hl <- hl |> mutate(total = 1)

hh_add <- hh |> select(HHID, HLnum, headed, headsex, headage, headethnic,
                       CH2)
hl <- hl |> left_join(hh_add, by = "HHID")

# Exclusions to be added to footnote
xxx <- hh |> 
filter(HH17 %in% c(0,1),
       intduration == 97) |> 
nrow()

assign("xxx", xxx, envir = .GlobalEnv)

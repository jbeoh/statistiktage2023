library(magrittr)
query.parameter <- list(
  "Kanton" = c(as.character(seq(1, 26))), "Jahr" = as.character(seq(2000, 2021)), "Staatsangehörigkeit (Kategorie)" = c("0"), "Geschlecht" = c("0"),
  "Demografische Komponente" = c("12")
)
# get data
daten <- BFS::bfs_get_data(number_bfs = "px-x-0102020000_101", query = query.parameter) %>%
  dplyr::select(Kanton, Jahr, `Demografische Bilanz nach Kanton`) %>%
  dplyr::rename(Population = `Demografische Bilanz nach Kanton`) %>%
  dplyr::mutate(
    Kanton = sub("\\/.*", "", Kanton),
    Canton = dplyr::case_when(
      Kanton == "Zürich" ~ "ZH",
      Kanton == "Bern " ~ "BE",
      Kanton == "Luzern" ~ "LU",
      Kanton == "Uri" ~ "UR",
      Kanton == "Schwyz" ~ "SZ",
      Kanton == "Obwalden" ~ "OW",
      Kanton == "Nidwalden" ~ "NW",
      Kanton == "Glarus" ~ "GL",
      Kanton == "Zug" ~ "ZG",
      Kanton == "Aargau" ~ "AG",
      Kanton == "Appenzell Ausserrhoden" ~ "AR",
      Kanton == "Appenzell Innerrhoden" ~ "AI",
      Kanton == "Basel-Landschaft" ~ "BL",
      Kanton == "Basel-Stadt" ~ "BS",
      Kanton == "Fribourg " ~ "FR",
      Kanton == "Genève" ~ "GE",
      Kanton == "Graubünden " ~ "GR",
      Kanton == "Jura" ~ "JU",
      Kanton == "Neuchâtel" ~ "NE",
      Kanton == "Schaffhausen" ~ "SH",
      Kanton == "Solothurn" ~ "SO",
      Kanton == "St. Gallen" ~ "SG",
      Kanton == "Thurgau" ~ "TG",
      Kanton == "Ticino" ~ "TI",
      Kanton == "Valais " ~ "VS",
      Kanton == "Vaud" ~ "VD"
    )
  )

daten1 <- daten %>%
  dplyr::filter(Jahr == "2021") %>%
  dplyr::mutate(hover = paste0("Canton: ",Kanton,"<br>Population: ",format(Population,big.mar=",",small.mark=".")))
save(daten1,file="./data/daten1.rda")
daten4 <- daten %>%
  dplyr::filter(Canton == "ZG") %>%
  dplyr::rename(Year = Jahr)
save(daten4,file="./data/daten4.rda")
query.parameter2 <- list(
  "Kanton" = "9", "Jahr" = c(as.character(seq(2000, 2021))), "Staatsangehörigkeit (Kategorie)" = c("0"), "Geschlecht" = c("0"),
  "Demografische Komponente" = c("1", "2", "4", "5", "6", "7")
)
# filter for negative numbers
decline <- c("Todesfall", "Auswanderung", "Interkantonaler Wegzug")
# recode lookup table
lookup <- data.frame(g = c("Lebendgeburt", "Todesfall", "Einwanderung inkl. Änderung des Bevölkerungstyps", "Auswanderung", "Interkantonaler Zuzug", "Interkantonaler Wegzug"), e = c("Birth", "Death", "Immigration", "Emigration", "Influx other cantons", "Outflux other cantons"))
# get data
daten2 <- BFS::bfs_get_data(number_bfs = "px-x-0102020000_101", query = query.parameter2) %>%
  dplyr::rename(Year = Jahr, Type = `Demografische Komponente`, Value = `Demografische Bilanz nach Kanton`) %>%
  dplyr::mutate(
    Value = dplyr::case_when(Type %in% decline ~ Value * -1, .default = Value),
    Type = plyr::mapvalues(Type, from = lookup$g, to = lookup$e),
    Year = as.numeric(Year)
  )
save(daten2,file="./data/daten2.rda")
query.parameter3 <- list(
  "Jahr" = "2021", "Kanton (-) / Bezirk (>>) / Gemeinde (......)" = c(as.character(seq(699, 710))), "Staatsangehörigkeit (Kategorie)" = c("0"), "Geschlecht" = c("0"),
  "Demografische Komponente" = c("14")
)

# get data
daten3 <- BFS::bfs_get_data(number_bfs = "px-x-0102020000_201", query = query.parameter3) %>%
  dplyr::rename(
    Year = Jahr,
    Inhabitants = `Demografische Bilanz nach institutionellen Gliederungen`,
    Gemeinde = "Kanton (-) / Bezirk (>>) / Gemeinde (......)"
  ) %>%
  dplyr::mutate(
    Year = as.numeric(Year),
    Gemeinde = forcats::fct_relevel(forcats::as_factor(sub("[^[:alpha:]]+", "", Gemeinde)), "Kanton Zug"),
    hover = paste0("Municipality: ", Gemeinde, "<br>Inhabitants: ", format(Inhabitants, small.mark = ",", big.mark = "'"))
  )
save(daten3,file="./data/daten3.rda")
Sys.sleep(10)
kanton <- daten3 %>% dplyr::filter(Gemeinde == "Kanton Zug")
save(kanton,file="./data/kanton.rda")
gemeinden <- daten3 %>% dplyr::filter(Gemeinde != "Kanton Zug")
save(gemeinden,file="./data/gemeinden.rda")
# get data
daten4 <- daten4 %>%
  dplyr::mutate(
    Year = as.Date(paste0(Year, "-31-12"), format = "%Y-%d-%m"),
    hover = paste0("Year: ", lubridate::year(Year), "<br>Inhabitants: ", format(Population, small.mark = ",", big.mark = "'"))
  )
save(daten4,file="./data/daten4.rda")

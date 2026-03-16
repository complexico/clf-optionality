library(tidyverse)
library(readxl)
# read frequency breakdown for CD + NN
cd_nn_freq <- read_xlsx("data/cd_nn_freq.xlsx") |> 
  separate_wider_delim(cols = Search_result,
                       delim = " ",
                       names = c("cd", "nn"),
                       too_few = "debug") |> 
  filter(Search_result_ok) |> 
  ## harmonise the words to be lowercase for counting purpose
  mutate(cd = tolower(cd),
         nn = tolower(nn)) |> 
  group_by(cd, nn) |> 
  summarise(Token = sum(Token)) |> 
  arrange(desc(Token)) |> 
  ungroup()
# read frequency breakdown for CD + buah + NN
cd_buah_nn_freq <- read_xlsx("data/cd_buah_nn_freq.xlsx") |> 
  separate_wider_delim(cols = Search_result,
                       delim = " ",
                       names = c("cd", "buah", "nn"),
                       too_few = "debug") |> 
  filter(Search_result_ok) |> 
  ## harmonise the words to be lowercase for counting purpose
  mutate(cd = tolower(cd),
         buah = tolower(buah),
         nn = tolower(nn)) |> 
  group_by(cd, buah, nn) |> 
  summarise(Token = sum(Token)) |> 
  arrange(desc(Token)) |> 
  ungroup()

included_cd <- c("satu",
                 "dua",
                 "tiga",
                 "lima",
                 "empat",
                 "enam",
                 "delapan",
                 "tujuh",
                 "sepuluh",
                 "belas",
                 "sembilan",
                 "puluh",
                 "seribu",
                 "sejuta",
                 "sebelas",
                 "seratus",
                 "ratus",
                 "triliun",
                 "ribu",
                 "juta",
                 "miliar",
                 "milliar",
                 "semiliar",
                 "semilliar",
                 "milyar",
                 "semilyar")

excluded_nn <- c("orang", "ekor",
                 "tahun", "bulan",
                 "hari", "detik", "menit",
                 "dekade", "jam", "minggu",
                 "pekan",
                 "rupiah")

# load annotated Excel data
# coll_df <- read_xlsx("data/Coll1R1R_WithSemanticsForPaperYear2.xlsx",
#                      sheet = "all buah") |> 
#   # filter out observations that are NOT "se-"
#   filter(SearchKeyword != "se")

cd_nn_freq |> 
  filter(!nn %in% excluded_nn) |> 
  filter(cd %in% included_cd|str_detect(cd, "[0-9]+")) |> 
  filter(str_detect(nn, "^(wi(b|ta?)$|\\-)", negate = TRUE)) |> 
  filter(str_detect(cd, "^\\d\\.\\d$", negate = TRUE)) |> 
  mutate(nn_corrected = "") |> 
  relocate(nn_corrected, .before = Token) |> 
  writexl::write_xlsx(path = "data/cd_nn_freq_filtered.xlsx")

cd_buah_nn_freq |> 
  filter(!nn %in% excluded_nn) |> 
  filter(cd %in% included_cd|str_detect(cd, "[0-9]+")) |> 
  filter(str_detect(nn, "^(wi(b|ta?)$|\\-)", negate = TRUE)) |> 
  filter(str_detect(cd, "^\\d\\.\\d$", negate = TRUE)) |> 
  mutate(nn_corrected = "") |> 
  relocate(nn_corrected, .before = Token) |> 
  writexl::write_xlsx(path = "data/cd_buah_nn_freq_filtered.xlsx")

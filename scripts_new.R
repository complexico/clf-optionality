library(tidyverse)
library(readxl)

# load annotated Excel data
coll_df <- read_xlsx("data/Coll1R1R_WithSemanticsForPaperYear2.xlsx",
                     sheet = "all buah") |>
  # filter out observations that are NOT "se-"
  filter(SearchKeyword != "se")

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
                 "rupiah",
                 "ayat", "huruf",
                 "hari", "hr",
                 "thn", "persen",
                 "dollar", "dolar", "euro",
                 "m", "ml", "g", "mg", "gram")

# subsetting the CD+NN freq data
cd_nn_freq |> 
  filter(!nn %in% excluded_nn) |> 
  filter(cd %in% included_cd|str_detect(cd, "[0-9]+")) |> 
  filter(str_detect(nn, "^(wi(b|ta?)$|\\-)", negate = TRUE)) |> 
  filter(str_detect(cd, "^\\d\\.\\d$", negate = TRUE)) |> 
  filter(str_detect(nn, "^.{1,2}$", negate = TRUE)) |> 
  filter(str_detect(nn, "^rp[0-9]", negate = TRUE)) |> 
  filter(str_detect(nn, "^[[:punct:]]$", negate = TRUE)) |> 
  filter(str_detect(nn, "^(thn|bln|hr|ml)\\b", negate = TRUE)) |> 
  mutate(nn_corrected = "") |> 
  relocate(nn_corrected, .before = Token) # |> 
  # writexl::write_xlsx(path = "data/cd_nn_freq_filtered.xlsx")

# cd_nn_freq data whose nn is available from Karlina's Excel, manually-checked data
cd_nn_freq |> 
  filter(!nn %in% excluded_nn) |> 
  filter(cd %in% included_cd|str_detect(cd, "[0-9]+")) |> 
  filter(str_detect(nn, "^(wi(b|ta?)$|\\-)", negate = TRUE)) |> 
  filter(str_detect(cd, "^\\d\\.\\d$", negate = TRUE)) |> 
  filter(str_detect(nn, "^.{1,2}$", negate = TRUE)) |> 
  filter(str_detect(nn, "^rp[0-9]", negate = TRUE)) |> 
  filter(str_detect(nn, "^[[:punct:]]$", negate = TRUE)) |> 
  filter(str_detect(nn, "^(thn|bln|hr|ml)\\b", negate = TRUE)) |> 
  mutate(nn_corrected = "") |> 
  relocate(nn_corrected, .before = Token) |> 
  # the code below is the filtering using Karlina's checked noun data
  filter(nn %in% str_to_lower(coll_df$Word)) # |> 
  # writexl::write_xlsx(path = "data/cd_nn_freq_filtered_from_Karlina_Excel.xlsx")

# subsetting the CD+buah+NN data
cd_buah_nn_freq |> 
  filter(!nn %in% excluded_nn) |> 
  filter(cd %in% included_cd|str_detect(cd, "[0-9]+")) |> 
  filter(str_detect(nn, "^(wi(b|ta?)$|\\-)", negate = TRUE)) |> 
  filter(str_detect(cd, "^\\d\\.\\d$", negate = TRUE)) |> 
  filter(str_detect(nn, "^.{1,2}$", negate = TRUE)) |> 
  filter(str_detect(nn, "^rp[0-9]", negate = TRUE)) |> 
  filter(str_detect(nn, "^[[:punct:]]$", negate = TRUE)) |> 
  filter(str_detect(nn, "^(thn|bln|hr|ml)\\b", negate = TRUE)) |> 
  mutate(nn_corrected = "") |> 
  relocate(nn_corrected, .before = Token) # |> 
  # writexl::write_xlsx(path = "data/cd_buah_nn_freq_filtered.xlsx")

# read frequency breakdown for CD + buah + NN from Karlina's assistant
cd_buah_nn_freq_checked <- read_xlsx("data/ForAssistants.xlsx",
                                     sheet = 2)

cd_buah_nn_freq_checked_01 <- cd_buah_nn_freq_checked |> 
  filter(is.na(Confidence))

# read frequency breakdown for CD + NN
## cd_nn_freq data whose nn is available from Karlina's Excel, manually-checked data
cd_nn_freq <- read_xlsx("data/cd_nn_freq_filtered.xlsx")
cd_nn_freq_00 <- read_xlsx("data/cd_nn_freq_filtered_from_Karlina_Excel.xlsx")
cd_nn_freq_01 <- cd_nn_freq |> 
  filter(nn %in% cd_buah_nn_freq_checked_01$nn)
nn_combined <- unique(c(cd_nn_freq_00$nn, # from Karlina's data
                        cd_nn_freq_01$nn,
                        cd_buah_nn_freq_checked_01$nn) # from Assistant's data
                      )
cd_nn_freq_02 <- cd_nn_freq |> 
  filter(nn %in% nn_combined)


# read frequency breakdown for CD + NN from Karlina's assistant
cd_nn_freq_uncheck <- read_xlsx("data/ForAssistants.xlsx",
                                sheet = 4)

n_from_Karlina <- tibble(nn = str_to_lower(coll_df$Word), is_checked = "y", nn_correction = "")
n_from_cd_buah_checked <- cd_buah_nn_freq_checked_01 |> 
  select(nn, nn_correction) |> 
  mutate(is_checked = "y",
         nn_correction = replace_na(nn_correction, ""))
n_checked <- bind_rows(n_from_Karlina, n_from_cd_buah_checked) |> 
  distinct()

# get combined NNs from cxn with BUAH and joined them to indicate
## which N in CD+NN needs to be checked.

cd_nn_freq_uncheck_01 <- cd_nn_freq_uncheck |> 
  select(-nn_correction, -Confidence) |> 
  # code below to join table with nouns that have been checked manually and that have a column indicating it has been checked
  left_join(n_checked |> select(-nn_correction) |> distinct()) |> 
  mutate(nn_correction = "", # placeholder for assistant
         Confidence = "") |> # placeholder
  relocate(nn_correction, .after = "nn") |> 
  relocate(Confidence, .after = nn_correction) |> 
  arrange(desc(is_checked), nn) |> # sort the data such that the NNs that have been checked from cxn with BUAH appears on top.
  
  # filtering out criteria
  filter(str_detect(nn, "(^[[:punct:]]$|“|”|\\b[[:punct:]]\\b)", negate = TRUE),
         str_detect(nn, str_c("\\b(", str_replace_all("ayat, huruf, hari, hr, bulan, bln, tahun, thn, persen", ", ", "|"), ")\\b", sep = ""),
                    negate = TRUE),
         str_detect(nn, "^([a-z]|[a-z]{2}|dollar|dolar|rupiah|euro)$", negate = TRUE),
         str_detect(nn, "\\b(m|ml|g|mg|hz|kg|cm|jam|ton|ppm|kgbb|gram|ci|ppm|m[0-9])\\b", negate = TRUE),
         str_detect(nn, "(\\b[a-z]([[:punct:]&&[^-]])\\b|\\b[[:punct:]&&[^-]][a-z]\\b)",
                    negate = TRUE),
         str_detect(nn, "\\brp[0-7]+", negate = TRUE),
         str_detect(cd, "(\\:|\\/|\\\\|\\.|\\,)", negate = TRUE))

# googledrive::drive_create(name = "cd_nn_to_check", path = "https://drive.google.com/drive/folders/1cirKKApXTUu-h8lnaQdYJGsnORvTxzlW",
#                           type = "spreadsheet")
# Created Drive file:
#   • cd_nn_to_check <id: 138ices4Ms1aM7FFJmF74RePuWpBlMKsYRtL58WUeJl8>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet

# googlesheets4::write_sheet(cd_nn_freq_uncheck_01,
#                            ss = "138ices4Ms1aM7FFJmF74RePuWpBlMKsYRtL58WUeJl8",
#                            sheet = "Sheet1")

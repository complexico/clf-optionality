library(tidyverse)
library(readxl)

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

# load annotated Excel data
coll_df <- read_xlsx("data/Coll1R1R_WithSemanticsForPaperYear2.xlsx",
                     sheet = "all buah") |> 
  # filter out observations that are NOT "se-"
  filter(SearchKeyword != "se")

# collocation data for the search term CD + buah
## the filtered collocates are those tagged as NN (1R&1L)
cd_buah_df <- read.table("data/collocations-2.txt",
                         header = TRUE,
                         skip = 3,
                         sep = "\t") |> 
  as_tibble()

# data of freqlist with the pattern CD + NN
## goal: to get the NN that can occur with numeral (CD) without "buah"
wfreq <- read_xlsx("data/query-freq-breakdown.xlsx")
wfreq1 <- wfreq |> 
  separate_wider_delim(cols = Search_result,
                       delim = " ",
                       names = c("cd", "nn"),
                       too_few = "debug") |> 
  filter(Search_result_ok) |> 
  ## harmonise the words to be lowercase for counting purpose
  mutate(cd = tolower(cd),
         nn = tolower(nn)) |> 
  group_by(cd, nn) |> 
  summarise(No_of_occurrences = sum(No_of_occurrences)) |> 
  arrange(desc(No_of_occurrences)) |> 
  ungroup()

# wfreq 2 contains nouns in the search pattern [CD + NN]
wfreq2 <- wfreq1 |> 
  ## filter out CD-tagged item that are not word-character
  filter(str_detect(cd, "\\W", negate = TRUE))

# this wfreq3 contains NN words  (from CD + NN search pattern in wfreq2)
# that are available also in Karlina's Excel sheet file Coll1R1R_With...
# In other words, wfreq3 contains nouns that can AND cannot appear with clf.
wfreq3 <- wfreq2 |> 
  filter(nn %in% tolower(coll_df$Word))
wfreq3

wfreq3 |> 
  filter(cd %in% included_cd | str_detect(cd, "[0-9]+")) |> 
  pull(nn) |> 
  unique()











# the unique noun-type that can and cannot appear with clf. in CD + (clf.) + N
wfreq3 |> 
  select(nn) |> 
  mutate(nn = tolower(nn)) |> 
  distinct() |> 
  nrow()

cd_buah_df |> 
  select(Word) |> 
  mutate(Word = tolower(Word)) |> 
  distinct() |> 
  nrow()

# 1. noun in [Num/CD + nn] BUT NOT in [Num/CD + class + nn]
wfreq3 |> 
  filter(!tolower(nn) %in% tolower(cd_buah_df$Word)) |> 
  select(nn) |> 
  mutate(nn = tolower(nn)) |> 
  distinct() |> 
  nrow()
# 167

# 2. noun in [Num + class + nn] BUT NOT in [Num + nn]
cd_buah_df |> 
  filter(!tolower(Word) %in% tolower(wfreq3$nn)) |> 
  select(Word) |> 
  mutate(Word = tolower(Word)) |> 
  distinct() |> 
  nrow()
# 44

# 3 noun in [Num + class + nn] AND in [Num + nn]
cd_buah_df |> 
  filter(tolower(Word) %in% tolower(wfreq3$nn)) |> 
  select(Word) |> 
  mutate(Word = tolower(Word)) |> 
  distinct() |> 
  nrow()
# 63

library(tidyverse)

pp_lines <- readLines("data/day_04.txt")

i_blank_lines <- which(pp_lines == "")

idx <- 1
pp_vec <- vector(mode="character")
for (i in pp_lines){
  if(i == ""){
    idx <- idx + 1
    next
  } else {
    if(is.na(pp_vec[idx]))
      pp_vec[idx] <- i
    else 
      pp_vec[idx] <- paste(pp_vec[idx],i)
  }
}
pp_df <- tibble(raw_list = str_split(pp_vec," ")) %>%
  mutate(id = 1:n()) %>%
  unnest(raw_list) %>%
  mutate(key_val_split = map(raw_list,~str_split(.,":"))) %>%
  mutate(key = map_chr(key_val_split,~.[[1]][1])) %>%
  mutate(val = map_chr(key_val_split,~.[[1]][2])) %>%
  pivot_wider(names_from = key,values_from=val,id_cols=id)

pp_df %>% filter(across(!(starts_with("cid") | starts_with("id")),Negate(is.na))) %>%
  nrow
pp_df %>% select(!(starts_with("cid") | starts_with("id")))


# part 2 ------------------------------------------------------------------
# 
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
# Your job is to count the passports where all required fields are both present and valid according to the above rules. Here are some example values:
#   

aa=pp_df %>% 
  mutate(hgt_unit = str_extract(hgt,"cm|in")) %>%
  mutate(hgt_num = as.integer(str_extract(hgt,"[[:digit:]]+"))) %>%
  filter(nchar(byr) == 4 & as.integer(byr) >= 1920 & as.integer(byr) <=2002) %>%
  filter(nchar(iyr) == 4 & as.integer(iyr) >= 2010 & as.integer(iyr) <=2020) %>%
  filter(nchar(eyr) == 4 & as.integer(eyr) >= 2020 & as.integer(eyr) <=2030) %>%
  filter(!is.na(hgt_unit) & !is.na(hgt_num)) %>%
  filter(if_else(hgt_unit=="cm",
                 ifelse(hgt_num >= 150 & hgt_num <= 193,T,F),
                 ifelse(hgt_num >=59 & hgt_num <=76,T,F))) %>%
  filter(grepl("^#[0-9a-f]{6}$",hcl)) %>%
  filter(ecl %in% c("amb","blu","brn","gry","grn","hzl","oth")) %>%
  filter(grepl("^[0-9]{9}$",pid)) 

aa %>% anti_join(pp_df %>% 
                   mutate(hgt_unit = str_extract(hgt,"cm|in")) %>%
                   mutate(hgt_num = as.integer(str_extract(hgt,"[[:digit:]]+"))) %>%
                   filter(nchar(byr) == 4 & as.integer(byr) >= 1920 & as.integer(byr) <=2002) %>%
                   filter(nchar(iyr) == 4 & as.integer(iyr) >= 2010 & as.integer(iyr) <=2020) %>%
                   filter(nchar(eyr) == 4 & as.integer(eyr) >= 2020 & as.integer(eyr) <=2030) %>%
                   filter(!is.na(hgt_unit) & !is.na(hgt_num)) %>%
                   filter(if_else(hgt_unit=="cm",
                                  ifelse(hgt_num >= 150 & hgt_num <= 193,T,F),
                                  ifelse(hgt_num >=59 & hgt_num <=76,T,F))) %>%
                   filter(grepl("^#[0-9a-f]{6}$",hcl)) %>%
                   filter(ecl %in% c("amb","blu","brn","gry","grn","hzl","oth")) %>%
                   filter(grepl("^[0-9]{9}$",pid)) %>% select(id))  

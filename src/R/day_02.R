library(tidyverse)

raw_pw <- readLines("data/day_02.txt")


pw_pattern <- "(?<minl>(?:[0-9]+))-(?<maxl>(?:[0-9]+)) (?<letter>(?:[a-z]{1}))\\: (?<pw>(?:[a-z]+))"
str_match_all(raw_pw[1:2],pw_pattern)
pw_df <- tibble(str_list = str_match_all(raw_pw,pw_pattern)) %>%
  mutate(raw_pw = raw_pw) %>% 
  mutate(min_letter = map_int(str_list,~as.integer(.[2]))) %>%
  mutate(max_letter = map_int(str_list,~as.integer(.[3]))) %>%
  mutate(letter_minmax = map_chr(str_list,~.[4])) %>% 
  mutate(pw_in = map_chr(str_list,~.[5])) %>%
  mutate(pw_length = map_int(pw_in,nchar)) %>%
  mutate(letter_count = map2_int(pw_in,letter_minmax,function(x,y)str_count(x,y)))

is_valid_pw <- function(minl,maxl,l,p){
  # browser()
  l_count <- str_count(p,l)
  if(l_count >= minl && l_count <= maxl)
    return(T)
  else
    return(F)
}
pw_df_out <- pw_df %>% mutate(is_valid = pmap_lgl(list(min_letter,max_letter,
                                          letter_minmax,pw_in),is_valid_pw))
pw_df_out %>% count(is_valid)


# part 2 ------------------------------------------------------------------

pw_df <- tibble(str_list = str_match_all(raw_pw,pw_pattern)) %>%
  mutate(raw_pw = raw_pw) %>% 
  mutate(min_letter = map_int(str_list,~as.integer(.[2]))) %>%
  mutate(max_letter = map_int(str_list,~as.integer(.[3]))) %>%
  mutate(letter_minmax = map_chr(str_list,~.[4])) %>% 
  mutate(pw_in = map_chr(str_list,~.[5])) %>%
  mutate(pw_length = map_int(pw_in,nchar)) %>%
  mutate(letter_count = map2_int(pw_in,letter_minmax,function(x,y)str_count(x,y)))
is_valid_pw_2 <- function(pos1,pos2,l,p){
  p_split <- str_split(p,"")[[1]]
  pos1_is_valid <- l == p_split[pos1]
  pos2_is_valid <- l == p_split[pos2]
  
  if((pos1_is_valid  + pos2_is_valid)==1)
    return(T)
  else
    return(F)
}

pw_df_out_2 <- pw_df %>% mutate(is_valid = pmap_lgl(list(min_letter,max_letter,
                                                       letter_minmax,pw_in),is_valid_pw_2))
pw_df_out_2 %>% count(is_valid)

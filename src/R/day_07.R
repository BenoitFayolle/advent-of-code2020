library(tidyverse)
raw_rules <- readLines("data/day_07.txt")


rule_pattern <- "(?<bag_parent>(?:[a-z]+ [a-z]+)) bags contain (?<bag_>(?:[0-9]+)) (?<letter>(?:[a-z]{1}))\\: (?<pw>(?:[a-z]+))"
rules_df <- tibble(str_list = strsplit(raw_rules,'bags contain|,')) %>%
  mutate(parent_bag = map_chr(str_list,~trimws(.[1]))) %>%
  mutate(child_bag_raw = map(str_list,~trimws(.[-1]))) %>%
  select(-1) %>% unnest(child_bag_raw) %>% 
  mutate(child_bag_count = as.integer(str_extract(child_bag_raw,"^[0-9]+"))) %>%
  mutate(child_bag = map_chr(child_bag_raw,~paste(strsplit(.," ")[[1]][2:3],collapse=" "))) %>%
  mutate(child_bag = ifelse(child_bag == "other bags.",NA_character_,child_bag))


rules_df %>% filter(child_bag=="shiny gold")  
  
get_parents <- function(bag_out){
  rules_df %>% filter(child_bag == bag_out) %>% {.$parent_bag}
}

ok_bag <- character(0)
input_bag <- "shiny gold"
while(length(input_bag)>0){
  print("loop")
  output_bags <- map(input_bag,get_parents)
  ok_bag <- unique(c(ok_bag,unlist(output_bags)))
  input_bag = unlist(output_bags)
}


# part 2 ------------------------------------------------------------------

get_childs_bag <- function(bag_in,bag_count_in){
  child_bag = rules_df %>% filter(parent_bag %in% bag_in) %>% {.$child_bag}
  child_bag_count = rules_df %>% filter(parent_bag %in% bag_in) %>% {.$child_bag_count}
  child_bag <- child_bag[which(!is.na(child_bag))]
  child_bag_count = child_bag_count[which(!is.na(child_bag))]
  if(length(child_bag)==0)
    return(0)
  list(map2(child_bag,bag_count_in * child_bag_count,get_childs_bag),
       sum(bag_count_in * child_bag_count,na.rm=T))
}
res <- get_childs_bag("shiny gold",1)
sum(unlist(res))

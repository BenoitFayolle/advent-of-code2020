library(tidyverse)

ans_lines <- readLines("data/day_06.txt")

idx <- 1
ans_vec <- vector(mode="character")
for (i in ans_lines){
  if(i == ""){
    idx <- idx + 1
    next
  } else {
    if(is.na(ans_vec[idx]))
      ans_vec[idx] <- i
    else 
      ans_vec[idx] <- paste(ans_vec[idx],i)
  }
}
ans_df <- tibble(raw_list = str_split(ans_vec," ")) %>%
  mutate(group_id = 1:n()) %>%
  unnest(raw_list) %>%
  group_by(group_id) %>% mutate(member_id = 1:n()) %>%
  mutate(ans_split = map(raw_list,~str_split(.,"")[[1]])) %>%
  unnest(ans_split) %>%
  # group_by(id) %>% add_count(ans_split) %>% filter(n>1)
  mutate(val=T) %>%
  pivot_wider(names_from = ans_split,values_fill=F,
              values_from=val,id_cols=c(group_id,member_id),
              names_prefix="q_") %>%
  `colnames<-`(c(names(.)[c(1,2)],sort(names(.)[-c(1:2)]))) %>% ungroup
  
ans_df %>% summarise_all(mean)

ans_group <- ans_df %>% group_by(group_id) %>%
  summarise(across(starts_with("q_"),~sum(.)>0))

ans_group <- ans_group %>% mutate(total_answered_q = rowSums(ans_group %>% select(-group_id))) %>%
  select(group_id,total_answered_q,everything())

sum(ans_group$total_answered_q)


# part 2 ------------------------------------------------------------------


ans_group_2 <- ans_df %>% group_by(group_id) %>%
  summarise(across(starts_with("q_"),~sum(.)==n()))

ans_group_2 <- ans_group_2 %>% mutate(total_answered_q = rowSums(ans_group_2 %>% select(-group_id))) %>%
  select(group_id,total_answered_q,everything())

sum(ans_group_2$total_answered_q)

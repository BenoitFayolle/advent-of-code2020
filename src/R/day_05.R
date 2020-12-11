library(tidyverse)

seat_vec <- readLines("data/day_05.txt")

row_vec <- seq(0,127)
col_vec <- seq(0,7)

select_part <- function(seq_vec,command){
  L <- length(seq_vec)
  if (command == "L" || command == "F"){
    if (L == 2)
      seq_vec[1]
    else
      seq_vec[1:(L/2)]
  } else if (command == "B" || command == "R") {
    if (L == 2)
      seq_vec[2]
    else
      seq_vec[(L/2+1):L]
  }
}

seat_df <- tibble(command = strsplit(seat_vec,"")) %>% 
  mutate(id = 1:n()) %>%
  unnest(command) %>%
  group_by(id) %>%
  mutate(row_col = c(rep("row",7),rep("col",3))) %>% 
  mutate(inner_id = 1:n()) %>%
  ungroup %>%
  mutate(final_pos=NA)
  

for (i in 1:nrow(seat_df)){
  if (seat_df$inner_id[i] == 1)
    vec_temp <- row_vec
  else if (seat_df$inner_id[i] == 8)
    vec_temp <- col_vec
  vec_temp <- select_part(vec_temp,seat_df$command[i])
  print(vec_temp)
  if(length(vec_temp)==1)
    seat_df$final_pos[i] <- vec_temp
}
seat_part1 <- seat_df %>% filter(!is.na(final_pos)) %>%
  pivot_wider(names_from = row_col,values_from=final_pos,id_cols=id) %>%
  mutate(seat_id = row * 8 + col) %>%
  arrange(desc(seat_id))

head(seat_part1,1)
# part 2 ------------------------------------------------------------------

seat_part1 %>%
  pivot_wider(names_from = col,values_from=seat_id,id_cols=row) %>%
  arrange(row) %>% View

library(tidyverse);library(bit64)

raw_xmas <- as.integer64(readLines("data/day_09.txt"))

find_terms <- function(n,vec){
  df_out <- expand_grid(term1=vec,term2=vec) %>%
    mutate(id=1:n()) %>%
    mutate(sum_var = term1+term2) %>%
    filter(sum_var == n)
  if(nrow(df_out) == 0)
    return(NA)
  df_out %>% slice(1) %>% {c(.$term1[1],.$term2[1])}
  
}

find_terms(2,1:3)

i= 26
is_valid = T
while(is_valid & i <= length(raw_xmas)){
  previous_25 <- raw_xmas[(i-25):(i-1)]
  new_number <- raw_xmas[i]
  previous_25_ids <- find_terms(new_number,previous_25)
  if (all(is.na(previous_25_ids))){
    is_valid=F
  } else {
    print(paste(paste(previous_25_ids,collapse=" + "),"=",new_number))
    i = i + 1
  }
}

invalid_number = new_number

# part 2 ------------------------------------------------------------------

invalid_number

contiguous_numbers_count = 2
is_summed = F
while(!is_summed){
  for (i in 1:(length(raw_xmas)-(contiguous_numbers_count-1))){
    # browser()
    contiguous_numbers <- raw_xmas[i:(i+contiguous_numbers_count-1)]
    print(paste(paste(contiguous_numbers,collapse="+"),"=",invalid_number,"?"))
    if(sum(contiguous_numbers) == invalid_number){
      print("yes")
      is_summed = T
      break
    } else print("no")
  }
  contiguous_numbers_count <- contiguous_numbers_count + 1
}

min(contiguous_numbers) + max(contiguous_numbers)

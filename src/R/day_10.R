adapt <- sort(as.integer(readLines("data/day_10.txt")))


# part 1 ------------------------------------------------------------------

max(cumprod(table(diff(c(0,adapt,max(adapt)+3)))))

# part 2 ------------------------------------------------------------------
library(tidyverse)

# BRUTE FORCE impossible
# n_remove = 1
# new_sol_count = 1
# total_sol_count <- 1
# candidates <- list(adapt)
# while (new_sol_count>0){
#   sol_list <- list()
#   for (candidate in candidates){
#     for (i in 1:length(candidate)){
#       candidate_new <- candidate[-i]
#       if (all(unique(diff(c(0,candidate_new,max(candidate_new)+3))) %in% c(1,2,3)))
#         sol_list[[length(sol_list)+1]] <- candidate_new
#       total_sol_count = total_sol_count + 1
#     }
#     print(total_sol_count)
#   }
#   candidates <- sol_list
# }


diffs <- diff(c(0,adapt,max(adapt)+3))
lengths <- integer(0)
serie <- 0
for(i in 1:length(diffs)){
  if(diffs[i] == 1){
    serie <- serie + 1
  } else {
    if(serie>0)
      lengths <- c(lengths,serie)
    serie <- 0
  }
}

part2_df <- tibble(series_length= lengths) %>%
  filter(series_length>1) %>%
  left_join(tibble(series_length = c(2,3,4),
                   permutations=c(2,4,7))) # <- pen and paper that!

cumprod(bit64::as.integer64(part2_df$permutations))

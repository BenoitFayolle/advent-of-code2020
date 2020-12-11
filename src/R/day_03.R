library(tidyverse)

lines <- readLines("data/day_03.txt")
n_char <- nchar(lines[1])
x_pos <- 1
x_pos_rel <- x_pos
tree_bump_count <- 0
for (i in 1:length(lines)){
  splitted_line <- str_split(lines[i],"")[[1]]
  is_tree_vec_short = ifelse(splitted_line==".",F,T)
  modulo = x_pos %% n_char
  x_pos_rel <- ifelse(modulo == 0,31,modulo)
  # print(x_pos_rel)
  if(is_tree_vec_short[x_pos_rel]){
    tree_bump_count <- tree_bump_count + 1
    print("bump!")
    splitted_line[x_pos_rel] <- "X"
  } else {
    splitted_line[x_pos_rel] <- "O"
  }
  print(paste(lines[i],paste(splitted_line,collapse="")))
  x_pos <- x_pos + 3
}

print(tree_bump_count)


# part 2 ------------------------------------------------------------------

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
right_vec <- c(1,3,5,7,1)
down_vec <- c(1,1,1,1,2)
tree_bump_vec <- vector(mode="integer",length=length(down_vec))
for (idx in 1:5){
  n_char <- nchar(lines[1])
  x_pos <- 1
  x_pos_rel <- x_pos
  tree_bump_count <- 0
  for (i in seq(from=1,to = length(lines),by=down_vec[idx])){
    splitted_line <- str_split(lines[i],"")[[1]]
    is_tree_vec_short = ifelse(splitted_line==".",F,T)
    modulo = x_pos %% n_char
    x_pos_rel <- ifelse(modulo == 0,31,modulo)
    # print(x_pos_rel)
    if(is_tree_vec_short[x_pos_rel]){
      tree_bump_count <- tree_bump_count + 1
      print("bump!")
      splitted_line[x_pos_rel] <- "X"
    } else {
      splitted_line[x_pos_rel] <- "O"
    }
    print(paste(lines[i],paste(splitted_line,collapse="")))
    x_pos <- x_pos + right_vec[idx]
  }
  print(tree_bump_count)
  tree_bump_vec[idx] <- tree_bump_count
}

cumprod(tree_bump_vec)


library(tidyverse)

raw_inst <- readLines("data/day_08.txt")

acc <- 0
i <- 1
i_vec <- rep(0,length(raw_inst))
i_vec[i] <- 1


while(!any(i_vec == 2)){
  print(i)
  instr_vec <- str_match_all(raw_inst[i],"(?<commandtype>(?:[a-z]+)) (?<valu>(?:.[0-9]+))")[[1]]
  command_type = instr_vec[2]
  argu = as.integer(instr_vec[3])
  
  if (command_type=="acc"){
    acc <- acc + argu
    i <- i + 1
  } else if (command_type == "jmp"){
    i <- i + argu
  } else if (command_type == "nop"){
    i <- i + 1 
  }
  i_vec[i] <- i_vec[i] + 1 
}



# part 2 ------------------------------------------------------------------



for (j in seq_along(raw_inst)){
  print(j)
  instr_to_mod <- str_match_all(raw_inst[j],"(?<commandtype>(?:[a-z]+)) (?<valu>(?:.[0-9]+))")[[1]]
  command_type = instr_to_mod[2]
  argu = instr_to_mod[3]
  if(command_type == "jmp"){
    mod_inst <- raw_inst
    mod_inst[j] <- paste("nop",argu,sep=" ")
  } else if (command_type == "nop"){
    mod_inst <- raw_inst
    mod_inst[j] <- paste("jmp",argu,sep=" ")
  } else {
    next
  }
  acc <- 0
  i <- 1
  i_vec <- rep(0,length(mod_inst))
  i_vec[i] <- 1
  while(!any(i_vec == 2) & i < (length(i_vec) + 1)){
    print(i)
    instr_vec <- str_match_all(mod_inst[i],"(?<commandtype>(?:[a-z]+)) (?<valu>(?:.[0-9]+))")[[1]]
    command_type = instr_vec[2]
    argu = as.integer(instr_vec[3])
    
    if (command_type=="acc"){
      acc <- acc + argu
      i <- i + 1
    } else if (command_type == "jmp"){
      i <- i + argu
    } else if (command_type == "nop"){
      i <- i + 1 
    }
    if (i == (length(i_vec) + 1))
      stop("here we are!")
    i_vec[i] <- i_vec[i] + 1 
  }
  
}

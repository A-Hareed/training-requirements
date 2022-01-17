## tic tack toe
print('the script works')

# create a matrix for the board
correct <- FALSE
check = NA
while( correct == FALSE) {
  cat("Pick X or O\n")
  user.choice <- readLines("stdin",n=1)
  user.choice <- toupper(as.character(user.choice))
  if (user.choice == 'X' | user.choice == 'O'){
    correct = TRUE
  }
  if(user.choice == 'X' ) {
    comp.choice = 'O'
  } else {
    comp.choice = 'X'
  }
}

cat(paste0('the value piked is  ',user.choice,'\n','comp pike is   ',comp.choice))



board <- matrix(nrow = 3,ncol = 3)
output = '\n\n#####################\n\n'
cat(output)
print(board)
cat(output)

# start the game
outcome_of_game <- 'continue'
game_cycle = 0

avalible_slots <- c(11,12,13,21,22,23,31,32,33)

## function to check if there is a winner
check_a_winner = function(board){
  # check first di-angle 
  di_1 = c(board[1,1],board[2,2],board[3,3])
  U_lett = unique(di_1)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE ){
    return (U_lett)
  }
  
  # check second di-angle
  di_2 = c(board[3,1],board[2,2],board[1,3])
  U_lett = unique(di_2)
  U_num = length(U_lett)
  if (U_num == 1 && is.na(U_lett) == FALSE){
    return (U_lett)
  
  }
  
  # check first row
  row1 = board[1,]
  U_lett = unique(row1)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  # check second row
  row2 = board[2,]
  U_lett = unique(row2)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  # check third row
  row3 = board[3,]
  U_lett = unique(row3)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  # check first col
  col1 = board[,1]
  U_lett = unique(col1)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  # check second col
  col2 = board[,2]
  U_lett = unique(col2)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  # check third col
  col3 = board[,3]
  U_lett = unique(col3)
  U_num = length(U_lett)
  if (U_num == 1  && is.na(U_lett) == FALSE){
    return (U_lett)
  }
  
  return('no winner')
}

## The computer game play
# winning move 
go_for_win = function(board,comp.choice){
  # check first di-angle 
  di_1 = c(board[1,1],board[2,2],board[3,3])
  all_pos = c(1,2,3)
  
  U_lett = unique(di_1)
  U_num = length(U_lett)
  a = table(di_1)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  # check for diagonal win possibility
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(di_1 == comp.choice)
    if(TRUE %in% (di_1 == comp.choice) && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      
      return(c(mat_index,mat_index))
    }
  }
  
  # check for diagonal-2 win possibility
  di_2 = c(board[1,3],board[2,2],board[3,1])
  U_lett = unique(di_2)
  U_num = length(U_lett)
  
  a = table(di_2)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(di_2 == comp.choice)
    if(TRUE %in% (di_2 == comp.choice) && count_comp == 2){
     
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      
      return(c(mat_index,(4-mat_index)))
    }
  }
  
  # check first row
  row1 = board[1,]
  U_lett = unique(row1)
  U_num = length(U_lett)
  
  a = table(row1)
  count_comp = as.vector(a[names(a) == comp.choice])[1]

  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(row1 == comp.choice)
    
    if(TRUE %in% (row1 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      #print(mat_index)
      return(c(1,mat_index))
    }
  }
  
  # check second row
  row2 = board[2,]
  U_lett = unique(row2)
  U_num = length(U_lett)
  
  a = table(row2)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(row2 == comp.choice)
    
    if(TRUE %in% (row2 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]

      return(c(2,mat_index))
    }
  }
  
  # check third row
  row3 = board[3,]
  U_lett = unique(row3)
  U_num = length(U_lett)
  
  a = table(row3)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(row3 == comp.choice)
   
    if(TRUE %in% (row3 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]

      return(c(3,mat_index))
    }
  }
  
  # check first col
  col1 = board[,1]
  U_lett = unique(col1)
  U_num = length(U_lett)
  
  a = table(col1)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(col1 == comp.choice)
    
    if(TRUE %in% (col1 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      
      return(c(mat_index,1))
    }
  }
  
  
  # check second col
  col2 = board[,2]
  U_lett = unique(col2)
  U_num = length(U_lett)
  
  a = table(col2)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(col2 == comp.choice)
    
    if(TRUE %in% (col2 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      
      return(c(mat_index,2))
    }
  }
  
  
  # check third col
  col3 = board[,3]
  U_lett = unique(col3)
  U_num = length(U_lett)
  
  a = table(col3)
  count_comp = as.vector(a[names(a) == comp.choice])[1]
  
  if (U_num == 2  && TRUE %in% is.na(U_lett) ){
    find_none_na = which(col3 == comp.choice)
    
    if(TRUE %in% (col3 == comp.choice)  && count_comp == 2){
      mat_index = all_pos[(! all_pos %in% find_none_na)]
      
      return(c(mat_index,3))
    }
  }
  return('continue')
  
}
  



while(outcome_of_game == 'continue') {
  avalible_slots <- avalible_slots[! avalible_slots %in% c(check) ]
# select user row
  cat("\nchoose a row number: ")
  ur <- readLines('stdin',n=1)
  ur <- as.integer(ur)
  
  # select a user column
  cat("\nchoose a column number: ")
  uc <- readLines('stdin',n=1)
  uc <- as.integer(uc)
  check = as.integer(paste0(ur,uc))   #!(ur %in% avalible_row)
  #print(check)
  # check if the choice is in range
  if (uc > 3 | ur > 3){
    cat("\n\n>>out of range!!! row and column should be between 1 to 3\n")
    cat("choose a row number: \n")
    ur <- readLines('stdin',n=1)
    ur <- as.integer(ur)
    
    # select a user column
    cat("\nchoose a column number: \n")
    uc <- readLines('stdin',n=1)
    uc <- as.integer(uc)
  }
 
  
  game_cycle = game_cycle + 1
  
  
  
  if (game_cycle >=9) {
    outcome_of_game = '###    DRAW    ###'
    break
  }
  
  
  if(check %in% avalible_slots){
    found_in_list <- TRUE
  } else {
    found_in_list <- FALSE
  }
  

  if (found_in_list == FALSE ) {
    cat(paste0('\nused the row and column: ', ur,uc,'\n\n'))
    cat("choose a row number: ")
    ur <- readLines('stdin',n=1)
    ur <- as.integer(ur)
    
    # select a user column
    cat("\nchoose a column number: \n")
    uc <- readLines('stdin',n=1)
    uc <- as.integer(uc)
    
  }
  board[ur, uc] = user.choice
  
  # check if player won 
  winner = check_a_winner(board)
  
  if (winner == user.choice){
    output = '\n\n#####################\n\n'
    cat(output)
    print(board)
    cat(output)
    outcome_of_game = paste0('###    Player ',user.choice,  ' WON!!!!    ###')
    break
  }
  # output = '\n\n#####################\n\n'
  # cat(output)
  # print(board)
  # cat(output)
  
  # remove user slot
  avalible_slots <- avalible_slots[! avalible_slots %in% c(check) ]
  go_win = go_for_win(board,comp.choice) # win
  if (go_win != 'continue'){
    cnr = go_win[1]
    cnc = go_win[2]
    board[cnr, cnc] = comp.choice
    output = '\n\n#####################\n\n'
    cat(output)
    print(board)
    cat(output)
    outcome_of_game = paste0('###    Player ',comp.choice,' WON!!!!    ###')
    break
    
  }
  
  go_win = go_for_win(board,user.choice) # stop player winning
  if (go_win != 'continue'){
    cnr = go_win[1]
    cnc = go_win[2]
  }
  
  
  if (go_win == 'continue'){
    comp_slot = sample(avalible_slots,1)
    cnr = as.integer(substr(comp_slot,1,1))
    cnc = as.integer(substr(comp_slot,2,2))
  }
  
  # print(paste0('computer row: ',cnr))
  # print(paste0('computer column: ',cnc))
 
  check <- as.integer(paste0(cnr,cnc))
  # computer enters
  board[cnr, cnc] = comp.choice
  output = '\n\n#####################\n\n'
  cat(output)
  print(board)
  cat(output)
  
  # check if the game is over 
  game_cycle = game_cycle + 1
  
  # check if computer won 
  winner = check_a_winner(board)
  if (winner == comp.choice){
    outcome_of_game = paste0('###    Player ',comp.choice,' WON!!!!    ###')
    break
  }
  
}



cat(paste0('\n\n\n',outcome_of_game,'\n\n\n'))


# di_1 = c(board[1,1],board[2,2],board[3,3])
# U_num = length(unique(di_1))
# U_num
# di_1[3] = 'O'

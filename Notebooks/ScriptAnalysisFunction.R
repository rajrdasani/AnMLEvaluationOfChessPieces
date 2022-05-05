
library(dplyr)
library(ggplot2)
library(stringr)

#helper function to translate string piece name to current value
name2value_func = function(piece_scriptname) {
  piece_value = 0
  
  #initially was just piecename == "B" but had to change because of the following:
  #for chess notation, if two of the same piece can take the piece, then it will be notated with 2 letters such as Nbxa4 which stands for the Knight on the b file taking the piece on a4
  
  
  if (grepl('N', piece_scriptname) || grepl('B', piece_scriptname)) {
    piece_value = 3
  } else if (grepl('R', piece_scriptname)) {
    piece_value = 5
  } else if (grepl('Q', piece_scriptname)) {
    piece_value = 9
  } else if (piece_scriptname == "" || str_detect(piece_scriptname, "[[:lower:]]")) {
    piece_value = 1
  }
  
  return(piece_value)
} 

#helper function to translate string piece name to full name (for nicer dataframe)
scriptname2fullname_func = function(piece_scriptname) {
  
  fullname = "None"
  
  #initially was just piecename == "B" but had to change because of the following:
  #for chess notation, if two of the same piece can take the piece, then it will be notated with 2 letters such as Nbxa4 which stands for the Knight on the b file taking the piece on a4
  
  if (grepl('N', piece_scriptname)) {
    fullname = "Knight"
  } else if (grepl('B', piece_scriptname)) {
    fullname = "Bishop"
  } else if (grepl('R', piece_scriptname)) {
    fullname = "Rook"
  } else if (grepl('Q', piece_scriptname)) {
    fullname = "Queen"
  } else if (piece_scriptname == "K") {
    fullname = "King"
  } else if (piece_scriptname == "" || str_detect(piece_scriptname, "[[:lower:]]")) {
    fullname = "Pawn"
  }
  
  return(fullname)
}


#helper function to tell us where piece start (use for script analysis)
original_piece_locations = function(piece_color, piece_loc) {
  
  piece = "None"
  
  if(piece_color == 'white') {
    if (piece_loc %in% c('a2', 'b2', 'c2', 'd2', 'e2', 'f2', 'g2', 'h2')) {
      piece = ""
    } else if (piece_loc %in% c('a1', 'h1')) {
      piece = "R"
    } else if (piece_loc %in% c('b1', 'g1')) {
      piece = "N"
    } else if (piece_loc %in% c('c1', 'f1')) {
      piece = "B"
    } else if (piece_loc == 'd1') {
      piece = "Q"
    } else if (piece_loc == 'e1') {
      piece = "K"
    }}
  else {
    if (piece_loc %in% c('a7', 'b7', 'c7', 'd7', 'e7', 'f7', 'g7', 'h7')) {
      piece = ""
    } else if (piece_loc %in% c('a8', 'h8')) {
      piece = "R"
    } else if (piece_loc %in% c('b8', 'g8')) {
      piece = "N"
    } else if (piece_loc %in% c('c8', 'f8')) {
      piece = "B"
    } else if (piece_loc == 'd8') {
      piece = "Q"
    } else if (piece_loc == 'e8') {
      piece = "K"
    }}
  
  return(piece)
}



#helper function for edge case
#EDGE: replace castling with two moves (plus one "Skip" place holder to be able to keep them on the same color)
#not including when a check leads to a check, will skip later \

# use to only replace the one castle so theres no worry of vectorization placing
castling_replace = function(script) {
  
  castling = c('O-O', 'O-O#', 'O-O+', 'O-O-O', 'O-O-O#', 'O-O-O+')
  
  castle_locs = which(script %in% castling)
  castle_loc1 = castle_locs[1]
  castle_mov = script[castle_loc1]
  castling_rep_moves = c("")
  
  ## FIND WHICH MOVES SHOULD BE ADDED IN PLACE OF THE CASTLE 
  
  #if odd move castle, then its a white castle
  if (castle_loc1 %% 2 == 1) {
    #if short castle
    if (castle_mov %in% c('O-O', 'O-O#', 'O-O+')) {
        
      #moves that will be substituted for a castle
      castling_rep_moves = c('Kg1','Skip','Rf1')
        
        
    #if long castle
    } else if (castle_mov %in% c('O-O-O', 'O-O-O#', 'O-O-O+')) {
        
      castling_rep_moves = c('Kc1','Skip','Rd1')
        
    }
      
    #if even move number then its a black castle
  } else {
    if (castle_mov %in% c('O-O', 'O-O#', 'O-O+')) {
        
      castling_rep_moves = c('Kg8','Skip','Rf8')
        
    } else if (castle_mov %in% c('O-O-O', 'O-O-O#', 'O-O-O+')) {
        
      castling_rep_moves = c('Kc8','Skip','Rd8')
    }
  }
  
  #split script such that we can add the 3 moves into the script seamlessly
  script_pre_castle = script[1:castle_loc1-1]
  script_post_castle = script[castle_loc1+1:length(script)]
   
  script_final = c(script_pre_castle, castling_rep_moves, script_post_castle) 
  script_final <- script_final[!is.na(script_final)]
  #substitute in placement x's 
  #script <- rep(script, 1 + (length(castling_rep_moves) - 1)*(script == castle_mov))
  
  #replace
  #script[script == castle_mov] <- castling_rep_moves
  
  return(script_final)
  
}


#function to create dataframe of number of moves, number of checks, number of pieces taken, and the value of pieces taken for each piece for a game, on both sides
game_piece_analysis_func = function(full_script) {
  
  #initialize dataframe for white piece analysis  
  white_piece_analysis = as.data.frame(matrix(nrow = 6, ncol = 4))
  white_piece_analysis[is.na(white_piece_analysis)] = 0
  rownames(white_piece_analysis) = c('Pawn', 'Knight', 'Bishop', 'Rook', 'Queen', 'King')
  colnames(white_piece_analysis) = c('num_moves', 'num_checks', 'num_pieces_taken', 'value_pieces_taken')
  white_piece_analysis$list_pieces_taken = ""
  
  #copy for black, but make sure they are independent
  black_piece_analysis = data.frame(white_piece_analysis)
  
  
  #vectorize script such that its a vector with each element as a move rather than one long string
  script_split = strsplit(full_script,"[[:space:]]")[[1]]
  
  
  #EDGE: replace castling with two moves (plus one to be able to keep them on the same color)
  #not including when a check leads to a check, will skip later 
  
  castling = c('O-O', 'O-O#', 'O-O+', 'O-O-O', 'O-O-O#', 'O-O-O+')
  castling_move_numbers = which(script_split %in% castling)
  total_castles = length(castling_move_numbers)

  while(total_castles > 0) {
    script_split = castling_replace(script_split)
    total_castles = total_castles - 1
  }
  
  #split script into white and black
  script_white = script_split[c(TRUE, FALSE)]
  script_black = script_split[c(FALSE, TRUE)]
  
  
  #Fill in column for number of moves
  
  for (i in c(1:length(script_white))) {
    move = script_white[i]
    
    #skip refers to what was added as a filler move for castling
    if (move == "Skip") {
      next
    }
    first_char = substr(move, 1, 1)
    piece = scriptname2fullname_func(first_char)
    
    
    white_piece_analysis[piece, 'num_moves'] = 
      white_piece_analysis[piece, 'num_moves'] + 1
  }
  
  for (i in c(1:length(script_black))) {
    move = script_black[i]
    
    #skip refers to what was added as a filler move for castling
    if (move == "Skip") {
      next
    }
    
    first_char = substr(move, 1, 1)
    piece = scriptname2fullname_func(first_char)
    
    
    black_piece_analysis[piece, 'num_moves'] = 
      black_piece_analysis[piece, 'num_moves'] + 1
  }
  
  #Fill in column for number of checks
  
  for (i in c(1:length(script_white))) {
    move = script_white[i]
    
    if((grepl("+", move, fixed = TRUE)) | (grepl("#", move, fixed = TRUE))) {
      first_char = substr(move, 1, 1)
      piece = scriptname2fullname_func(first_char)
      
      
      white_piece_analysis[piece, 'num_checks'] = 
        white_piece_analysis[piece, 'num_checks'] + 1
    }
  }
  
  for (i in c(1:length(script_black))) {
    move = script_black[i]
    
    if((grepl("+", move, fixed = TRUE)) | (grepl("#", move, fixed = TRUE))) {
      first_char = substr(move, 1, 1)
      piece = scriptname2fullname_func(first_char)
      
      
      black_piece_analysis[piece, 'num_checks'] = 
        black_piece_analysis[piece, 'num_checks'] + 1
    }
  }
  
  #Fill in columns num_pieces_taken, value_pieces_taken, and list_pieces_taken
  
  for (i in c(1:length(script_white))) {
    white_move = script_white[i]
    #Step 1: Find the moves where a piece was taken (all the moves that have an x in the term)
    if(grepl("x", white_move, fixed=TRUE)) {
      
      
      #Step 2: Extract the move number of the take move and the piece that took \
      ###Example: axb5 leads to the piece_capturer = a & board_loc = b5
      move_num <<- i
      piece_capturer <<- strsplit(white_move, "[x]")[[1]][1]
      #take out any "+" or "#" for checks and checkmates
      board_loc <<- str_replace_all(strsplit(white_move, "[x]")[[1]][2], "[^[:alnum:]]", "")
      
      
      #Step 3: Identify which piece it took by iterating backwards through black pieces, 
      ##if piece_taken is "", its a pawn
      
      #Intialize, used for later in case taken piece hasn't moved
      piece_taken <<- "RESET"
      
      #EN PESSANT EDGE CASE 
      #special_loc will return the square a piece would be taken if an enpessant occured
      #so for white, if board_loc was f6, the special_move would be f5
      special_loc = paste0(strsplit(board_loc, "")[[1]][1], as.numeric(strsplit(board_loc, "")[[1]][2]) - 1)
        
      if(str_detect(piece_capturer, "[[:lower:]]") && grepl('6', board_loc) && grepl(script_black[i-1], special_loc)) {
        piece_taken <<- strsplit(board_loc, "")[[1]][1]
      }
        
        
      if(piece_taken == "RESET") {
        for (j in c((move_num-1):1)) {
          black_move <- script_black[j]
          piece_select = strsplit(black_move, board_loc)[[1]][1]
          
          
          if (grepl(board_loc, black_move, fixed = TRUE)) {
            #take out excess x (which indicated a previous take)
            piece_taken <<- str_replace(piece_select, 'x', '')
            #if successful piece found, break loop
            break
          }
        }
      }
      
      ###Edge case: piece that was taken has not moved yet
      if (piece_taken == "RESET") {
        piece_taken <<- original_piece_locations("black", board_loc)
      }
      
      #step 4: Add values to dataframe
      if(i == move_num) {
        piece_capturer_name = scriptname2fullname_func(piece_capturer)
        piece_taken_name = scriptname2fullname_func(piece_taken)
        piece_taken_value = name2value_func(piece_taken)
        
        white_piece_analysis[piece_capturer_name, 'num_pieces_taken'] = 
          white_piece_analysis[piece_capturer_name, 'num_pieces_taken'] + 1
        
        white_piece_analysis[piece_capturer_name, 'value_pieces_taken'] = 
          white_piece_analysis[piece_capturer_name, 'value_pieces_taken'] + piece_taken_value
        
        string = white_piece_analysis[piece_capturer_name, 'list_pieces_taken']
        new_string = paste(string, piece_taken_name)
        white_piece_analysis[piece_capturer_name, 'list_pieces_taken'] = new_string
        
      }
    }
  }
  
  #Fill in columns for black's side - num_pieces_taken, value_pieces_taken, and list_pieces_taken
  
  for (i in c(1:length(script_black))) {
    black_move = script_black[i]
    #Step 1: Find the moves where a piece was taken (all the moves that have an x in the term)
    if(grepl("x", black_move, fixed=TRUE)) {
      
      
      #Step 2: Extract the move number of the take move and the piece that took \
      ###Example: axb5 leads to the piece_capturer = a & board_loc = b5
      move_num <<- i
      piece_capturer <<- strsplit(black_move, "[x]")[[1]][1]
      board_loc <<- str_replace_all(strsplit(black_move, "[x]")[[1]][2], "[^[:alnum:]]", "")
      
      
      #Step 3: Identify which piece it took by iterating backwards through black pieces, 
      ##if piece_taken is "", its a pawn
      
      #Intialize, used for later in case taken piece hasn't moved
      piece_taken <<- "RESET"
      
      #EN PESSANT EDGE CASE 
      #special_loc will return the square a piece would be taken if an enpessant occured
      #so for white, if board_loc was f6, the special_move would be f5
      special_loc = paste0(strsplit(board_loc, "")[[1]][1], as.numeric(strsplit(board_loc, "")[[1]][2]) + 1)
      
      if(str_detect(piece_capturer, "[[:lower:]]") && grepl('3', board_loc) && grepl(script_white[i], special_loc)) {
        piece_taken <<- strsplit(board_loc, "")[[1]][1]
      }
      
      if(piece_taken == "RESET") {
      #no more move_num - 1 since black moves already happen after white moves
        for (j in c((move_num):1)) {
          white_move <- script_white[j]
          piece_select = strsplit(white_move, board_loc)[[1]][1]
        
          if (grepl(board_loc, white_move, fixed = TRUE)) {
            #take out excess x (which indicated a previous take)
            piece_taken <<- str_replace(piece_select, 'x', '')
            #if successful piece found, break loop
            break
          }
        }
      }
      
      
      
      ###Edge case: white piece that was taken has not moved yet
      if (piece_taken == "RESET") {
        piece_taken <<- original_piece_locations("white", board_loc)
      }
      
      #step 4: Add values to dataframe
      if(i == move_num) {
        piece_capturer_name = scriptname2fullname_func(piece_capturer)
        piece_taken_name = scriptname2fullname_func(piece_taken)
        piece_taken_value = name2value_func(piece_taken)
        
        black_piece_analysis[piece_capturer_name, 'num_pieces_taken'] = 
          black_piece_analysis[piece_capturer_name, 'num_pieces_taken'] + 1
        
        black_piece_analysis[piece_capturer_name, 'value_pieces_taken'] = 
          black_piece_analysis[piece_capturer_name, 'value_pieces_taken'] + piece_taken_value
        
        string = black_piece_analysis[piece_capturer_name, 'list_pieces_taken']
        new_string = paste(string, piece_taken_name)
        black_piece_analysis[piece_capturer_name, 'list_pieces_taken'] = new_string
        
      }
    }
  }
  
  # Because R does not support returning 2 objects cleanly, we will use the function to convert the dataframes into one long vector - making it easy to add to a potential dataframe
  
  white_lst = as.list(white_piece_analysis)
  black_lst = as.list(black_piece_analysis)
  
  output_vector = c(white_lst[[1]], white_lst[[2]], white_lst[[3]], white_lst[[4]], white_lst[[5]],
                    black_lst[[1]], black_lst[[2]], black_lst[[3]], black_lst[[4]], black_lst[[5]])
  
  return(output_vector)
  
}

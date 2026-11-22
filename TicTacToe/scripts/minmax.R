evaluate_game = function(game){
  string = check_win(game)
  values = c("P2", "DRAW", "P1")
  if(string != "CONTINUE"){
    return(match(string, values) - 1)
  }
  return(NA)
}

take_turn = function(move, game){
  game[[game$player]] = append(game[[game$player]], move)
  game$player = (game$player %% 2) + 1
  return(game)
}

rotate_matrix = function(matriz, times){
  for(i in 1:times){
    matriz = t(matriz[nrow(matriz):1,])
  }
  return(m)
}

flip_matrix = function(matriz, axis){
  if(axis == "y"){
    m = matriz
    m[,1] = matriz[,3]
    m[,3] = matriz[,1]
  }
  else{
    m = matriz
    m[1,] = matriz[,3]
    m[3,] = matriz[,1]
  }
  return(m)
}

build_board = function(game){
  matriz = c(2,7,6,9,5,1,4,3,8) |> as.character()
  p1 = unlist(game[[1]]) |> as.character()
  p2 = unlist(game[[2]]) |> as.character()
  matriz[match(p1, matriz)] = "X"
  matriz[match(p2, matriz)] = "O"
  matriz[matriz %in% as.character(1:9)] = " "
  matriz = matrix(matriz, nrow = 3, byrow = TRUE)
  return(matriz)
}

have_sym = function(board1, board2){
  if(all(board1 == rotate_matrix(board2,1)) ||
     all(board1 == rotate_matrix(board2,2)) ||
     all(board1 == rotate_matrix(board2,3)) ||
     all(board1 == t(board2)) ||
     all(board1 == flip_matrix(board2, "x")) ||
     all(board1 == flip_matrix(board2, "y"))){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

check_symmetry = function(options, game){
  p1 = game[[1]] |> unlist()
  p2 = game[[2]] |> unlist()
  to_test = options
  while(length(to_test) > 1){
    count <<- count + 1
    board = build_board(take_turn(to_test[1], game))
    others = to_test[-1]
    for(i in 1:length(others)){
      other_board = build_board(take_turn(others[i], game))
      if(have_sym(board, other_board)){
        options = options[-match(to_test[i], options)]
        to_test = to_test[-match(to_test[i], to_test)]
      }
    }
    to_test = to_test[-1]
  }
  return(options)
}

get_options = function(game){
  taken = unlist(game[[1]], game[[2]])
  if(!is.null(taken)){
    options = seq(1,9)[-match(taken, 1:9)]
  }
  else{
    options = 1:9
  }
  options = check_symmetry(options, game)

  return(options)
}

count = 1

minmax = function(game){
  count <<- count + 1
  if(count > 1000){
    stop("Ufa)")
  }
  board = build_board(game)
  options = get_options(game)
  static_eval = evaluate_game(game)
  if(!is.na(static_eval)){
    return(static_eval)
  }
  if(game$player == 1){
    maxEval = -Inf
    for(child in options){
      print(board)
      new_game = take_turn(child, game)
      eval = minmax(new_game)
      maxEval = max(maxEval, eval)
    }
    return(maxEval)
  }
  else{
    minEval = Inf
    for(child in options){
      print(board)
      new_game = take_turn(child, game)
      eval = minmax(new_game)
      minEval = min(minEval, eval)
    }
    return(minEval)
  }
}

find_move = function(game){
  options = get_options(game)
  vetor = vector()
  for(option in options){
    vetor = c(vetor, minmax(take_turn(option, game)))
  }
  choose = vetor[vetor == max(vetor)][1]
  return(choose)
}
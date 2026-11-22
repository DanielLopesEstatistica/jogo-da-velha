create = function(){
  game = list(player1 = list(), player2 = list(), player = 1, state = "CONTINUE")
  return(game)
}

move = function(number, game){
  if(number < 0 || number > 9){
    stop("Invalid Number")
  }
  square = c(2,7,6,9,5,1,4,3,8)
  game[[game$player]] = append(game[[game$player]], square[number])
  return(game)
}

check_win = function(game){
  p1 = length(game[[1]])
  p2 = length(game[[2]])
  if(p1 > 2){
    cases = expand.grid(replicate(p1, 0:1, simplify = FALSE))[-1,]
    cases = cases[rowSums(cases)>2,] |> as.matrix()
    vetor = unlist(game[[1]]) |> as.matrix() 
    check = rowSums(cases %*% vetor)
    if(any(check == 15)){
      return("P1")
    }
  }
  if(p2 > 2){
    cases = expand.grid(replicate(p2, 0:1, simplify = FALSE))[-1,]
    cases = cases[rowSums(cases)>2,] |> as.matrix()
    vetor = unlist(game[[2]]) |> as.matrix()

    check = rowSums(cases %*% vetor)
    if(any(check == 15)){
      return("P2")
    }
  }
  if(p1 + p2 == 9){
    return("DRAW")
  }
  return("CONTINUE")
}

player_move = function(game){
  texto = paste0("Player ", game$player, " turn, pick a square from 1 to 9.")
  number = readline(texto) |> as.numeric()
  return(number)
}

pc_move = function(game, strategy = "minmax"){
  source(paste0("scripts/",strategy, ".R"))
  number = find_move(game)
  return(number)
}

graph = function(game){
  matriz = c(2,7,6,9,5,1,4,3,8) |> as.character()
  p1 = unlist(game[[1]]) |> as.character()
  p2 = unlist(game[[2]]) |> as.character()
  matriz[match(p1, matriz)] = "X"
  matriz[match(p2, matriz)] = "O"
  matriz[matriz %in% as.character(1:9)] = " "
  matriz = matrix(matriz, nrow = 3, byrow = TRUE)
  print(matriz)
}

interactive_game = function(){
  game = create()
  while(game$state == "CONTINUE"){
    graph(game)
    game = move(player_move(game), game)
    game$state = check_win(game)
    game$player = ((game$player) %% 2) + 1
  }
  graph(game)
  print("Jogo terminou")
}

pc_game = function(show = FALSE){
  game = create()
  while(game$state == "CONTINUE"){
    if(show){
      graph(game)
    }
    print("turn")
    game = move(pc_move(game), game)
    game$state = check_win(game)
    game$player = ((game$player) %% 2) + 1
  }
  if(show){
    graph(game)
    print("Jogo terminou")
  }
}
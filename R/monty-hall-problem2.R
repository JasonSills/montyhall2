#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select the first door.
#' @description
#'   `select_door()` generates the first door our of three that 
#'   the contestant picks.  
#' @details
#'   The initial door picked is the first move in the game. 
#'   In this step The contestant picks the initial door. 
#'   The door is either door number 1, door number 2, or door number 3.
#'   This pick sets up the rest of the game where a goat door is shown and 
#'   the contestant can choose to either stay with the door chosen in this
#'   step or switch doors.
#' @param ... no arguments are used by the function.
#' @return The function returns a numeric value representing 1 door, the 
#'   door picked by the contestant.
#' @examples
#'   return(a.pick)
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Opens a goat door. 
#' @description
#'   `open_goat_door()` simulates the reveal of one of the doors with
#'   a goat. 
#' @details
#'   Following the picking of the initial door two doors remain. One
#'   of these doors is a door with a goat behind it. In this round
#'   one of the doors with a goat is identified. This function takes into
#'   account the door picked in the select_door function and identifies 
#'   a goat door. If the initial pick was a door with a goat the remaining
#'   goat door is opened. If the initial pick had the car one of the goat
#'   doors is chosen at random. 
#' @param 
#'   This function uses game and a.pick as parameters.  
#' @return 
#'   The function returns a numeric value representing 1 door, the 
#'   numeric value associated with a door with a goat.
#' @examples
#'   return(opened.door)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change door strategy.
#' @description
#'   `change_door()` simulates a strategy of switching from the initial pick
#'   to the remaining door. 
#' @details
#'   This function simulates the change the door strategy. In this strategy 
#'   the contestant decides to switch from their initial pick chosen 
#'   in the select_door function. The final pick is neither the door selected
#'   in select_door or open_goat_door. The if statement identifies this as 
#'   final.pick and is returned if the change door strategy is chosen. 
#' @param
#'   The function utilizes opened.door and a.pick and sets the stay value 
#'   to T. 
#' @return 
#'   The The function returns a numeric value representing 1 door, 
#'   neither the door in select_door or opened_goat_door. 
#' @examples
#'   change_door()
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine winner
#' @description
#'  `determine_winner()`determines if the contestant chose the door
#'  with the car or goat.
#' @details
#'  This function identifies if the contestant won or lost the game.
#'  If the final pick was the door associated with the car the 
#'  contestant wins. If the final pick was door associated with the
#'  goat the contestant loses. 
#' @param 
#'   This function is dependent on final.pick and game. 
#' @return 
#'   The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#' @examples
#'   determine_winner()
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Play Monty Hall Game
#' @description
#'   `play_game()` plays the Monty Hall game. 
#' @details
#'   This function plays the Monty Hall game. The function creates the game, 
#'   chooses the initial door, opens a door with a goat, and runs the 
#'   simulation for both staying with the initial door and switching doors. 
#'   The function assesses the outcomes of both switch and stay strategies.
#'   The game provides results for either switching or staying with the
#'   initial door. 
#' @param 
#'   ... no arguments are used by the function. 
#' @return 
#'   The function returns character vectors of the stay and switch strategies
#'   indicating win or lose. 
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Play n Number of Game
#' @description
#'   `play_n_games()` plays the Monty Hall game as many times as the
#'   simulator wants to play.  
#' @details
#'   The function allows the simulator to choose an n for the number of times
#'   the simulator wishes to simulate the Monty Hall game. Choosing the n
#'   allows the simulator to choose a number and calculate the ideal
#'   winning strategy. Running the simulation a large number of times
#'   will indicate whether the strategy of stay or switch is most 
#'   likely to result in a win.
#' @param 
#'   Setting the n of the function.
#' @return 
#'   Table of numeric values indicating the percentage of times a win occurred.
#' @examples
#' play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

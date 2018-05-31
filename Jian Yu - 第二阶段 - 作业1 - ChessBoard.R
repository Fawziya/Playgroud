# define check board. 
# 0: a cell is not visited
# 1:a cell is visited.
ChessBoard <- matrix(0,nrow=8,ncol=8)
# a piece starts from top left corner
# mark 1
ChessBoard[1][1] <- 1

# write move_chess function
# input: two arguments - current position as vector
# and move
move_chess <- function(currPosVec,move){
    if (move == 1) {
      if (currPosVec[2] == 8) {
        return(c(currPosVec[1],currPosVec[2]-2))
      } else {
        return(c(currPosVec[1],currPosVec[2]+1))
      }
    } else if(move == 2) {
      if (currPosVec[1] == 8) {
        return(c(currPosVec[1]-2,currPosVec[2]))
      } else {
        return(c(currPosVec[1]+1,currPosVec[2]))
      }      
    } else if(move == 3) {
      if (currPosVec[2] == 1) {
        return(c(currPosVec[1],currPosVec[2]+2))
      } else {
        return(c(currPosVec[1],currPosVec[2]-1))
      }      
    } else if(move == 4) {
      if (currPosVec[1] == 1) {
        return(c(currPosVec[1]+2,currPosVec[2]))
      } else {
        return(c(currPosVec[1]-1,currPosVec[2]))
      }   
    } else {
      return(currPosVec)
    }
}

# main code
MoveCount <- 0
currPos <- c(1,1)
while(sum(ChessBoard) != 64) {
  # generate a randome number between 1-6
  dice <- sample(1:6,1)
  nextMove <- move_chess(currPos,dice)
  ChessBoard[nextMove[1],nextMove[2]] <- 1
  currPos <- nextMove
  MoveCount <- MoveCount + 1
}
print(MoveCount)
 

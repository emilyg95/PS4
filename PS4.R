car_door = function(x){
  x = sample(1:3, 1)
  return(x)
} ## a function that randomly selects a number between 1 and 3 for the placement of the car

car1 = car_door("car1")

car_pos = function(door_choice, car_door){ ## a function that takes as inputs the choice of door and the placement of the car
  if (door_choice == car_door){  ## if then statement which says to return true if the choice of door equals the placement of the car and false otherwise
    return(TRUE)
    } 
  else {
    return(FALSE)
  }
} ## returns TRUE if the inputs are equal and FALSE if they are not

car_pos(2, car1) ## test

setClass(Class = "door", ## creates a new class "door"
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical"
           )
           ## indicates 3 inputs; chosenDoor which must be an integer, carDoor which must be an integer, switch which must be a logical (boolean)
)

getSlots("door") ## check

newDoor = function(x, y, z){ ## construction function takes in 3 arguments x, y, and z
  object = new("door", chosenDoor = x, carDoor = y, switch = z) ## creates a new object setting the 3 inputs into the slots I defined earlier
  return(object) ## returns the object
}

newDoor(3,2,TRUE) ## check

Door1 = newDoor(3,2,TRUE) ## defines an object I want to be valid

Door2 = newDoor(3,7,TRUE) ## defines an object I want to be invalid

checkValidity = function(object){ ## creates a function to check the validity of objects of class door
  test1 = (object@chosenDoor == 1)
  test2 = (object@chosenDoor == 2)
  test3 = (object@chosenDoor == 3)
  test4 = (object@carDoor == 1)
  test5 = (object@carDoor == 2)
  test6 = (object@carDoor == 3)
  test7 = (object@switch == TRUE)
  test8 = (object@switch == FALSE) ## a number of test statements defining valid inputs
  if (!test1 & !test2 & !test3 | !test4 & !test5 & !test6 | !test7 & !test8){ ## if then statement which checks to see if the inputs x and y are both equal to 1 2 or 3 and input z is a boolean (TRUE/FALSE)
    return("object is not a valid value")} ## returns true if the above holds and object is not a valid value if not
    else{
      return(TRUE)}
}

checkValidity(Door1) ## check on valid object

checkValidity(Door2) ## check on invalid object

setValidity("door", checkValidity) ## a function which only allows objects of class door to be created if they meet the criteria of my validity function above

validObject(Door1) ## check on valid object

validObject(Door2) ## check on invalid object 

new("door", chosenDoor = 3, carDoor = 2, switch = TRUE) ## check on valid object

new("door", chosenDoor = 3, carDoor = 6, switch = TRUE) ## check on invalid object 

generic = function(object = "door"){ ## creates interior function for setGeneric, which defines the function as PlayGame and its generic input as of class door
  standardGeneric("PlayGame")
}

setGeneric("PlayGame", generic) ## sets the generic of function PlayGame as the function I just created

chooseDoor = function(object){ ## creates a function for the interior of setMethod
  object@carDoor = sample(1:3, 1) ## sets the input for the carDoor slot as a randomly selected number between 1 and 3
  chosenDoor1 = sample(1:3, 1) ## chooses a number between 1 and 3 randomly to be the player's first selection 
  removeDoor = as.numeric(sample(1:3, 1) != object@carDoor | chosenDoor1) ## selects a door to be removed which is a random number between 1 and 3 not equal to either carDoor or the player's initial chosen door
  if (object@switch == FALSE){ ## if else statement which sets chosenDoor equal to the initially selected door chosenDoor1 if switch is FALSE
    object@chosenDoor = chosenDoor1
    return(object) ## returns the new object
  }
  else{
    object@chosenDoor = as.numeric(sample(1:3, 1) != chosenDoor1 | removeDoor) ## sets chosen door equal to a new value between 1 and 3 not equal to either the initial chosen door or the removed door if switch is TRUE
    return(object) ## returns the new object
  }
}

CD1 = 2  ## creates a test value to check interior of function
RD1 = 3 ## creates a test value to check interior of function
x = as.numeric(sample(1:3, 1) != CD1 | RD1) ## check

Door3 = chooseDoor(Door1) ## check

setMethod("PlayGame", "door", ## creates a new method for door objects called PlayGame
          function(object){ ## function which intakes an object of class door
            object = chooseDoor(object) ## sets a new object using the chooseDoor function I just created
            if (object@chosenDoor == object@carDoor){ ## if else statements that creates an object called winner and sets it equal to TRUE if the chosen door is equal to the door with the car
              winner = TRUE
              return(winner) ## returns winner
            }
            else{
              winner = FALSE ## if else statements that creates an object called winner and sets it equal to FALSE if the chosen door is not equal to the door with the car
              return(winner) ## returns winner
            }
          }
)
          
PlayGame(Door3) ## check




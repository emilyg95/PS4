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
}

# Should return a TRUE if these samples are equal and
# a false if they are not

car_pos(2, car1) ## test

setClass(Class = "door", ## creates a new class "door"
         representation = representation(
           chosenDoor = "integer",
           carDoor = "integer",
           switch = "logical"
           )
           ## indicates 3 inputs; carDoor which must be an integer, switch which must be a boolean, and winner which must be a boolean
)

getSlots("door")





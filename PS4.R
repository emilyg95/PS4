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

newDoor(3,2,TRUE)









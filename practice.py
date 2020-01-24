def testMethod(bins): #//get number of bins passed by R Shiny server
  if bins is not None:
    string = "I came from a Python Function" 
    return "You have selected " +bins+ " and " +string
    
def defineName(na):
  if (na=="propane"):
    na = "Ashley"
  elif(na=="oxygen"):
    na = "Patricia"
  else:
    na = "Helen"
    
  return na
  

  


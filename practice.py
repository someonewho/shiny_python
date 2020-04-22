def testMethod(bins): #//get number of bins passed by R Shiny server
  if bins is not None:
    string = "I came from a Python Function" 
    return "You have selected " +bins+ " and " +string
    
def defineName(na):
  if (na=="propane0"):
    na = "prop0"
  elif(na="propane1"):
    na = "prop1"
  elif(na=="propane2"):
    na = "prop2"
  return na
  

  


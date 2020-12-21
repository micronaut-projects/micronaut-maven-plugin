def isCi = System.getenv('CI')
return isCi ? false : true
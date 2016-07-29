prod <- array(NA, dim=c(1,14,151))

prod[1,1,1:121] <- swcalcdata$`Sawn Wood Production` 
prod[1,2,1:121] <- swcalcdata$`Structural Panel Production`
prod[1,3,1:121] <- swcalcdata$`Nonstructural Panels Production`
prod[1,4,1:121] <- swpcalcdata$`Other Products Production`



##############################array for imports
imports <- array(NA, dim=c(1,14,151))

imports[1,1,1:121] <- swcalcdata$`Sawn Wood Imports`
imports[1,2,1:121] <- swcalcdata$`Structural Panel Imports`
imports[1,3,1:121] <- swcalcdata$`Nonstructural Panels Imports`
imports[1,4,1:121] <- 0



#########################array for exports
exports <- array(NA, dim=c(1,14,151))

exports[1,1,1:121] <- swcalcdata$`Sawn Wood Exports`
exports[1,2,1:121] <- swcalcdata$`Structural Panel Exports`
exports[1,3,1:121] <- swcalcdata$`Nonstructural Panels Exports`
exports[1,4,1:121] <- 0

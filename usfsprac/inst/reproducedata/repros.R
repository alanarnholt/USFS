library(devtools)
library(repmis)
library(xlsx)
hair1958 <- read.xlsx("./data/CopyOfData/hair1958.xlsx",1,header = FALSE)
use_data(hair1958, overwrite = TRUE)

hair1963 <- read.xlsx("./data/CopyOfData/hair1963tab2.xlsx", 1, header=FALSE)
use_data(hair1963, overwrite = TRUE)

hair1963t20 <- read.xlsx("./data/CopyOfData/hair1963t20.xlsx",1,header=FALSE)
use_data(hair1963t20, overwrite = TRUE)

hair1963t21 <- read.xlsx("./data/CopyOfData/hair1963t21.xlsx",1,header=FALSE)
use_data(hair1963t21, overwrite = TRUE)

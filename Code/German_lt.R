### Prepare German data
# Henrik-Alexander schubert
# 24.05.2023


## Preperations ------------------------------------------

library(tidyverse)
library(HMDHFDplus)

## Passwords  ------------------------------------------

# Set the account details for global data lab
GDLun <- "schubert@demogr.mpg.de"
GDLpw <- "2021Demographie"

# Password and username for the US mortality database
myHFDusername <-  "schubert@demogr.mpg.de" #"INSERT_YOUR_USMDB_USERNAME" 
myHFDpassword <-  "E-Dagger2023"           #"INSERT_YOUR_USMDB_PASSWORD" 

## Wrangling ------------------------------------------


# Load the TFR
tfr <- readHFDweb(CNTR = "DEUTW", item = "tfrRR", username = myHFDusername, password = myHFDpassword)


# Load the TFR
lt1 <- readHMDweb(CNTR = "DEUTNP", item = "fltper_1x1",  username = myHFDusername, password = myHFDpassword)
lt_old <- read.csv("Data/german_lifetables.csv")

# Get the ax-columns
ax <- subset(lt1, subset = Year == 1990, select = c(ax, Age))


# Subset the data
lt_old <- subset(lt_old, subset = Sex == 2 & Year1 == Year2 & Year1 >= 1971 & AgeInt == 1 &  Year1 < 1990)

# Are there duplicated rows
lt_old <- unique(lt_old)

# Rename variables
lt_old <- rename(lt_old, Year = Year1, dx = d.x.)

# Merge ax and lt
lt_old1 <- left_join(lt_old[lt_old$AgeInt == 1,  ], ax) 


# Create ax for the last age interval
lt_old1 <- lt_old1 %>% group_by(Year) %>% 
  mutate(maxAge = max(Age)) %>% 
  mutate(ax = if_else(Age == maxAge, (l.x. / m.x.) / dx, ax)) %>% 
  select(Year, Age, ax, dx)


# Bind both life tables together
lt <- rbind(lt_old1,  lt1[, c("Year", "Age", "ax", "dx")])


# Estimate the HLI  
HLI <- lt %>% mutate(d = dx/100000) %>% 
  group_by(Year) %>% summarise(HLI = prod((Age+ax)^d), .groups = "drop") %>% 
  select(Year, HLI)

# Join the data
d <- left_join(tfr, HLI)

# Save the data
save(d, file = "Data/german_lt.Rda")


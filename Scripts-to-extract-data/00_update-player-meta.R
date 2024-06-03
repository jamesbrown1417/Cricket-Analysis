#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(cricketdata)

# Update player metadata table
player_meta <- update_player_meta()

# Write out as RDS
write_rds(player_meta, "Data/player_meta_updated.rds")
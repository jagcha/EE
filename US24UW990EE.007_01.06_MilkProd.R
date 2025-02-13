############################### Objective ######################################
# Potential problem: suddenly one analysis is giving a significant negative 
# genetic correlation. Why such difference?

# Report.


################################### Notes ######################################

############################## Initialization ##################################
rm(list = ls())

CN <- 'US24UW990EE.007_01.06_MilkProd'

################################ Packages ######################################
require(ssh)

######################### Stablish connection to HPG ###########################
session <- ssh_connect("agustinchasco@hpg2.rc.ufl.edu")

######################### Define links to hpg files ############################
path <- '/blue/mateescu/agustinchasco/Projects/EstrusExpression/validation/update/codes/myblup'
dirs1 <- c('20_GenCorrMilkEE', '21_GenCorrMilkEE', '22_GenCorrMilkEE')
add <- 'codes'
file_out <- 'o.out'

####################### Define link in your computer ###########################
mydir <- '/Users/agustinchasco/Documents/UW Madison/Core/Research/Data/'

################################ Read outputs ##################################

fdf <- data.frame(
  dir1 = logical(0), dir2 = logical(0),
  PE1 = logical(0), SE1 = logical(0),
  PE2 = logical(0), SE2 = logical(0),
  PE3 = logical(0), SE3 = logical(0)
)

idx = 0
for (dir1 in dirs1) {
  path_i <- file.path(path, dir1, add)
  encripted <- ssh_exec_internal(session, paste0('ls -l ', path_i))
  string <- rawToChar(encripted$stdout)
  dirs2 <- unlist(regmatches(string, 
                             gregexpr("\\d{2}_[^\\s]+(?=\\b)", 
                                      string, perl = TRUE)))
  for (dir2 in dirs2) {
    idx = idx + 1
    link_i <- file.path(path_i, dir2, file_out)
    scp_download(session, link_i, mydir)
    Sys.sleep(1)
    out <- readLines(file.path(mydir, file_out))
    
    line1PE_idx <- grep('Function: g_3_3_1_1', out) + 2
    line1SE_idx <- grep('Function: g_3_3_1_1', out) + 3
    line1PE <- out[line1PE_idx]
    line1SE <- out[line1SE_idx]
    PE1 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line1PE))
    SE1 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line1SE))
    
    line2PE_idx <- grep('Function: g_3_3_2_2', out) + 2
    line2SE_idx <- grep('Function: g_3_3_2_2', out) + 3
    line2PE <- out[line2PE_idx]
    line2SE <- out[line2SE_idx]
    PE2 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line2PE))
    SE2 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line2SE))
    
    line3PE_idx <- grep('Function: g_3_3_1_2', out) + 2
    line3SE_idx <- grep('Function: g_3_3_1_2', out) + 3
    line3PE <- out[line3PE_idx]
    line3SE <- out[line3SE_idx]
    PE3 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line3PE))
    SE3 <- as.numeric(sub(pattern = '.*:\\s*(.*)', replacement = '\\1', line3SE))
    
    fdf[idx, 'dir1'] <- dir1
    fdf[idx, 'dir2'] <- dir2
    fdf[idx, 'PE1'] <- PE1
    fdf[idx, 'SE1'] <- SE1
    fdf[idx, 'PE2'] <- PE2
    fdf[idx, 'SE2'] <- SE2
    fdf[idx, 'PE3'] <- PE3
    fdf[idx, 'SE3'] <- SE3
    
    cat('dir1 = ', dir1, ', dir2 = ', dir2, '. Finished! \n')

  }
}

################################ Plot results ##################################







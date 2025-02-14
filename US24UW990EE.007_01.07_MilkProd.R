############################### Objective ######################################
# Connect to hpg user. Read files. Extract data. Plot results.


################################### Notes ######################################

############################## Initialization ##################################
rm(list = ls())

CN <- 'US24UW990EE.007_01.07_MilkProd'

################################ Packages ######################################
require(ggplot2)
require(ssh)

######################### Stablish connection to HPG ###########################
session <- ssh_connect("agustinchasco@hpg2.rc.ufl.edu")

######################### Define links to hpg files ############################
path <- '/blue/mateescu/agustinchasco/Projects/EstrusExpression/validation/update/codes/myblup'
dirs1 <- c('20_GenCorrMilkEE', '21_GenCorrMilkEE', '22_GenCorrMilkEE', 
           '23_GenCorrMilkEE', '24_GenCorrMilkEE')
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

# traits_fix <- c('Test', 'P305M')
# mapper_raw <- c('01', '02', '03', '04')
# mapper_trait <- c('CO', 'DU', 'AS', 'RS')


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

CO <- fdf[substr(fdf$dir2, 1, 2) %in% '01', ]
DU <- fdf[substr(fdf$dir2, 1, 2) %in% '02', ]
AS <- fdf[substr(fdf$dir2, 1, 2) %in% '03', ]
RS <- fdf[substr(fdf$dir2, 1, 2) %in% '04', ]


CO$LB <- CO$PE3 - 2 * CO$SE3
CO$UB <- CO$PE3 + 2 * CO$SE3
DU$LB <- DU$PE3 - 2 * DU$SE3
DU$UB <- DU$PE3 + 2 * DU$SE3
AS$LB <- AS$PE3 - 2 * AS$SE3
AS$UB <- AS$PE3 + 2 * AS$SE3
RS$LB <- RS$PE3 - 2 * RS$SE3
RS$UB <- RS$PE3 + 2 * RS$SE3

CO$LABEL <- paste0(substr(CO$dir1, 1, 3), CO$dir2)
CO$LABEL <- paste0(substr(CO$dir1, 1, 3), CO$dir2)
DU$LABEL <- paste0(substr(DU$dir1, 1, 3), DU$dir2)
DU$LABEL <- paste0(substr(DU$dir1, 1, 3), DU$dir2)
AS$LABEL <- paste0(substr(AS$dir1, 1, 3), AS$dir2)
AS$LABEL <- paste0(substr(AS$dir1, 1, 3), AS$dir2)
RS$LABEL <- paste0(substr(RS$dir1, 1, 3), RS$dir2)
RS$LABEL <- paste0(substr(RS$dir1, 1, 3), RS$dir2)


## CO
ggplot(CO, aes(x = PE3, y = LABEL)) +
  geom_point(color = "blue", size = 2) +  # Point estimates
  geom_errorbarh(aes(xmin = LB, xmax = UB), height = 0.3, color = "red") +  # Confidence intervals
  theme_minimal() +
  labs(x = "Point Estimate (PE3)", y = NULL, title = "Point Estimates with Confidence Intervals") +
  theme(
    strip.text = element_blank(),  # Removes facet titles to save space
    axis.text.y = element_text(size = 8),  # Adjusts label size
    panel.spacing.y = unit(0.2, "lines")  # Reduces vertical space between facets
  )

## DU
ggplot(DU, aes(x = PE3, y = LABEL)) +
  geom_point(color = "blue", size = 2) +  # Point estimates
  geom_errorbarh(aes(xmin = LB, xmax = UB), height = 0.3, color = "red") +  # Confidence intervals
  theme_minimal() +
  labs(x = "Point Estimate (PE3)", y = NULL, title = "Point Estimates with Confidence Intervals") +
  theme(
    strip.text = element_blank(),  # Removes facet titles to save space
    axis.text.y = element_text(size = 8),  # Adjusts label size
    panel.spacing.y = unit(0.2, "lines")  # Reduces vertical space between facets
  )

## AS
ggplot(AS, aes(x = PE3, y = LABEL)) +
  geom_point(color = "blue", size = 2) +  # Point estimates
  geom_errorbarh(aes(xmin = LB, xmax = UB), height = 0.3, color = "red") +  # Confidence intervals
  theme_minimal() +
  labs(x = "Point Estimate (PE3)", y = NULL, title = "Point Estimates with Confidence Intervals") +
  theme(
    strip.text = element_blank(),  # Removes facet titles to save space
    axis.text.y = element_text(size = 8),  # Adjusts label size
    panel.spacing.y = unit(0.2, "lines")  # Reduces vertical space between facets
  )



## RS
ggplot(RS, aes(x = PE3, y = LABEL)) +
  geom_point(color = "blue", size = 2) +  # Point estimates
  geom_errorbarh(aes(xmin = LB, xmax = UB), height = 0.3, color = "red") +  # Confidence intervals
  theme_minimal() +
  labs(x = "Point Estimate (PE3)", y = NULL, title = "Point Estimates with Confidence Intervals") +
  theme(
    strip.text = element_blank(),  # Removes facet titles to save space
    axis.text.y = element_text(size = 8),  # Adjusts label size
    panel.spacing.y = unit(0.2, "lines")  # Reduces vertical space between facets
  )

############################## Disconnect session ##############################

ssh_disconnect(session)


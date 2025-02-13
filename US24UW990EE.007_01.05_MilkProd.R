############################### Objective ######################################
# Same than `US24UW990EE.007_01.04_MilkProd.R`.
# Main difference: keeping the 22% missing observations w/o milk production.
# Replace NA by -999/ Save file.


################################### Notes ######################################

############################## Initialization ##################################
rm(list = ls())

CN <- 'US24UW990EE.007_01.05_MilkProd'

################################ Packages ######################################
require(boxr)
require(data.table)
require(dplyr)
require(ggplot2)

############################ Connection to server ##############################
box_auth()

################################ Reading files #################################
# Navigate until reach required folder.
fls <- as.data.frame(box_ls(306172225609))$id
i <- 0
fdfl <- list()
for (f in fls) {
  i = i + 1
  fdfl[[i]] <- box_read(file_id = f)
  cat('File ', i, ' ', f, ' read. \n')
}

############################ Stacking data-frames ##############################
# Collapse all in a single data-frame (data is not to large).
fdf <- rbindlist(fdfl)

########################### Elimination of duplications ########################
# Eliminate consistent duplications.
fdf1 <- fdf[!duplicated(fdf[, c('ID', 'LACT', 'PEN', 'FDAT', 
                                'PM1', 'TDAT1',
                                'PM2', 'TDAT2',
                                'PM3', 'TDAT3', 'P305M')]), ]

################################# Release Memory ###############################
rm(list = ls()[!ls() %in% 'fdf1'])

################################# Handle dates #################################
fdf1$FDAT <- as.Date(fdf1$FDAT, format = '%m/%d/%y')
fdf1$TDAT1 <- as.Date(fdf1$TDAT1, format = '%m/%d/%y')
fdf1$TDAT2 <- as.Date(fdf1$TDAT2, format = '%m/%d/%y')
fdf1$TDAT3 <- as.Date(fdf1$TDAT3, format = '%m/%d/%y')

############################# Chronological spanning ###########################
# Pheno dataset has the EE data from 2021 to 2022
# Then, just keep TDAT1 or TDAT2 spanning 2020 to 2023.
# Also, keep NAs. I want to identify if at least the Cow-Lact is in the dataset.
# FDAT must be between 2020 to 2022. Non NA allowed in FDAT.

# Condition for FDAT:
c1.1 <- !is.na(fdf1$FDAT)
c1.2 <- year(fdf1$FDAT) %in% 2020:2022
c1 <- c1.1 & c1.2

# Condition for TDAT1:
c2.1 <- is.na(fdf1$TDAT1)
c2.2 <- year(fdf1$TDAT1) %in% 2020:2023
c2 <- c2.1 | c2.2

# Condition for TDAT2:
c3.1 <- is.na(fdf1$TDAT2)
c3.2 <- year(fdf1$TDAT2) %in% 2020:2023
c3 <- c3.1 | c3.2

# Condition for TDAT3:
c4.1 <- is.na(fdf1$TDAT3)
c4.2 <- year(fdf1$TDAT3) %in% 2020:2024
c4 <- c4.1 | c4.2

# 4 conditions must be TRUE.
fdf2 <- fdf1[c1 & c2 & c3 & c4, ]

################################# Release Memory ###############################
rm(list = ls()[!ls() %in% 'fdf2'])

####################### Add milk production to pheno-dataset ###################

# Similar rationale than `US24UW990EE.007_01.02_MilkProd.R`
phe <- read.csv('/Users/agustinchasco/Documents/UW Madison/Core/Research/Data/repro_EE/PHE02.01.txt',
                header = FALSE, sep = ' ')

colnames(phe) <- c('ID2', 'LAC2', 'YS', 
                   'EECOUNT', 'EEMEANDU', 'EEMEANAS', 'EEMEANRS')

############################## Span milk records by ID #########################

vect_IDs <- sub(pattern = '(^\\d{1,})_.*', 
                replacement = '\\1', x = phe$ID2)
phe_IDs <- unique(as.numeric(vect_IDs))
fdf3 <- fdf2[fdf2$ID %in% phe_IDs, ]

############################## Assert duplications #############################
stopifnot(sum(duplicated(fdf3[, c('ID', 'LACT', 'PEN', 'FDAT')])) == 0)
stopifnot(sum(duplicated(fdf3[, c('ID', 'LACT', 'PEN')])) == 0)

# See cases.., but strange ones. Again:
# "Don't look for a needle in a haystack when all you need is the hay."
fdf3 <- fdf3[order(fdf3$ID, fdf3$FDAT), ]
check <- fdf3[fdf3$ID %in% fdf3$ID[duplicated(fdf3[, c('ID', 'LACT')])] & 
                fdf3$LACT %in% fdf3$LACT[duplicated(fdf3[, c('ID', 'LACT')])], ]
check <- check[order(check$ID, check$FDAT), ]

# Basically, few IDs have different FDAT but same LACT.
# Among them, some have few days between FDATs, while other have 1 year.

######################## Target and correct duplications #######################

fdf3_c <- fdf3[!fdf3$ID %in% check$ID, ]
stopifnot(sum(duplicated(fdf3_c[, c('ID', 'LACT')])) == 0) 

correct <- fdf3[fdf3$ID %in% check$ID, ]

correct <- correct %>% 
  group_by(ID, LACT) %>%
  mutate(DIFF = c(0, diff(FDAT)))

sort(correct$DIFF[correct$DIFF != 0])

# Keep in mind: just few animals! do not waste your time here.
# Assume these are different lactations, and ad +1 to the subsequent lactation.
correct <- correct[order(correct$ID, correct$FDAT), ]
correct <- correct %>% 
  group_by(ID) %>%
  mutate(LACT = rep(min(LACT), length(LACT)) + seq(from = 0, to = length(LACT)-1))
correct$DIFF <- NULL

fdf3 <- rbind(fdf3_c, correct)
stopifnot(sum(duplicated(fdf3[, c('ID', 'LACT')])) == 0)

######################### Lower and upper bounds ###############################
# Use information of `YS` in `phe` to define a lower and upper bound for 
# acceptable FDAT in `fdf3`.

# Given a value of `YS`, I can estimate the lower and upper date such that
# this value is possible.

# To the lower possible date, I will subtract 70 days. This is my Lower Bound LB.
# To the upper date, I will leave it untouched. This is my upper bound UB.

# If a FDAT is between LB and UB, then testing date linked to such FDAT is added.

# Dummy columns
phe$UB <- phe$LB <- NA

# Logical vector with TRUE in desired position
W_idxs <- sub(pattern = '(\\w{, })_.*', 
              replacement = '\\1', x = phe$YS) == 'W'
Sp_idxs <- sub(pattern = '(\\w{, })_.*', 
               replacement = '\\1', x = phe$YS) == 'Sp'
Su_idxs <- sub(pattern = '(\\w{, })_.*', 
               replacement = '\\1', x = phe$YS) == 'Su'
F_idxs <- sub(pattern = '(\\w{, })_.*', 
              replacement = '\\1', x = phe$YS) == 'F'
stopifnot(sum(W_idxs & Sp_idxs & Su_idxs & F_idxs) == 0)

## Below, a code that could be improved. When now end of months may change. But remember:
# "Don't look for a needle in a haystack when all you need is the hay."
# "It's like rearranging deck chairs on the Titanic."
# "It's like putting lipstick on a pig."
# "It's like polishing a cannonball before it's fired."

# Define Bounds for EE events in Winter:
phe$LB[W_idxs] <- paste0('20/09/', as.numeric(sub(pattern = '.*_(\\d{,})',
                                                  replacement = '\\1', 
                                                  x = phe$YS[W_idxs])) - 1)
phe$UB[W_idxs] <- paste0('28/02/', sub(pattern = '.*_(\\d{,})',
                                       replacement = '\\1', 
                                       x = phe$YS[W_idxs]))

# Define Bounds for EE events in Spring:
phe$LB[Sp_idxs] <- paste0('20/12/', as.numeric(sub(pattern = '.*_(\\d{,})',
                                                   replacement = '\\1', 
                                                   x = phe$YS[Sp_idxs])) - 1)
phe$UB[Sp_idxs] <- paste0('31/05/', sub(pattern = '.*_(\\d{,})',
                                        replacement = '\\1', 
                                        x = phe$YS[Sp_idxs]))

# Define Bounds for EE events in Summer:
phe$LB[Su_idxs] <- paste0('20/03/', sub(pattern = '.*_(\\d{,})',
                                        replacement = '\\1', 
                                        x = phe$YS[Su_idxs]))
phe$UB[Su_idxs] <- paste0('31/08/', sub(pattern = '.*_(\\d{,})',
                                        replacement = '\\1', 
                                        x = phe$YS[Su_idxs]))

# Define Bounds for EE events in Fall:
phe$LB[F_idxs] <- paste0('20/06/', sub(pattern = '.*_(\\d{,})',
                                       replacement = '\\1', 
                                       x = phe$YS[F_idxs]))
phe$UB[F_idxs] <- paste0('30/11/', sub(pattern = '.*_(\\d{,})',
                                       replacement = '\\1', 
                                       x = phe$YS[F_idxs]))

## Note for UB: I am not really sure if 31/mm/yyyy is a valid date. 
# Eventually, I may need to correct this stuff.

############################# Add extra columns ################################
add_df <- data.frame(matrix(ncol = ncol(fdf3), nrow = nrow(phe)))
add_cols <- colnames(fdf3)
colnames(add_df) <- add_cols
phe2 <- cbind(phe, add_df)

################################## Add Rows ####################################
for (i in 1:nrow(phe2)) {
  ID_i <- sub(pattern = '(^.{1,})_.*', replacement = '\\1', x = phe2$ID2[i])
  LB_i <- as.Date(phe2$LB[i], format = '%d/%m/%Y')
  UB_i <- as.Date(phe2$UB[i], format = '%d/%m/%Y')
  FDATs <- as.Date(fdf3$FDAT[fdf3$ID %in% ID_i], format = '%m/%d/%Y')
  LACT_i <- phe2$LAC2[i]
  LACTs <- fdf3$LACT[fdf3$ID %in% ID_i]
  if (LACT_i %in% LACTs) {
    phe2[i, add_cols] <- fdf3[fdf3$ID %in% ID_i & fdf3$LACT %in% LACT_i, ]
  } else {
    boolcheck <- FDATs >= LB_i & FDATs <= UB_i
    if (sum(boolcheck) != 1) {
      cat('Row ', i, ' with EE from ID ', ID_i, 
          ' in YS ', phe2$YS[i], ' w/o spanned milk test. \n')
      next
    }
    target_FDA <- FDATs[boolcheck]
    phe2[i, add_cols] <- fdf3[fdf3$ID %in% ID_i & fdf3$FDAT %in% target_FDA, ]
  }
}

################################################################################
###################### Quantify missing milk records ###########################

## CRF!!! Something strange in the YS label. 
sum(is.na(phe2$ID))
100*sum(is.na(phe2$ID))/nrow(phe2)

############################# Why still 30% missing? ###########################
## Potentially needs further analysis....
# Navigate until reach required folder.
# Below, you can use the raw data to compare datasets and visualize why is missing.

fls <- as.data.frame(box_ls(306172225609))$id
i <- 0
fdfl <- list()
for (f in fls) {
  i = i + 1
  fdfl[[i]] <- box_read(file_id = f)
  cat('File ', i, ' ', f, ' read. \n')
}

# Collapse all in a single data-frame (data is not to large).
fdf <- rbindlist(fdfl)

# Eliminate consistent duplications.
fdf1 <- fdf[!duplicated(fdf[, c('ID', 'LACT', 'PEN', 'FDAT', 
                                'PM1', 'TDAT1',
                                'PM2', 'TDAT2',
                                'PM3', 'TDAT3', 'P305M')]), ]

### Visual inspection.
## check 1:
ch1 <- fdf1[fdf1$ID == 126, ]
ch1 <- ch1[order(ch1$LACT), ]

## Check 2:
ch2 <- fdf1[fdf1$ID == 46816, ] # Crf
ch2 <- ch2[order(ch2$LACT), ]

## Check 3:
ch3 <- fdf1[fdf1$ID == 37971, ]
ch3 <- ch3[order(ch3$LACT), ]

## Check 4:
ch4 <- fdf1[fdf1$ID == 609, ]
ch4 <- ch4[order(ch4$LACT), ]

## Check 5:
ch5 <- fdf1[fdf1$ID == 47615, ]
ch5 <- ch5[order(ch5$LACT), ]

############################ Potential improvement #############################
# White elephants in the room:
## Why sometimes my label YS is incompatible with the FDAT?
## Why some missing LACTATIONs?

# Again: errors go to errors, and stats gives you tools to quantify if such errors are significnat.
# Do not search for a needle in a haystack when you only need the hay.
# Carry on with the result, and then take actions to see how impactful was the correction.
# My concern is that, oftentimes I invest lot of time and effort in doing a
# correction, and at the end of the day, such correction does not changes the
# results.

################################################################################
################################################################################

############################### Useful data type ###############################
# Keep rows with missing data.

phe2_2 <- phe2 # Code carry over. Just keep it.

phe2_2$FDAT <- as.Date(phe2_2$FDAT, origin = "1970-01-01")
phe2_2$TDAT1 <- as.Date(phe2_2$TDAT1, format = '%m/%d/%Y')
phe2_2$TDAT2 <- as.Date(phe2_2$TDAT2, format = '%m/%d/%Y')
phe2_2$TDAT3 <- as.Date(phe2_2$TDAT3, format = '%m/%d/%Y')

phe2_2$difF1 <- as.numeric(phe2_2$TDAT1 - phe2_2$FDAT)
phe2_2$difF2 <- as.numeric(phe2_2$TDAT2 - phe2_2$FDAT)
phe2_2$difF3 <- as.numeric(phe2_2$TDAT3 - phe2_2$FDAT)

hist(phe2_2$difF1)
hist(phe2_2$difF2)
hist(phe2_2$difF3)

######################### Quantify PMs equal to 0 ##############################

# Not big issue. Minimal impact. Do not waste your time in this topic. 
# Fixing this is like polishing a single brick in a wall.

sum(phe2_2$PM1[!is.na(phe2_2$PM1)] == 0)
100*sum(phe2_2$PM1[!is.na(phe2_2$PM1)] == 0) / nrow(phe2_2)

sum(phe2_2$PM2[!is.na(phe2_2$PM2)] == 0) # Missing data (no-test) is 0.
100*sum(phe2_2$PM2[!is.na(phe2_2$PM2)] == 0) / nrow(phe2_2)

sum(phe2_2$PM3[!is.na(phe2_2$PM3)] == 0) # Missing data (no-test) is 0.
100*sum(phe2_2$PM3[!is.na(phe2_2$PM3)] == 0) / nrow(phe2_2)

################################ Little trick ##################################
# Few lines after, I will take the average of PM1, PM2 and PM3 whenever a 
# criteria holds.

# The point, is that I also have NAs due missing testing records for some 
# IDs and Lactations. Then, I want to index -999 in these fields with missing
# milk production records.

# Due to the NAs in missing rows, Ifilters below may mess-up stuff.
# By doing some tricks, allow rows with NAs to escape future filters.

phe2_2$PM1[is.na(phe2_2$ID)] <- 
  phe2_2$PM2[is.na(phe2_2$ID)] <- 
  phe2_2$PM3[is.na(phe2_2$ID)] <- 
  phe2_2$P305M[is.na(phe2_2$ID)] <- -999 # -999 indicates missing data.

phe2_2$difF1[is.na(phe2_2$ID)] <- 
  phe2_2$difF2[is.na(phe2_2$ID)] <- 
  phe2_2$difF3[is.na(phe2_2$ID)] <- 60 # This diff will not be detected.

################################ Simplify stuff ################################

tagcols <- c('ID2', 'LAC2', 'YS', 
             'EECOUNT', 'EEMEANDU', 'EEMEANAS', 'EEMEANRS', 
             'PM1', 'PM2', 'PM3', 'difF1', 'difF2', 'difF3', 'P305M')

allcols <- colnames(phe2_2)

delcols <- allcols[!allcols %in% tagcols]

phe2_2[, delcols] <- NULL

######################### Average PM with restrictions #########################
# General idea: sum whatever holds fixed criteria, and divide by the number of added elements.

# Think in vectors.
vect_deno <- rep(x = 3, nrow(phe2_2))
corr1 <- rep(x = 0, nrow(phe2_2))
corr2 <- rep(x = 0, nrow(phe2_2))
corr3 <- rep(x = 0, nrow(phe2_2))

# Handle criteria in test 1: add little trick to 'forgive' missing rows.
cond1 <- phe2_2$difF1 < 10 |  phe2_2$difF1 > 80 | is.na(phe2_2$difF1)
corr1[cond1] <- -1
PM1 <- phe2_2$PM1
PM1[cond1] <- 0

# Handle criteria in test 2:
cond2 <- phe2_2$difF2 < 10 |  phe2_2$difF2 > 80 | is.na(phe2_2$difF2)
corr2[cond2] <- -1
PM2 <- phe2_2$PM2
PM2[cond2] <- 0

# Handle criteria in test 3:
cond3 <- phe2_2$difF3 < 10 |  phe2_2$difF3 > 80 | is.na(phe2_2$difF3)
corr3[cond3] <- -1
PM3 <- phe2_2$PM3
PM3[cond3] <- 0

# Compile corrections:
corr <- corr1 + corr2 + corr3
denominator <- vect_deno + corr
PM <- PM1 + PM2 + PM3
phe2_2$PMavg <- PM/denominator
hist(phe2_2$PMavg, breaks = 100)
sum(is.na(phe2_2$PMavg)) ## Denominator was 0!
# Replace by -999
phe2_2$PMavg[is.na(phe2_2$PMavg)] <- -999
stopifnot(sum(is.na(phe2_2$PMavg)) == sum(is.na(phe2_2$P305M)) &
            sum(is.na(phe2_2$PMavg)) == 0)

################################ Simplify stuff ################################
phe2_2$PM1 <- phe2_2$PM2 <- phe2_2$PM3 <- phe2_2$difF1 <- phe2_2$difF2 <- phe2_2$difF3 <- NULL
stopifnot(sum(is.na(phe2_2)) == 0)

# To avoid modifications in parameter file, swap position of 2 last columns.
coln <- colnames(phe2_2)
newcoln <- c(coln[1:(length(coln)-2)], coln[length(coln)], coln[length(coln)-1])
phe2_2 <- phe2_2[, newcoln]

# Round number to 3 digits. It may avoid problems in BLUPF90..... Annoying.
phe2_2$EEMEANDU <- round(x = phe2_2$EEMEANDU, digits = 3)
phe2_2$EEMEANAS <- round(x = phe2_2$EEMEANAS, digits = 3)
phe2_2$EEMEANRS <- round(x = phe2_2$EEMEANRS, digits = 3)

########################### Sumarize milk production ###########################

ggplot(phe2_2, aes(x = PMavg)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "black") +
  labs(x = "PMavg", y = "Count", title = "Histogram of PMavg") +
  theme_minimal()

ggplot(phe2_2, aes(x = P305M)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "black") +
  labs(x = "P305M", y = "Count", title = "Histogram of P305M") +
  theme_minimal()

################################ Save txt file #################################
# Saving criteria: must be ready to be used by BLUPF90+
tagfolder <- '/Users/agustinchasco/Documents/UW Madison/Core/Research/Data'
filename <- file.path(tagfolder, paste0(CN, '.txt'))
write.table(x = phe2_2, file = filename,
            quote = FALSE, sep = ' ', row.names = FALSE, col.names = FALSE)

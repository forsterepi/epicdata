# Seed
set.seed(123)

# Define length of input data
n_entry <- 200

# Define ID of entry
entry_id <- c(1:n_entry) %>% as.character()

# Define ID of participant (some participants have double entries)
how_many_entries <- sample(c(1,2),size = n_entry, replace = T, prob = c(0.67,0.33))

i <- 1
while (sum(how_many_entries[1:i]) < n_entry) {
  i <- i + 1
}
how_many_entries <- how_many_entries[1:i]

if (sum(how_many_entries) > n_entry) {
  how_many_entries[i] <- 1
}

n <- length(how_many_entries)

part_ids <- paste0("ID",c(1:length(how_many_entries)) %>% as.character() %>% stringi::stri_pad(width = 3, side = "left", pad = "0"))

# Define a few categorical variables
male <- sample(c("0","1",NA), size = n, replace = T, prob = c(0.45,0.45,0.1))
asthma_diag <- sample(c("1","2","3",NA), size = n, replace = T, prob = c(0.3,0.3,0.3,0.1))

# Define a few numerical variables
height <- rnorm(n, 170, 15) %>% round(0)
height_na <- sample(c(0,1), size = n, replace = T, prob = c(0.9,0.1))
height[height_na == 1] <- NA

weight <- rnorm(n, 70, 10) %>% round(0)
weight_na <- sample(c(0,1), size = n, replace = T, prob = c(0.9,0.1))
weight[weight_na == 1] <- NA

# Define a multiple choice variable
mc1 <- sample(c("0","1"), size = n, replace = T, prob = c(0.85,0.15))
mc2 <- sample(c("0","1"), size = n, replace = T, prob = c(0.85,0.15))
mc3 <- sample(c("0","1"), size = n, replace = T, prob = c(0.85,0.15))
mc4 <- sample(c("0","1"), size = n, replace = T, prob = c(0.85,0.15))
mc_other <- sample(c("0","1"), size = n, replace = T, prob = c(0.85,0.15))

mc_empty_na <- sample(c(1:n), size = 20, replace = F)
mc_empty <- mc_empty_na[c(1:10)]
mc_na <- mc_empty_na[c(11:20)]

mc1[mc_empty] <- ""
mc2[mc_empty] <- ""
mc3[mc_empty] <- ""
mc4[mc_empty] <- ""
mc_other[mc_empty] <- ""

mc1[mc_na] <- NA
mc2[mc_na] <- NA
mc3[mc_na] <- NA
mc4[mc_na] <- NA
mc_other[mc_na] <- NA

rnd_text <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
rnd_text %<>% paste0(., sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))

mc_ops <- sample(c(NA,"","1"), size = n, replace = T, prob = c(0.1,0.75,0.15))
mc_ops[mc_ops == "1" & !is.na(mc_ops)] <- rnd_text[mc_ops == "1" & !is.na(mc_ops)]

# Define a string variable
jobs <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
jobs %<>% paste0(., sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
jobs_na <- sample(c(0,1), size = n, replace = T, prob = c(0.9,0.1))
jobs[jobs_na == 1] <- NA

# Define some datetime variables
birthday <- sample(c(1:20000), size = n, replace = T) %>% lubridate::as_date() %>% as.character()

date_of_something_datetime <- sample(c(1:20000), size = n, replace = T) %>% lubridate::as_date()
date_of_something <- paste0(lubridate::day(date_of_something_datetime),".",lubridate::month(date_of_something_datetime),".",lubridate::year(date_of_something_datetime))
dos_na <- sample(c(0,1), size = n, replace = T, prob = c(0.9,0.1))
date_of_something[dos_na == 1] <- NA

# Combine to data.frame
sim_df <- data.frame(part_ids = part_ids,
                     male = male,
                     asthma_diag = asthma_diag,
                     height = height,
                     weight = weight,
                     mc1 = mc1,
                     mc2 = mc2,
                     mc3 = mc3,
                     mc4 = mc4,
                     mc_other = mc_other,
                     mc_ops = mc_ops,
                     jobs = jobs,
                     birthday = birthday,
                     date_of_something = date_of_something)

# Expand to 200 individuals % include some changes in duplicated entries
out <- vector(mode = "list", length = n)

for (i in 1:n) {
  temp <- sim_df[i,]
  if (how_many_entries[i] == 2) {
    temp <- rbind(sim_df[i,],sim_df[i,])

    changes <- sample(c(0,1), size = 13, replace = T, prob = c(0.9, 0.1))
    changes %<>% c(0,.)

    if (changes[2] == 1) {temp[2,2] <- sample(c("0","1",NA), size = 1, replace = T, prob = c(0.45,0.45,0.1))}
    if (changes[3] == 1) {temp[2,3] <- sample(c("1","2","3",NA), size = 1, replace = T, prob = c(0.3,0.3,0.3,0.1))}
    if (changes[4] == 1) {temp[2,4] <- rnorm(1, 170, 15) %>% round(0)}
    if (changes[5] == 1) {temp[2,5] <- rnorm(1, 70, 10) %>% round(0)}
    if (changes[6] == 1) {temp[2,6] <- sample(c("0","1"), size = 1, replace = T, prob = c(0.85,0.15))}
    if (changes[7] == 1) {temp[2,7] <- sample(c("0","1"), size = 1, replace = T, prob = c(0.85,0.15))}
    if (changes[8] == 1) {temp[2,8] <- sample(c("0","1"), size = 1, replace = T, prob = c(0.85,0.15))}
    if (changes[9] == 1) {temp[2,9] <- sample(c("0","1"), size = 1, replace = T, prob = c(0.85,0.15))}
    if (changes[10] == 1) {temp[2,10] <- sample(c("0","1"), size = 1, replace = T, prob = c(0.85,0.15))}
    if (changes[11] == 1) {temp[2,11] <- sample(c(NA,"","xxxxxxxxxx"), size = 1, replace = T, prob = c(0.1,0.75,0.15))}
    if (changes[12] == 1) {temp[2,12] <- sample(c(NA,"","xxxxxxxxxx"), size = 1, replace = T, prob = c(0.1,0.75,0.15))}
    if (changes[13] == 1) {temp[2,13] <- sample(c(1:20000), size = 1, replace = T) %>% lubridate::as_date() %>% as.character()}
    if (changes[14] == 1) {
      date_temp <- sample(c(1:20000), size = 1, replace = T) %>% lubridate::as_date()
      temp[2,14] <- paste0(lubridate::day(date_temp),".",lubridate::month(date_temp),".",lubridate::year(date_temp))
    }
  }

  out[[i]] <- temp
}

out %<>% purrr::map_dfr(as.data.frame)
sim_df <- out

sim_df$entry_id <- entry_id
rownames(sim_df) <- c(1:nrow(sim_df))

# use_data
usethis::use_data(sim_df, overwrite = TRUE)

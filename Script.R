library(tidyverse)
library(stringr)

# load X_train as character columns to avoid problems
X_train <- read_delim("./data/train/X_train.txt", delim = " ",
                       col_names = FALSE,
                       col_types = strrep("c",561)
                       )

# load subjects train
subj_train <- read_delim("./data/train/subject_train.txt", delim = " ",
                      col_names = "subj_id"
)

# load y_train
y_train <- read_delim("./data/train/y_train.txt", delim = " ",
                      col_names = "activity"
                      )
# bind the 3 train sets into one dataframe
train <- cbind(X_train, subj_train, y_train)


# load X_test
X_test <- read_delim("./data/test/X_test.txt", delim = " ",
                      col_names = FALSE,
                      col_types = strrep("c",561)
                     )

# load subjects test
subj_test <- read_delim("./data/test/subject_test.txt", delim = " ",
                         col_names = "subj_id"
)

# load y_test
y_test <- read_delim("./data/test/y_test.txt", delim = " ",
                      col_names = "activity"
                     )

# bind the 3 test sets into one dataframe
test <- cbind(X_test, subj_test, y_test)


# create the full dataset combining train and test sets
full_df <- rbind(train, test)

# first we trim residual white spaces and then we convert to numeric
full_df <- full_df %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate_if(is.character, as.numeric)

# calculate means per column
col_mean <- colMeans(full_df)
print(col_mean)

# calculate std dev per column
col_std <-  apply(full_df, 2, sd, na.rm=T)
print(col_std)

# load the activity labels
act_labels <- read_delim("./data/activity_labels.txt", delim = " ",
                         col_names = c("id", "key")
                         )

# change activity column to factor
full_df$activity <- as.factor(full_df$activity)
# rename levels to match the labels
levels(full_df$activity) <- act_labels$key

# load the variable names
variables <- read_delim("./data/features.txt", delim = " ",
                         col_names = c("id", "name")
)

# rename dataset columns using the variable names and make sure they are unique
names(full_df)[1:561] <- make.names(variables$name, unique=TRUE)


# group by activity and subject and calculate means across numeric columns
grouped_mean <- full_df %>% 
  group_by(subj_id, activity) %>% 
  summarise(across(where(is.numeric), mean))

if(!file.exists("./data")){dir.create("./data")}
write_csv(grouped_mean, "./data/grouped_mean.csv")

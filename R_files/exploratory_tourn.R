source("R_files/merge_data.R")


colnames(PLAYER_tourn_non_goal) <- gsub(" ","_", colnames(PLAYER_tourn_non_goal))


check_dup_columns <- function(df) {
    for (i in colnames(df)) {
        for (j in colnames(df)) {
            if (i == j) {
                next
            } else {
                if (all(!is.na(df[[i]])) & all(!is.na(df[[j]])) & sum(df[[i]] == df[[j]])/nrow(df) > 0.9) {
                    print(paste(i,j))
                }
            }
        }
    }
}

check_dup_columns(PLAYER_tourn_non_goal)
dup_cols <- c("Nation.x","Nation.y","Pos.x","Pos.y")
PLAYER_tourn_non_goal<- PLAYER_tourn_non_goal %>% select(-all_of(dup_cols))

PLAYER_tourn_non_goal

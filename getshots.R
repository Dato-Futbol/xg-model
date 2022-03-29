get_shots <- function(file_path, name_detail, save_files = T){
        
        players <- fromJSON("players.json")
        
        shots <- fromJSON(file_path) %>%
                 filter(subEventName == "Shot")
        
        tags <- tibble(tags = shots$tags) %>%
                hoist(tags, 
                      tags_id = "id") %>%
                unnest_wider(tags_id, names_sep = "")
        
        tags2 <- tags %>%
                 mutate(is_goal = ifelse(rowSums(. == "101", na.rm = T) > 0, 1, 0),
                        is_blocked = ifelse(rowSums(. == "2101", na.rm = T) > 0, 1, 0),
                        is_CA = ifelse(rowSums(. == "1901", na.rm = T) > 0, 1, 0),
                        body_part = ifelse(rowSums(. == "401", na.rm = T) > 0, "left", 
                                           ifelse(rowSums(. == "402", na.rm = T) > 0, "right", 
                                                  ifelse(rowSums(. == "403", na.rm = T) > 0, "head/body", "NA"))))
        
        pos <- tibble(positions = shots$positions) %>%
                hoist(positions, 
                      y = "y",
                      x = "x") %>%
                unnest_wider(y, names_sep = "") %>%
                unnest_wider(x, names_sep = "") %>%
                dplyr::select(-c(x2, y2))
        
        shots_ok <- shots %>%
                dplyr::select(matchId, teamId, playerId, eventSec, matchPeriod) %>%
                bind_cols(pos, tags2) %>%
                filter(is_blocked == 0) %>%
                dplyr::select(-c(8:13)) %>%
                left_join(players %>%
                                  dplyr::select(c("wyId", "foot")), by = c("playerId" = "wyId")) %>%
                mutate(league = name_detail)
        
        if(save_files){
                write_rds(shots, paste0("shots", name_detail, ".rds"))
                write_rds(tags2, paste0("tags2", name_detail, ".rds"))
                write_rds(pos, paste0("pos", name_detail, ".rds"))
                write_rds(shots_ok, paste0("unblocked_shots", name_detail, ".rds"))
        }
        
        shots_ok
}

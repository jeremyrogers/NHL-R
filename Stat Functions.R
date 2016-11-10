get.goals <- function(player_id, events) {
  nrow(subset(events, (etype == "GOAL" & ev.player.1 == player_id)))
}

get.assists <- function(player_id, events) {
  nrow(subset(events, ((etype == "GOAL") & (ev.player.2 == player_id 
                       | ev.player.3 == player_id))))
}

get.points <- function(player_id, events) {
  get.goals(player_id, events) + get.assists(player_id, events)
}

get.shots <- function(player_id, events) {
  nrow(subset(events, ((etype == "GOAL" | etype == "SHOT") & ev.player.1 == player_id)))
}

get.icorsi <- function(player_id, events) {
  nrow(subset(events, ((etype == "GOAL" | etype == "SHOT" | etype == "BLOCK" | etype == "MISS") 
                       & ev.player.1 == player_id)))
}

get.ifenwick <- function(player_id, events) {
  nrow(subset(events, ((etype == "GOAL" | etype == "SHOT" | etype == "MISS") 
                       & ev.player.1 == player_id)))
}

get.corsi.for <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "BLOCK" | etype == "GOAL"))
  corsi_for <- nrow(subset(shots, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                          a4 == player_id | a5 == player_id | a6 == player_id) 
                                       & ev.team == awayteam) 
                                      | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                            h4 == player_id | h5 == player_id | h6 == player_id) 
                                         & ev.team == hometeam))))
  
  corsi_for
}

get.corsi.against <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "BLOCK" | etype == "GOAL"))
  
  corsi_against <- nrow(subset(shots, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                              a4 == player_id | a5 == player_id | a6 == player_id) 
                                           & ev.team == hometeam) 
                                          | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                                h4 == player_id | h5 == player_id | h6 == player_id) 
                                             & ev.team == awayteam))))
  
  corsi_against
}

get.corsi <- function(player_id, events) {
  
  corsi_for <- get.corsi.for(player_id, events)
  corsi_against <- get.corsi.against(player_id, events)
  
  corsi <- corsi_for - corsi_against
  corsi
}

get.fenwick.for <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "GOAL"))
  fenwick_for <- nrow(subset(shots, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                          a4 == player_id | a5 == player_id | a6 == player_id) 
                                       & ev.team == awayteam) 
                                      | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                            h4 == player_id | h5 == player_id | h6 == player_id) 
                                         & ev.team == hometeam))))
  
  fenwick_for
}

get.fenwick.against <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "GOAL"))
  fenwick_against <- nrow(subset(shots, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                            a4 == player_id | a5 == player_id | a6 == player_id) 
                                         & ev.team == hometeam) 
                                        | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                              h4 == player_id | h5 == player_id | h6 == player_id) 
                                           & ev.team == awayteam))))
  
  fenwick_against
}

get.fenwick <- function(player_id, events) {
  fenwick_for <- get.fenwick.for(player_id, events)
  fenwick_against <- get.fenwick.for(player_id, events)
  
  fenwick <- fenwick_for - fenwick_against
  fenwick
  
}

player.table <- function(roster, events) {
  player <- roster$firstlast
  position <- roster$pos
  id <- roster$player.id
  player.ids <- roster$player.id
  points <- sapply(roster$player.id, get.points, events = events)
  goals <- sapply(roster$player.id, get.goals, events = events)
  assists <- sapply(roster$player.id, get.assists, events = events)
  full.stats <- data.frame(player, position, id, points, goals, assists)
}

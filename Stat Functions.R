get.corsi.for <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "BLOCK" | etype == "GOAL"))
  corsi_for <- length(subset(events, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                          a4 == player_id | a5 == player_id | a6 == player_id) 
                                       & ev.team == awayteam) 
                                      | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                            h4 == player_id | h5 == player_id | h6 == player_id) 
                                         & ev.team == hometeam))))
  
  corsi_for
}

get.corsi.against <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "BLOCK" | etype == "GOAL"))
  
  corsi_against <- length(subset(events, (((a1 == player_id | a2 == player_id | a3 == player_id | 
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
  fenwick_for <- length(subset(events, (((a1 == player_id | a2 == player_id | a3 == player_id | 
                                          a4 == player_id | a5 == player_id | a6 == player_id) 
                                       & ev.team == awayteam) 
                                      | ((h1 == player_id | h2 == player_id | h3 == player_id | 
                                            h4 == player_id | h5 == player_id | h6 == player_id) 
                                         & ev.team == hometeam))))
  
  fenwick_for
}

get.fenwick.against <- function(player_id, events) {
  shots <- subset(events, (etype =="MISS" | etype == "SHOT" | etype == "GOAL"))
  fenwick_against <- length(subset(events, (((a1 == player_id | a2 == player_id | a3 == player_id | 
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

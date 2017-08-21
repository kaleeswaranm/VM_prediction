brackets <- function(master, l_r, u_r, l_t) {
  info           <- data.frame(l_r = l_r, u_r = u_r, l_t = l_t)
  bracket_edit   <- list()
  bracket_cancel <- list()
  for(i in 1:nrow(info)) {
    bracket_edit[[i]] <- subset(master, orders >= info$l_t[i])
    bracket_edit[[i]] <- bracket_edit[[i]][(info$l_r[i] < (bracket_edit[[i]]$edited_not_cancelled / bracket_edit[[i]]$orders) & 
                                                          (bracket_edit[[i]]$edited_not_cancelled / bracket_edit[[i]]$orders) <= info$u_r[i]),]
    bracket_edit[[i]]$fraction <- bracket_edit[[i]]$edited_not_cancelled / bracket_edit[[i]]$orders
    if(nrow(bracket_edit[[i]]) >= 1){
      bracket_edit[[i]]$range    <- paste(info$l_r[i], info$u_r[i], sep = "-") 
    }
  }
  for(i in 1:nrow(info)) {
    bracket_cancel[[i]] <- subset(master, orders >= info$l_t[i])
    bracket_cancel[[i]] <- bracket_cancel[[i]][(info$l_r[i] < (bracket_cancel[[i]]$edited_and_cancelled / bracket_cancel[[i]]$orders) & 
                                                              (bracket_cancel[[i]]$edited_and_cancelled / bracket_cancel[[i]]$orders) <= info$u_r[i]),]
    bracket_cancel[[i]]$fraction <- bracket_cancel[[i]]$edited_and_cancelled / bracket_cancel[[i]]$orders
    if(nrow(bracket_cancel[[i]]) >= 1){
      bracket_cancel[[i]]$range    <- paste(info$l_r[i], info$u_r[i], sep = "-") 
    }
  }
  return(list(edit = bracket_edit, cancel = bracket_cancel))
}

l_r  <- c(0.5,0.3,0.25,0.20,0.15,0.1)
u_r  <- c(0.7,0.5,0.30,0.25,0.20,0.15)
l_t  <- c(8,10,25,25,25,25)
bracketlist <- brackets(master, l_r, u_r, l_t)
e <- do.call(rbind, bracketlist$edit)
c <- do.call(rbind, bracketlist$cancel)
write.csv(e, "edits_VM.csv")
write.csv(c, "cancel_VM.csv")
unique(c$area)
c_s <- c[c$area %in% c("Old Gurgaon (Zone 6)", "Sector-14", "Huda City", "Jalvayu Towers", "Palam Vihar", "Cyber City", "sohna road"),]
c_a <- c[c$area %in% c("Kirti Nagar", "Uttam Nagar", "Janakpuri", "Punjabi Bagh", "Dwarka", "Rajouri Garden"),]
c_m <- c[c$area %in% c("Hauz Khas", "South Extension", "Malviya Nagar", "Chattarpur"),]
write.csv(c_s, "c_s.csv")
write.csv(c_a, "c_a.csv")
write.csv(c_m, "c_m.csv")

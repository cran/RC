RC.print.html <-
function (a="") {
    a <- gsub("<br>","",a)
    a <- gsub("<center>","",a)
    a <- gsub("</center>","",a)
    #find tables
    a.arr <- strsplit(a,"</table><table>")
    #find rows
    for (i in 1:length(a.arr[[1]])) {
      a <- a.arr[[1]][i]
      a <- gsub("<table>","",a)
      a <- gsub("</table>","",a)
      r.arr <- strsplit(a,"</tr><tr>")
      #find columns
      nr <- length(r.arr[[1]])
      for (j in 1:nr) {
        r <- r.arr[[1]][j]
        r <- gsub("<tr>","",r)
        r <- gsub("</tr>","",r)
        c.arr <- strsplit(r,"</td><td>")
        if (j==1) {
          nc <- length(c.arr[[1]])
          nc.alt <- 0
          try(nc.alt <- as.numeric(strsplit(strsplit(c.arr[[1]],"colspan=")[[1]][2],">")[[1]][1]),silent=TRUE)
          try(if (nc.alt > nc) nc <- nc.alt)
        }
        for (c in 1:nc) {
          if ((j==1) & (c==1)) {
            cells <- array(NA,dim=c(nr,nc))
          }
          cell <- c.arr[[1]][c]
          cell <- gsub("<td>","",cell)
          cell <- gsub("</td>","",cell)
          cell <- gsub("(<[^>]*>)","",cell)
          cells[j,c] <- cell
        }
      }
      print(cells)
    }
}


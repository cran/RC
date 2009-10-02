RC.report.user <-
function(id="UseR user ", quote=TRUE) {
  r <- RC.ls.user(id=id)
  if (is.null(r) == FALSE) {
    #r <- r[r$user==gsub("'","",id),]
    numcomp <- length(r[,1])
    module.table <- table(r$module)
    keywords.table <- table(r$keywords)
    year.array <- substr(r$date,1,4)
    month.array <- substr(r$date,6,7)
    day.array <- substr(r$date,9,10)
    year.month.array <- substr(r$date,1,7)
    year.month.day.array <- substr(r$date,1,10)
    hour.array <- substr(r$date,12,13)
    year.table <- table(year.array)
    month.table <- table(months(as.Date(r$date)))
    day.table <- table(weekdays(as.Date(r$date)))
    year.month.table <- table(year.month.array)
    year.month.day.table <- table(year.month.day.array)
    hour.table <- table(hour.array)
    return(list(numcomp=numcomp, module.table=module.table, keywords.table=keywords.table, year.table=year.table, month.table=month.table, day.table=day.table, year.month.table=year.month.table, year.month.day.table=year.month.day.table, hour.table=hour.table))
  }
}


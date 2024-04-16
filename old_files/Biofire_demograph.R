library (sqldf)


df <- biofire_rect
sqldf('select mrn, stool_date from df where age is NULL and mrn is not null order by mrn desc')

sqldf('select mrn from df where age < 19')
sqldf('select * from df where mrn = "100389694"')


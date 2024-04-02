example_startq = "1977 Q2"
example_endq = "2017 Q2"
example_startyq = as.yearqtr(example_startq)
example_endyq = as.yearqtr(example_endq)

example_fhorizon = as.numeric("4")

start_y = as.numeric(year(example_startyq))
end_y = as.numeric(year(example_endyq))

start_q = as.numeric(quarter(example_startyq))
end_q = as.numeric(quarter(example_endyq))
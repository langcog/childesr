connect_to_childes <- function() {
  DBI::dbConnect(RMySQL::MySQL(),
                 host = "ec2-54-68-171-132.us-west-2.compute.amazonaws.com",
                 dbname = "childesdb",
                 user = "childesdb",
                 password = "uy5z4hf7ihBjf"
  )
}
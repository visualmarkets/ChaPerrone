# preferenceData <- lapply(seq_len(n), function(x) sample(seq_len(m), 3))
preferences <- function(student) preferenceData[[student]]

# the weight of a student choosing a course
# if the course is not among the preferences, the weight is -100000
weight <-
  function(student, course) {
    p <- which(as.numeric(course) == preferences(as.numeric(student)))
    as.integer(
      if(length(p) == 0){
        100000000
      } else {
        p
      }
    )
  }

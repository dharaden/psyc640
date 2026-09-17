# Simulates files/data/study_strategies.csv for PSYC 640 Lab 5
# 120 graduate students randomly assigned to a study strategy before a stats quiz.
set.seed(2026)
n <- 120
condition     <- sample(rep(c("Rereading", "Retrieval Practice"), each = n / 2))
study_minutes <- round(pmin(pmax(rnorm(n, 90, 25), 20), 180))
sleep_hours   <- round(pmin(pmax(rnorm(n, 6.8, 1.1), 4), 10), 1)
test_anxiety  <- round(pmin(pmax(rnorm(n, 30, 8), 10), 50))
exam_score <- 40 +
  7    * (condition == "Retrieval Practice") +
  0.18 * study_minutes +
  1.2  * sleep_hours -
  0.25 * test_anxiety +
  rnorm(n, 0, 8)
exam_score <- round(pmin(pmax(exam_score, 0), 100))
study_strategies <- data.frame(
  student_id = sprintf("S%03d", 1:n),
  condition, study_minutes, sleep_hours, test_anxiety, exam_score
)
write.csv(study_strategies, "study_strategies.csv", row.names = FALSE)

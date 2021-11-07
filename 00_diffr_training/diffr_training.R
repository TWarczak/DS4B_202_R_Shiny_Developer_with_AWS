install.packages("diffr")

library(diffr)

diffr::diffr(
    file1 = "app.R",
    file2 = "app_with_diffs.R"
)


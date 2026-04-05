library(pwr)
library(here)

source(here("analysis/stats_helpers.R"))
set_stats_file("sensitivity")

# Sensitivity Analysis: Minimum Detectable Cohen's d
# For within-subject pairwise contrasts (Studies 1-3), the effective test is
# a paired comparison with N = number of subjects. We use pwr.t.test() with
# type = "paired" to find the minimum detectable d at 80% power, alpha = 0.05.

cat("=== Sensitivity Analyses: Minimum Detectable Cohen's d ===\n\n")

# Study 1: N=59
cat("--- Study 1 (N=59) ---\n")
sens_1 <- pwr.t.test(n = 59, power = 0.80, sig.level = 0.05, type = "paired")
cat(sprintf("  Minimum detectable d = %.2f\n\n", sens_1$d))
write_stat("sensStudyOneMinD", sens_1$d, digits = 2)

# Study 2: N=59
cat("--- Study 2 (N=59) ---\n")
sens_2 <- pwr.t.test(n = 59, power = 0.80, sig.level = 0.05, type = "paired")
cat(sprintf("  Minimum detectable d = %.2f\n\n", sens_2$d))
write_stat("sensStudyTwoMinD", sens_2$d, digits = 2)

# Study 3: N=57
cat("--- Study 3 (N=57) ---\n")
sens_3 <- pwr.t.test(n = 57, power = 0.80, sig.level = 0.05, type = "paired")
cat(sprintf("  Minimum detectable d = %.2f\n\n", sens_3$d))
write_stat("sensStudyThreeMinD", sens_3$d, digits = 2)

# Studies 4-6: Logistic mixed models with binary outcomes.
# As a simple approximation, we treat each subject's proportion of precedent
# choices per condition as a continuous outcome and compute the minimum
# detectable d for a paired comparison. We then convert to an approximate
# odds ratio using OR = exp(d * pi / sqrt(3)).
# This is an approximation because it ignores the mixed-model structure and
# the binary nature of individual trials, but provides a rough sense of the
# minimum effect size detectable with these sample sizes.

cat("--- Study 4 (N=113) ---\n")
sens_4 <- pwr.t.test(n = 113, power = 0.80, sig.level = 0.05, type = "paired")
sens_4_or <- exp(sens_4$d * pi / sqrt(3))
cat(sprintf("  Minimum detectable d = %.2f (approximate OR = %.2f)\n\n", sens_4$d, sens_4_or))
write_stat("sensStudyFourMinD", sens_4$d, digits = 2)
write_stat("sensStudyFourMinOR", sens_4_or, digits = 2)

cat("--- Study 5 (N=153) ---\n")
sens_5 <- pwr.t.test(n = 153, power = 0.80, sig.level = 0.05, type = "paired")
sens_5_or <- exp(sens_5$d * pi / sqrt(3))
cat(sprintf("  Minimum detectable d = %.2f (approximate OR = %.2f)\n\n", sens_5$d, sens_5_or))
write_stat("sensStudyFiveMinD", sens_5$d, digits = 2)
write_stat("sensStudyFiveMinOR", sens_5_or, digits = 2)

cat("--- Study 6 (N=158) ---\n")
sens_6 <- pwr.t.test(n = 158, power = 0.80, sig.level = 0.05, type = "paired")
sens_6_or <- exp(sens_6$d * pi / sqrt(3))
cat(sprintf("  Minimum detectable d = %.2f (approximate OR = %.2f)\n\n", sens_6$d, sens_6_or))
write_stat("sensStudySixMinD", sens_6$d, digits = 2)
write_stat("sensStudySixMinOR", sens_6_or, digits = 2)

cat("=== Note ===\n")
cat("Studies 1-3: Minimum detectable Cohen's d for within-subject pairwise\n")
cat("contrasts (paired t-test equivalent) at 80% power, alpha = 0.05.\n")
cat("Studies 4-6: Approximate minimum detectable d and OR, treating subject-level\n")
cat("proportions as continuous. OR conversion: OR = exp(d * pi / sqrt(3)).\n")
cat("Conventional benchmarks for d: small = 0.20, medium = 0.50, large = 0.80.\n")

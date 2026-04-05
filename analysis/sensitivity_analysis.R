library(pwr)
library(here)

source(here("analysis/stats_helpers.R"))
set_stats_file("sensitivity")

# Sensitivity Analysis: Minimum Detectable Effect Sizes
# Using pwr.f2.test() with achieved sample sizes, power = 0.80, alpha = 0.05

cat("=== Sensitivity Analyses: Minimum Detectable Effect Sizes ===\n\n")

# Studies 1-4: Likert scale ratings with mixed models
# Approximate denominator df = N * trials_per_condition - number_of_parameters
# Studies 1-3: 3 (next_interaction) x 3 (relationship) = 9 conditions, 6 stories each
# Study 4: different design

# Study 1: N=59, 6 stories x 3 relationships x 3 next_interactions = 54 obs per subject
# Main effect next_interaction: NumDF=2, DenDF ~ 3095
# Approx denominator df: 59 * 54 - 10 = 3176
cat("--- Study 1 (N=59) ---\n")
cat("Main effects (2 df numerator, ~3176 denominator df):\n")
sens_1 <- pwr.f2.test(u = 2, v = 3176, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_1$f2, sqrt(sens_1$f2)))
write_stat("sensStudyOneMainFsq", sens_1$f2, digits = 4)
cat("Interaction (4 df numerator):\n")
sens_1i <- pwr.f2.test(u = 4, v = 3176, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_1i$f2, sqrt(sens_1i$f2)))
write_stat("sensStudyOneIntFsq", sens_1i$f2, digits = 4)

# Study 2: N=59, same design as Study 1
cat("--- Study 2 (N=59) ---\n")
cat("Main effects (2 df numerator, ~3090 denominator df):\n")
sens_2 <- pwr.f2.test(u = 2, v = 3090, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_2$f2, sqrt(sens_2$f2)))
write_stat("sensStudyTwoMainFsq", sens_2$f2, digits = 4)
cat("Interaction (4 df numerator):\n")
sens_2i <- pwr.f2.test(u = 4, v = 3090, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_2i$f2, sqrt(sens_2i$f2)))
write_stat("sensStudyTwoIntFsq", sens_2i$f2, digits = 4)

# Study 3: N=57, same design
cat("--- Study 3 (N=57) ---\n")
cat("Main effects (2 df numerator, ~2982 denominator df):\n")
sens_3 <- pwr.f2.test(u = 2, v = 2982, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_3$f2, sqrt(sens_3$f2)))
write_stat("sensStudyThreeMainFsq", sens_3$f2, digits = 4)
cat("Interaction (4 df numerator):\n")
sens_3i <- pwr.f2.test(u = 4, v = 2982, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_3i$f2, sqrt(sens_3i$f2)))
write_stat("sensStudyThreeIntFsq", sens_3i$f2, digits = 4)

# Study 4: N=113, 18 stories x 2 (Higher/Lower) = 36 obs per subject for main analysis
# glmer with first_actual (2 df), ~1350 residual df
cat("--- Study 4 (N=113) ---\n")
cat("Main effect of first_actual (2 df numerator, ~1350 denominator df):\n")
sens_4 <- pwr.f2.test(u = 2, v = 1350, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_4$f2, sqrt(sens_4$f2)))
write_stat("sensStudyFourMainFsq", sens_4$f2, digits = 4)
cat("Interaction first_response x first_actual (1 df numerator, ~1350 denominator df):\n")
sens_4i <- pwr.f2.test(u = 1, v = 1350, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_4i$f2, sqrt(sens_4i$f2)))
write_stat("sensStudyFourIntFsq", sens_4i$f2, digits = 4)

# Study 5: N=153, 6 scenarios x 3 partner_status x 2 coordination = 36 obs per subject
# Main model: strategy_repeating ~ partner_status * coordination
# Approx residual df: 153 * 6 - 10 = 908
cat("--- Study 5 (N=153) ---\n")
cat("Main effects (2 df numerator, ~908 denominator df):\n")
sens_5 <- pwr.f2.test(u = 2, v = 908, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_5$f2, sqrt(sens_5$f2)))
write_stat("sensStudyFiveMainFsq", sens_5$f2, digits = 4)
cat("Interaction (2 df numerator):\n")
sens_5i <- pwr.f2.test(u = 2, v = 908, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_5i$f2, sqrt(sens_5i$f2)))
write_stat("sensStudyFiveIntFsq", sens_5i$f2, digits = 4)

# Study 6: N=158, same design as Study 5
# Approx residual df: 158 * 6 - 10 = 938
cat("--- Study 6 (N=158) ---\n")
cat("Main effects (2 df numerator, ~938 denominator df):\n")
sens_6 <- pwr.f2.test(u = 2, v = 938, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n", sens_6$f2, sqrt(sens_6$f2)))
write_stat("sensStudySixMainFsq", sens_6$f2, digits = 4)
cat("Interaction (2 df numerator):\n")
sens_6i <- pwr.f2.test(u = 2, v = 938, power = 0.80, sig.level = 0.05)
cat(sprintf("  Minimum detectable f2 = %.4f (f = %.4f)\n\n", sens_6i$f2, sqrt(sens_6i$f2)))
write_stat("sensStudySixIntFsq", sens_6i$f2, digits = 4)

cat("=== Note ===\n")
cat("f2 is Cohen's f-squared effect size.\n")
cat("Conventional benchmarks: small = 0.02, medium = 0.15, large = 0.35\n")
cat("These are approximate since mixed models have complex df structures.\n")

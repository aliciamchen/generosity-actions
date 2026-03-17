library(ggplot2)

# Color palettes (colorblind-safe with luminance contrast)
action_colors <- c("#ECE133", "#D55E00", "#D6D6D6")
relationship_colors <- c("Higher" = "#CC78BC", "Lower" = "#56B4E9")

# Give/receive reuses action colors
give_receive_colors <- c("Receive" = "#D55E00", "Give" = "#ECE133")

# Global theme
theme_set(
  theme_classic(base_size = 15, base_family = "Arial Nova") +
    theme(strip.background = element_blank())
)

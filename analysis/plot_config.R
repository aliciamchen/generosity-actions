library(ggplot2)

# Color palettes (colorblind-safe with luminance contrast)
action_colors <- c("#4D7D4C", "#E09565", "#D6D6D6")
relationship_colors <- c("Higher" = "#993358", "Lower" = "#5B96B8")

# Give/receive reuses action colors
give_receive_colors <- c("Receive" = "#E09565", "Give" = "#4D7D4C")

# Global theme
theme_set(
  theme_classic(base_size = 15, base_family = "Arial") +
    theme(strip.background = element_blank())
)

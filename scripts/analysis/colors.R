
# COLORS ------------------------------------------------------------------

# LIGHTS
#fff98c - yellow
#ffe6a8 - orange
#e69ed1 - pink
#e7c7ff - purple
#cde1fa - blue
#d7fcfa - cyan
#eefda6 - green

# BRIGHTS

#ffef17 - yellow
#e037b0 - pink
#8b61e8 - purple
#0979b3 - teal
#5e94d6 - blue
#f78a31 - orange
#6e8011 - green

# for T/F values
tf_colors <- c("TRUE" = "#e69ed1",
               "FALSE" = "#0979b3")

# Mass Observation: pink!
scale_fill_gradient2(low = "white",
                     high = "#8F0B6A",
                     midpoint = 25,
                     mid = "#FFD1F2")

# UKWA: Blue!
scale_fill_gradient(low = "#cde1fa",
                    high = "#0979b3")

# Archive-It: orange/yellow!
scale_fill_gradient(low = "#fff98c",
                    high = "#f78a31")

# JOTPY: green!

scale_fill_gradient(low = "#eefda6",
                    high = "#6e8011")


# Disability: Purple! 
scale_fill_gradient(low = "#EDDFF7",
                    high = "#5B0899")

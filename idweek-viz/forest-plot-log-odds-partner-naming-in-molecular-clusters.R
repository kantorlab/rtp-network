library(ggplot2)

# Sample data
df <- data.frame(
  category = c("Gender: Male", "Gender: Non-Binary/Not Reported", "Race: Asian", "Race: Other", "Race: White", "Race: Not Reported", "Sequence: not B", "Behavior: MSM", "Behavior: HRH", "Risk: PWID", "Age at HIV Diagnosis"),
  log_odds = c(0.5195, 0.3712, -0.7893, 0.3093, 0.4604, 0.5280, -0.91371, 0.39270, -0.53185, 0.99007, -0.007950),
  std_error = c(0.1693, 0.6443, 0.4223, 0.3342, 0.1552, 0.7014, 0.20528, 0.13964, 0.14633, 0.42209, 0.005376),
  p_value = c(0.00214, 0.56453, 0.06162, 0.35464, 0.00301, 0.45160, 8.54e-06, 0.00492, 0.000278, 0.019, 0.139)
)

# Calculate confidence intervals
df$lower_ci <- df$log_odds - 1.96 * df$std_error
df$upper_ci <- df$log_odds + 1.96 * df$std_error

# Determine if CI crosses 0 (non-significant) or not (significant)
df$significant <- ifelse(df$lower_ci * df$upper_ci <= 0, "No", "Yes")

ggplot(df, aes(x=log_odds, y=category)) + 
  geom_vline(aes(xintercept=0), linetype="dashed", color="grey") +
  
  # Point for log odds colored by significance
  geom_point(aes(color=significant), size=3) +  
  
  # Error bars colored by significance
  geom_errorbarh(aes(xmin=lower_ci, xmax=upper_ci, color=significant), height=0.2) +
  
  scale_color_manual(values=c("No"="black", "Yes"="red"), 
                     name="Significance",
                     breaks=c("Yes", "No"),
                     labels=c("Significant", "Not Significant")) +
  
  labs(title="", x="Log Odds", y="") +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.text.y=element_text(size=18, face="bold", color="blue"),   # for y-axis categories
        axis.title.x=element_text(size=16, face="bold", color="black"), # for x-axis label "Log Odds"
        legend.title=element_text(size=16, face="bold", color="black"), # for legend title "Significance"
        legend.text=element_text(size=16, face="bold", color="black"))  # for legend items "Significant"/"Not Significant"

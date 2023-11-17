rm(list=ls())

library(ggplot2)


# Sample data 
df <- data.frame(
  category = c("Gender: Male  (n=477)", 
               #"Gender: Non-binary (n=7)",
               "Race: Asian (n=10)", 
               #"Race: Other (n=30)", 
               "Race: White (n=386)", 
               "Sequence: non-B subtype (n=51)",
               "Behavior: MSM (n=333)", 
               "Behavior: HRH (n=165)", 
               "Behavior: PWID (n=33)" 
               #"Age at HIV Diagnosis (n=583)"
               ),
  
  log_odds = c(-0.2188, 
               #-0.2446, 
               1.18474, 
               #-0.42470, 
               0.09366, 
               -0.29668, 
               -0.20453, 
               0.16108, 
               0.40669
               #0.006576
               ),
  
  std_error = c(0.1401, 
                #0.4390, 
                0.55928, 
                #0.33584, 
                0.13478, 
                0.22138, 
                0.11835, 
                0.12797, 
                0.19599
                #0.004724
                )
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
  geom_errorbarh(aes(xmin=lower_ci, xmax=upper_ci, color=significant), 
                 height=0.2) +
  scale_color_manual(values=c("No"="black", "Yes"="red"), 
                     name="",
                     breaks=c("Yes", "No"),
                     labels=c("Significant", "Not Significant")) +
  
  labs(title="", x="Log Odds", y="") +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.text.y=element_text(size=32, face="bold", color="blue"),   # for y-axis categories
        axis.title.x=element_text(size=20, face="bold", color="black"), # for x-axis label "Log Odds"
        axis.text.x=element_text(size=16, face="bold", color="black"), #for x-axis ticks
        legend.title=element_blank(), # for legend title "Significance"
        legend.text=element_text(size=28, face="bold", color="black"))  # for legend items "Significant"/"Not Significant"

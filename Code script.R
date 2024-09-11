# Jurycode
# Table1
x <- read.csv("CleanedJuryData.csv", as.is = TRUE)
# x25 contains all ages greater than 25
x25 <- x[x$age >= 25,]
venire <- x25
jury <- x25[x25$code == "J", ]
n1 <- nrow(venire)
n2 <- nrow(jury)
# t/f means table or figure, the first number is the number of table/figure,
#  the last two number is the position in the table
t111 <- sum(venire$edu < 12) / n1
t112 <- sum(jury$edu < 12) / n2
t121 <- sum(venire$edu == 12) / n1
t122 <- sum(jury$edu == 12)/ n2
t131 <- sum(venire$edu >12 & venire$edu < 16) /n1
t132 <- sum(jury$edu >12 & jury$edu < 16) / n2
t141 <- sum(venire$edu >= 16) / n1
t142 <- sum(jury$edu >= 16) /n2
t151 <- sum(venire$edu >= 12) / n1
t152 <- sum(jury$edu >= 12) / n2
t161 <- t141
t162 <- t142

# Figure 1
library(ggplot2)
library(cowplot)


calculate_percentage <- function(edu_counts) {
  total_count <- sum(edu_counts)
  return(edu_counts / total_count)
}


edu_counts1 <- table(x$edu)
edu_percentage1 <- calculate_percentage(edu_counts1)

edu_counts2 <- table(x[x$code == "J",]$edu)
edu_percentage2 <- calculate_percentage(edu_counts2)

ggplot1 <- ggplot(data = data.frame(edu_counts1, edu_percentage1), 
                  aes(x = as.numeric(names(edu_counts1)), y = edu_percentage1)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts1)), 
  labels = names(edu_counts1)) +xlim(0, max(as.numeric(names(edu_counts1)))) +
  theme_minimal()

ggplot2 <- ggplot(data = data.frame(edu_counts2, edu_percentage2), 
  aes(x = as.numeric(names(edu_counts2)), y = edu_percentage2)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts2)), 
  labels = names(edu_counts2)) + xlim(0, max(as.numeric(names(edu_counts1)))) +
  theme_minimal()

combined_plots <- plot_grid(ggplot1, ggplot2, ncol = 1)

print(combined_plots)

# Figure 2

criminal_data <- x[x$type == "criminal", ]
cri_ju_data <- x[x$code == "CP" & x$type == "criminal", ]
cri_qua_data <- x[x$jcp == "NCP" & x$type == "criminal", ]


calculate_percentage <- function(edu_counts) {
  total_count <- sum(edu_counts)
  return(edu_counts / total_count)
}

edu_counts <- table(criminal_data$edu)
edu_percentage <- calculate_percentage(edu_counts)

edu_counts1 <- table(cri_ju_data$edu)

edu_percentage1 <- calculate_percentage(edu_counts1)

edu_counts2 <- table(cri_qua_data$edu)
edu_percentage2 <- calculate_percentage(edu_counts2)

ggplot1 <- ggplot(data = data.frame(edu_counts, edu_percentage), 
                  aes(x = as.numeric(names(edu_counts)), y = edu_percentage)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Venires", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts)), 
  labels = names(edu_counts)) + xlim(0, max(as.numeric(names(edu_counts)))) +
  theme_minimal()

ggplot2 <- ggplot(data = data.frame(edu_counts1, edu_percentage1), 
  aes(x = as.numeric(names(edu_counts1)), y = edu_percentage1)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Excused by Judge", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts1)), 
  labels = names(edu_counts1)) + xlim(0, max(as.numeric(names(edu_counts1)))) +
  theme_minimal()

ggplot3 <- ggplot(data = data.frame(edu_counts2, edu_percentage2), 
  aes(x = as.numeric(names(edu_counts2)), y = edu_percentage2)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Qualified Panels", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts2)), 
  labels = names(edu_counts2)) + xlim(0, max(as.numeric(names(edu_counts2)))) +
  theme_minimal()

combined_plots <- plot_grid(ggplot1, ggplot2, ggplot3, ncol = 1)

print(combined_plots)



# Figure 3

civil_data <- x[x$type == "civil", ]
civ_ju_data <- x[x$code == "CP" & x$type == "civil", ]
civ_qua_data <- x[x$jcp == "NCP" & x$type == "civil", ]

calculate_percentage <- function(edu_counts) {
  total_count <- sum(edu_counts)
  return(edu_counts / total_count)
}

edu_counts <- table(civil_data$edu)
edu_percentage <- calculate_percentage(edu_counts)

edu_counts1 <- table(civ_ju_data$edu)
edu_percentage1 <- calculate_percentage(edu_counts1)

edu_counts2 <- table(civ_qua_data$edu)
edu_percentage2 <- calculate_percentage(edu_counts2)

ggplot1 <- ggplot(data = data.frame(edu_counts, edu_percentage), 
  aes(x = as.numeric(names(edu_counts)), y = edu_percentage)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Venires", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts)), 
  labels = names(edu_counts)) + xlim(0, max(as.numeric(names(edu_counts)))) +
  theme_minimal()

ggplot2 <- ggplot(data = data.frame(edu_counts1, edu_percentage1), 
  aes(x = as.numeric(names(edu_counts1)), y = edu_percentage1)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Excused by Judge", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts1)), 
  labels = names(edu_counts1)) + xlim(0, max(as.numeric(names(edu_counts1)))) +
  theme_minimal()

ggplot3 <- ggplot(data = data.frame(edu_counts2, edu_percentage2), 
  aes(x = as.numeric(names(edu_counts2)), y = edu_percentage2)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Qualified Panels", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts2)), 
  labels = names(edu_counts2)) + xlim(0, max(as.numeric(names(edu_counts2)))) +
  theme_minimal()

combined_plots <- plot_grid(ggplot1, ggplot2, ggplot3, ncol = 1)

print(combined_plots)

# Figure 4

cri_data <- x[x$type == "criminal" & x$jcp == "NCP", ]
cri_pp_data <- x[x$code == "PP" & x$type == "criminal", ]
cri_pd_data <- x[x$code == "PD" & x$type == "criminal", ]
cri_jury_data <- x[x$code == "J" & x$type == "criminal", ]

calculate_percentage <- function(edu_counts) {
  total_count <- sum(edu_counts)
  return(edu_counts / total_count)
}

edu_counts <- table(cri_data$edu)
edu_percentage <- calculate_percentage(edu_counts)

edu_counts1 <- table(cri_pp_data$edu)
edu_percentage1 <- calculate_percentage(edu_counts1)

edu_counts2 <- table(cri_pd_data$edu)
edu_percentage2 <- calculate_percentage(edu_counts2)

edu_counts3 <- table(cri_jury_data$edu)
edu_percentage3 <- calculate_percentage(edu_counts3)

ggplot1 <- ggplot(data = data.frame(edu_counts, edu_percentage), 
                  aes(x = as.numeric(names(edu_counts)), y = edu_percentage)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Qualified Panels", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts)), 
  labels = names(edu_counts)) +xlim(0, 25) + theme_minimal()

ggplot2 <- ggplot(data = data.frame(edu_counts1, edu_percentage1), 
                  aes(x = as.numeric(names(edu_counts1)), y = edu_percentage1)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Prosecution Challenges", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts1)), 
                     labels = names(edu_counts1)) + xlim(0, 25) +
  theme_minimal()

ggplot3 <- ggplot(data = data.frame(edu_counts2, edu_percentage2), 
                  aes(x = as.numeric(names(edu_counts2)), y = edu_percentage2)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Defense Challenges", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts2)), 
                     labels = names(edu_counts2)) + xlim(0, 25) +
  theme_minimal()

ggplot4 <- ggplot(data = data.frame(edu_counts3, edu_percentage3), 
                  aes(x = as.numeric(names(edu_counts3)), y = edu_percentage3)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Criminal Juries", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts3)), 
                     labels = names(edu_counts3)) + xlim(0, 25) +
  theme_minimal()

combined_plots <- plot_grid(ggplot1, ggplot2, ggplot3, ggplot4, ncol = 1)

print(combined_plots)

# Figure 5

civ_data <- x[x$type == "civil" & x$jcp == "NCP", ]
civ_pp_data <- x[x$code == "PP" & x$type == "civil", ]
civ_pd_data <- x[x$code == "PD" & x$type == "civil", ]
civ_jury_data <- x[x$code == "J" & x$type == "civil", ]

calculate_percentage <- function(edu_counts) {
  total_count <- sum(edu_counts)
  return(edu_counts / total_count)
}

edu_counts <- table(civ_data$edu)
edu_percentage <- calculate_percentage(edu_counts)

edu_counts1 <- table(civ_pp_data$edu)
edu_percentage1 <- calculate_percentage(edu_counts1)

edu_counts2 <- table(civ_pd_data$edu)
edu_percentage2 <- calculate_percentage(edu_counts2)

edu_counts3 <- table(civ_jury_data$edu)
edu_percentage3 <- calculate_percentage(edu_counts3)

ggplot1 <- ggplot(data = data.frame(edu_counts, edu_percentage), 
                  aes(x = as.numeric(names(edu_counts)), y = edu_percentage)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Qualified Panels", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts)), 
  labels = names(edu_counts)) + xlim(0, 25) + theme_minimal()

ggplot2 <- ggplot(data = data.frame(edu_counts1, edu_percentage1), 
                  aes(x = as.numeric(names(edu_counts1)), y = edu_percentage1)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Prosecution Challenges", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts1)), 
                     labels = names(edu_counts1)) + xlim(0, 25) +
  theme_minimal()

ggplot3 <- ggplot(data = data.frame(edu_counts2, edu_percentage2), 
                  aes(x = as.numeric(names(edu_counts2)), y = edu_percentage2)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Defense Challenges", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts2)), 
                     labels = names(edu_counts2)) + xlim(0, 25) +
  theme_minimal()

ggplot4 <- ggplot(data = data.frame(edu_counts3, edu_percentage3), 
                  aes(x = as.numeric(names(edu_counts3)), y = edu_percentage3)) +
  geom_bar(stat = "identity") +
  geom_line(color = "red", size = 1) +
  labs(x = "Years of Education, Civil Juries", y = "Density") +
  scale_x_continuous(breaks = as.numeric(names(edu_counts3)), 
                     labels = names(edu_counts3)) + xlim(0, 25) +
  theme_minimal()

combined_plots <- plot_grid(ggplot1, ggplot2, ggplot3, ggplot4, ncol = 1)

print(combined_plots)

# Table 2
Qualifiedpanel <- x25[x25$jcp == "NCP", ]
n2 <- nrow(Qualifiedpanel)
t111 <- sum(venire$edu < 12) / n1
t112 <- sum(Qualifiedpanel$edu < 12) / n2
t121 <- sum(venire$edu == 12) / n1
t122 <- sum(Qualifiedpanel$edu == 12)/ n2
t131 <- sum(venire$edu >12 & venire$edu < 16) /n1
t132 <- sum(Qualifiedpanel$edu >12 & Qualifiedpanel$edu < 16) / n2
t141 <- sum(venire$edu >= 16) / n1
t142 <- sum(Qualifiedpanel$edu >= 16) /n2
t151 <- sum(venire$edu >= 12) / n1
t152 <- sum(Qualifiedpanel$edu >= 12) / n2
t161 <- t141
t162 <- t142

# Table C-2
number <- as.numeric(names(table(x$CleanedJuryData.csv)))
p_values <- vector("numeric", length(number))

for (i in 1:length(number)) {
  c1 <- number[i]
  t_test_result <- t.test(x$edu[(x$code == "PD" | x$code == "PPPD") 
                                & x$CleanedJuryData.csv == c1],
                          x$edu[(x$code == "PP" | x$code == "PPPD") 
                                & x$CleanedJuryData.csv == c1],
                          alternative = "two.sided", var.equal = TRUE)
  p_values[i] <- t_test_result$p.value
}
print(p_values)
p_values1 <- vector("numeric", length(number))
for (i in 1:length(number)) {
  c1 <- number[i]
  t_test_result1 <- t.test(x$edu[(x$code == "PD" | x$code == "PP" | x$code == 
                                    "PPPD") & x$CleanedJuryData.csv == c1],
                           x$edu[x$code == "J" & x$CleanedJuryData.csv == c1]
                           , alternative = "greater", var.equal = TRUE)
  p_values1[i] <- t_test_result1$p.value
}
print(p_values1)

################################################################################
# Diving code
x <- read.csv("Diving2000.csv", as.is = TRUE)
x$DiveNumber <- rep(1:1541, each = 7)
x$avg <- rep(tapply(x$JScore, x$DiveNumber, mean), each = 7)
x$match <- (x$Country == x$JCountry)
table(x$match)
x$Discrepancy = (x$JScore - x$avg)

# Table 1
x[x$Diver == "XIONG Ni" & x$JCountry == "CHN" & x$Round == "Semi", ]
x[x$Diver == "XIONG Ni" & x$Round == "Semi" & x$DiveNo <=3, ]
x$JScore[x$Diver == "XIONG Ni" & x$Round == "Semi"]
for (i in 1:3) {
  cat(mean(x$JScore[x$Diver == "XIONG Ni" & x$Round == "Semi" & x$DiveNo == i], 
           trim = 0.142858), "\n")
}

# Table 2
x$DiveNumber <- rep(1:1541, each = 7)
x$avg <- rep(tapply(x$JScore, x$DiveNumber, mean), each = 7)
x$Discrepancy = (x$JScore - x$avg)
cat(untrimmedmean <- mean(x$JScore[x$Diver == "XIONG Ni" & x$Round == "Semi"
& x$DiveNo == 1]))
table2Name <- x$JCountry[x$Diver == "XIONG Ni" & x$Round == "Semi" & x$DiveNo == 1]
for (i in table2Name) {
  cat(x$Discrepancy[x$Diver == "XIONG Ni" & x$Round == "Semi" & x$DiveNo == 1 &
  x$JCountry == i], "\n")
}

# Table 3

y <- x[x$Judge =="McFARLAND Steve", ]

Nsim <- 100000
DOAD <- rep(-1, Nsim)

for (i in 1:Nsim) {
  y$SimOutcome <- sample(y$Country)
  y$match1 <- (y$SimOutcome == "USA")
  DOAD[i] <- (mean(y$Discrepancy[y$match1]) - mean(y$Discrepancy[!y$match1]))
}
hist(DOAD)
sum(DOAD > 0.19) / Nsim

y_mcfarland <- x[x$Judge == "McFARLAND Steve", ]
y_wang <- x[x$Judge == "WANG Facheng", ]
DOADMc <- mean(y_mcfarland$Discrepancy[y_mcfarland$match]) - 
  mean(y_mcfarland$Discrepancy[!y_mcfarland$match])
DOADWang <- mean(y_wang$Discrepancy[y_wang$match]) - 
  mean(y_wang$Discrepancy[!y_wang$match])
# Figure 1
Nsim <- 100000
DOAD_mcfarland <- rep(-1, Nsim)


for (i in 1:Nsim) {
  y_mcfarland$SimOutcome <- sample(y_mcfarland$Country)
  y_mcfarland$match1 <- (y_mcfarland$SimOutcome == "USA")
  DOAD_mcfarland[i] <- (mean(y_mcfarland$Discrepancy[y_mcfarland$match1]) -
                          mean(y_mcfarland$Discrepancy[!y_mcfarland$match1]))
}

density_DOAD_mcfarland <- density(DOAD_mcfarland)

# Figure 2
Nsim <- 100000
DOAD_wang <- rep(-1, Nsim)

for (i in 1:Nsim) {
  y_wang$SimOutcome <- sample(y_wang$Country)
  y_wang$match1 <- (y_wang$SimOutcome == "CHN")
  DOAD_wang[i] <- (mean(y_wang$Discrepancy[y_wang$match1]) - 
                     mean(y_wang$Discrepancy[!y_wang$match1]))
}

density_DOAD_wang <- density(DOAD_wang)

par(mfrow = c(2, 1))

ylim_upper <- max(density_DOAD_mcfarland$y, density_DOAD_wang$y)
ylim_lower <- min(density_DOAD_mcfarland$y, density_DOAD_wang$y)

plot(density_DOAD_mcfarland, main = "DOAD Distribution for McFARLAND Steve", 
     xlab = "DOAD", ylab = "Density", ylim = c(ylim_lower, ylim_upper))
abline(v = DOADMc, col = "red", lwd = 5)

plot(density_DOAD_wang, main = "DOAD Distribution for WANG Facheng", 
     xlab = "DOAD", ylab = "Density", ylim = c(ylim_lower, ylim_upper))
abline(v = DOADWang, col = "green", lwd = 5)

# Table 4
table4names <- unique(x$Judge[x$match])
Nsim <- 100000
DOAD <- rep(-1, Nsim)
for (n in table4names) {
  y <- x[x$Judge ==n, ]
  JCountryontrack <- unique(x$JCountry[x$Judge == n])
  y$match0 <- (y$Country == JCountryontrack)
  cat("DoAD of", n, "=", truedoad <- (round((mean(y$Discrepancy[y$match0]) - 
                                    mean(y$Discrepancy[!y$match0])), 2)), "\n")
  for (i in 1:Nsim) {
    y$SimOutcome <- sample(y$Country)
    y$match1 <- (y$SimOutcome == JCountryontrack)
    DOAD[i] <- (mean(y$Discrepancy[y$match1]) - mean(y$Discrepancy[!y$match1]))
  }
  cat("p-val of", n, "=", (sum(DOAD >= truedoad) / Nsim), "\n")
}

results <- data.frame(Judge = character(), t_test_p_value = numeric(), stringsAsFactors = FALSE)
for (n in table4names) {
  y <- x[x$Judge == n, ]
  JCountryontrack <- unique(x$JCountry[x$Judge == n])
  y$match0 <- (y$Country == JCountryontrack)
  
  # Perform the t-test and calculate the p-value
  t_test_p_val <- t.test(y$Discrepancy[y$match0], y$Discrepancy[!y$match0])$p.value
  
  # Store the results in the data frame
  results <- rbind(results, data.frame(Judge = n, t_test_p_value = t_test_p_val))
}

# View the results
print(results)









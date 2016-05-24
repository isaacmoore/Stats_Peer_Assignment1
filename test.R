# ---------- Part 1 ----------
# --- Number 1 ---
set.seed(1987)
lambda <- 0.2
sd <- 1/lambda
sims <- 1000
n <- 40
data <- NULL
for(i in 1 : sims){
data <- c(data, mean(rexp(n, lambda)))
}
theoretical_mean <- 1/lambda 
theoretical_mean
actual_mean <- mean(data)
actual_mean
hist(data, breaks = 'Sturges')
abline (v = theoretical_mean, col = 'red')
abline(v = actual_mean, col = 'green')
legend('topright', pch = 19, col = c('red', 'green'), legend = c('Theoretical Mean', 'Actual Mean'))
# --- Number 2 --- 
actual_std_dev <- sd(data)
actual_variance <- actual_std_dev^2
actual_variance
theoretical_std_dev <- (1/lambda)/sqrt(n)
theoretical_variance <- theoretical_std_dev^2
theoretical_variance
# --- Number 3 ---
xfit <- seq(min(data), max(data), length=1000)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda)/sqrt(n))
hist(data, breaks = 'Sturges', probability = T)
lines(xfit, yfit, type ='l', lwd = 1, col = 'red')
legend('topright', pch = 19, col = c('red'), legend = c('Normal Distribution'))
# ---------- Part 2 ----------
# --- Number 1 --- 
library('dplyr')
library('ggplot2')
data('ToothGrowth')
tooth_growth <- tbl_df(ToothGrowth)
tooth_growth
# --- Number 2 ---
oj <- tooth_growth %>% filter(tooth_growth$supp == 'OJ')
summary(oj$len)
vc <- tooth_growth %>% filter(tooth_growth$supp == 'VC')
summary(vc$len)
oj_dose_lens <- tooth_growth %>% filter(tooth_growth$supp == 'OJ') %>%
        group_by(dose) %>%
        summarise(oj_mean_len = mean(len))
oj_dose_lens
vc_dose_lens <- tooth_growth %>% filter(tooth_growth$supp == 'VC') %>%
        group_by(dose) %>%
        summarise(vc_mean_len = mean(len))
vc_dose_lens
plot <- ggplot(tooth_growth, aes(supp, len))+geom_point()
plot + facet_grid(.~dose)+
        labs(title='Supplements at each dose', y='Tooth Length',x='Supplement')
# --- Number 3 ---
t.test(oj$len, vc$len, paired = FALSE, var.equal = FALSE)
# t-test for dose 2.0
oj_2 <- filter(tooth_growth, dose == 2.0)
oj_2 <- filter(oj_2, supp == "OJ")
vc_2 <- filter(tooth_growth, dose == 2.0)
vc_2 <- filter(vc_2, supp == "VC")
t.test(oj_2$len, vc_2$len, paired = FALSE, var.equal = FALSE)
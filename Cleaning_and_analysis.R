
# load required packages 
library(dplyr)
library(car)
library(rstatix)
library(lmtest)
library(officer)
#---------------------------------------------------------- cleaning data for screeeing study----------------------------------------------------#

raw_df<-read.csv("data/screening.csv")

# removing practice atemmpts at compelating the questionare 

df<- raw_df[-c(2:16), ]

# selecting particpant ID's and PHQ-9 total scores 

df<- df %>% select(SC0, PROLIFIC_PID)

# removing the first row 

df<- df[-1, ]

# renaming collums 

df<- df %>% rename( PHQ_9 = SC0, 
                    ID =  PROLIFIC_PID)

# making PHQ_9 numeric 

df$PHQ_9<- as.numeric(df$PHQ_9)

# cheking for duplicates 

length(unique(df$ID)) == nrow(df)

# selecting  particpants with scores =< 10

df1<- filter(df, PHQ_9 >= 11)

# changing names of collums 
colnames(df1) [2] <- "PROLIFIC_PID"

# saving as .csv 
write.csv(df1, "data/eligible11.csv")

# At this stage participants who scored 11 and above were 
# contacted via prolific to take part in the next phase of the study.
# The subsequent code is run on data from those who responded to the invitation
# and completed the main experiment. 



#-------------------- cleaning data for main study --------------------------------------------------



# reading in data 

raw_df2<- read.csv("data/raw_data.csv")

# deleating collums 
# 
mdf2<- raw_df2[ , !names(raw_df2) %in%
                  c("StartDate", "EndDate", "Status", "Progress", "Duration..in.seconds.", "Finished", 
                    "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
                    "ExternalReference", "LocationLatitude", "LocationLongitude","DistributionChannel","UserLanguage" )]
mdf2<- mdf2[-c(1:42), ]

mdf2<- mdf2[, -c(1:9) ]

# selecting only attiudes collums 
ats<- mdf2 %>% 
  select(starts_with(c("ATSPPH","PROLIF")))

# renaming collums 

names(ats)[1] = "A1"
names(ats)[2] = "A2"
names(ats)[3] = "A3"
names(ats)[4] = "A4"
names(ats)[5] = "A5"
names(ats)[6] = "A6"
names(ats)[7] = "A7"
names(ats)[8] = "A8"
names(ats)[9] = "A9"
names(ats)[10] = "A10"

# replacing the values in df with the correct ones for the scale 


col_na_A<- c("A1","A2", "A3","A4", "A5", "A6", "A7", "A8", "A9","A10")

ats[col_na_A] <- lapply(ats[col_na_A], as.numeric)



# moving the scale down one opint 

ats[ats == 1]<- 0
ats[ats == 2]<- 1
ats[ats == 3]<- 2
ats[ats == 4]<- 3

# reverse scoring collums 2, 4, 8, 9, 10

rsA2<- max(ats$A2)- ats$A2
ats$A2 <- rsA2

rsA4<- max(ats$A4)- ats$A4
ats$A4 <- rsA4

rsA8<- max(ats$A8)- ats$A8
ats$A8 <- rsA8

rsA9<- max(ats$A9)- ats$A9
ats$A9 <- rsA9

rsA10<- max(ats$A10)- ats$A10
ats$A10 <- rsA10

# repeating the same prosess for stigma 

stig<- mdf2 %>% 
  select(starts_with(c("SSOSH","PROLIF")))

# renaming collums 

names(stig)[1] = "S1"
names(stig)[2] = "S2"
names(stig)[3] = "S3"
names(stig)[4] = "S4"
names(stig)[5] = "S5"
names(stig)[6] = "S6"
names(stig)[7] = "S7"
names(stig)[8] = "S8"
names(stig)[9] = "S9"
names(stig)[10] = "S10"

# replacing the values in df with the correct ones for the scale 


col_na_S<- c("S1","S2", "S3","S4", "S5", "S6", "S7", "S8", "S9","S10")

stig[col_na_S] <- lapply(stig[col_na_S], as.numeric)

# reverse scoring collums 2, 4, 5, 7, 9

rss2<- max(stig$S2) +1 - stig$S2
stig$S2 <- rss2

rss4<- max(stig$S4) +1 - stig$S4
stig$S4 <- rss4

rss5<- max(stig$S5) +1 - stig$S5
stig$S5 <- rss5

rss7<- max(stig$S7) +1 - stig$S7
stig$S7 <- rss7

rss9<- max(stig$S9) +1 - stig$S9
stig$S9 <- rss9

# creating total scores for both df 

ats$total_A <- rowSums(ats[col_na_A])
stig$total_S <- rowSums(stig[col_na_S])

ats2<- ats[, -c(1:10)]
stig2<- stig[, -c(1:10)]

# creating totals for MGSQ in mdf2 
# 
mdf2[, c(2:16)] <- lapply(mdf2[, c(2:16)], as.numeric)

mdf2$total_M <- rowSums(mdf2[, c(2:16)])

# adding atitude and stigma total score

mdf2<- left_join(mdf2, ats2, join_by("PROLIFIC_PID"))

mdf2<- left_join(mdf2, stig2, join_by("PROLIFIC_PID"))

# removing collums i dont need 

mdf3<- mdf2 %>% 
  select(starts_with(c("questi","PROLIF","total", "FL_16_" )))

# changing the cell values to represent the condions 

colnames(mdf3)[8] <- "Condition"

mdf3$Condition[mdf3$Condition == "FL_41"] <- "PI"
mdf3$Condition[mdf3$Condition == "FL_40"] <- "brochure"
mdf3$Condition[mdf3$Condition == "FL_42"] <- "Cont"
mdf3$Condition[mdf3$Condition == "FL_25"] <- "PI+brochure"


# adding names to the comprehtion checks 

colnames(mdf3)[1] <- "compq1"
colnames(mdf3)[2] <- "compq2"
colnames(mdf3)[3] <- "compq3"


# creating a separate df for brochure and PI + brochure so i an exclude those that score 1 or less in comprehension checks 

bro_conds<- subset(mdf3, Condition %in% c("brochure","PI+brochure"))

# replacing the values in the comprehension check 

bro_conds$compq1[bro_conds$compq1 != 1] <- 0

bro_conds$compq2[bro_conds$compq2 != 2 ] <- 0
bro_conds$compq2[bro_conds$compq2 == 2 ] <- 1

bro_conds$compq3[bro_conds$compq3 != 3 ] <- 0
bro_conds$compq3[bro_conds$compq3 == 3 ] <- 1

bro_conds$compq1 <- as.numeric(bro_conds$compq1)
bro_conds$compq2 <- as.numeric(bro_conds$compq2)
bro_conds$compq3 <- as.numeric(bro_conds$compq3)


# creating a total score for comprehension. 

bro_conds<- bro_conds %>% 
  mutate(sum = rowSums(across(c(compq1, compq2, compq3))))

# creating df just for toalal scores and IDs. 

sum_comp <- bro_conds[ , c(4,9)]

# adding this to the main df 

mdf3<- mdf3 %>% left_join(sum_comp, by = join_by(PROLIFIC_PID))

# deleting the comprehestion check questions 

mdf3<- mdf3[ , -c(1:3)]

# renaming the comprehestion chack collum 

colnames(mdf3) [6] <- "Comp_check"

# finding out how many participants are in each condtion 

t1<- table(mdf3$Condition)
print(t1)

# removing thoes that score 1 or 0 

mdf3<- mdf3 %>% filter((Comp_check == 2 | Comp_check == 3| is.na(Comp_check)))

# finding out how many participants are in each condtion after removal 

t2<- table(mdf3$Condition)
print(t2)


# creating two columns for PI and brochure conditions and dummy coding 

mdf3$PI <- ifelse(mdf3$Condition %in% c("PI","PI+brochure" ), 1, 0)

mdf3$Brochure <- ifelse(mdf3$Condition %in% c("brochure","PI+brochure" ), 1, 0)


# adding the depression scores from the screeing 

mdf3<- left_join(mdf3, df1, join_by("PROLIFIC_PID"))

# -------------------  adding demographic data  to the main df-------------------------------

raw_dem<- read.csv("data/raw_dem.csv")

dem <- raw_dem[ , c("Participant.id", "Age", "Ethnicity.simplified", "Employment.status" )]

# changing collum names 

colnames(dem)[1] <- "PROLIFIC_PID"
colnames(dem)[3] <- "Ethnicity"

# joining this to the main df 

mdf3<- mdf3 %>% left_join(dem, by = join_by(PROLIFIC_PID))

#--------generating descriptive STATS ----------------------#


library(epiDisplay)

mean(mdf3$Age)
sd(mdf3$Age)

par(mar = c(2, 2, 2, 2))
tab1(mdf3$Ethnicity, sort.group = "decreasing", cum.percent = TRUE)

par(mar = c(2, 2, 2, 2))
tab1(mdf3$Employment.status, sort.group = "decreasing", cum.percent = TRUE)

detach("package:epiDisplay", unload = TRUE)

#----- renaming the oucmonme veribels 

colnames(mdf3)[2] <-"MGSQ"
colnames(mdf3)[3] <-"ATSPPH"
colnames(mdf3)[4] <-"SSOSH"

#--------------------------------- assumptions and outliers--------------------------------------------#
# gerating plots 

# histograms 
hist(mdf3$ATSPPH, main = "ats")
hist(mdf3$SSOSH, main = "stig")
hist(mdf3$MGSQ, main = "masc")
# box plots 

boxplot(mdf3$ATSPPH, main = "ats")
boxplot(mdf3$SSOSH, main = "stig")
boxplot(mdf3$MGSQ, main = "masc")

# qq plot 

qqnorm(mdf3$ATSPPH, main = "ats")
qqnorm(mdf3$SSOSH, main = "stig")
qqnorm(mdf3$MGSQ, main = "masc")

# calculating means and standard devations of the veriables 

mats = mean(mdf3$ATSPPH)
mstig = mean(mdf3$SSOSH)
mmasc = mean(mdf3$MGSQ)

sats = sd(mdf3$ATSPPH)
sstig = sd(mdf3$SSOSH)
smasc = sd(mdf3$MGSQ)

# CREATING A TABLE OF MEANS AND sdS for all dv's and groups 
means_sd<- mdf3 %>% 
  group_by(Condition) %>% 
  summarise(
    mean_ATSPPH = mean(ATSPPH), 
    sd_ATSPPH = sd(ATSPPH), 
    mean_SSOSH = mean(SSOSH), 
    sd_SSOSH = sd(SSOSH), 
    mean_MGSQ = mean(MGSQ),
    sd_MGSQ = sd(MGSQ), 
    mean_PHQ_9 = mean(PHQ_9), 
    sd_PHQ_9 = sd(PHQ_9)
  )



doc <- read_docx()

table_df <- data.frame(
  Condition = means_sd$Condition,
  mean_ATSPPH = means_sd$mean_ATSPPH,
  sd_ATSPPH = means_sd$sd_ATSPPH,
  mean_SSOSH = means_sd$mean_SSOSH,
  sd_SSOSH = means_sd$sd_SSOSH,
  mean_MGSQ = means_sd$mean_MGSQ,
  sd_MGSQ = means_sd$sd_MGSQ,
  mean_PHQ_9= means_sd$mean_PHQ_9,
  sd_PHQ_9= means_sd$sd_PHQ_9
)


doc <- body_add_table(doc, value = table_df, style = "Normal Table")
file_path <-'G:/My Drive/Msc/disertation/write up and anaysis/tab_and_fig.docx'
print(doc, target = file_path)


# getting threshold values for  outliers 

Tmin_at = mats-(3*sats)
Tmax_at = mats +(3*sats)

Tmin_st = mstig - (3*sstig)
Tmax_st = mstig + (3*sstig)

Tmin_ma = mmasc - (3*smasc)
Tmax_ma = mmasc + (3*smasc)

# finding outliers 

mdf3$ATSPPH[which(mdf3$ATSPPH < Tmin_at | mdf3$ATSPPH > Tmax_at )]

mdf3$SSOSH[which(mdf3$SSOSH < Tmin_st | mdf3$SSOSH > Tmax_st)]

mdf3$MGSQ[which(mdf3$MGSQ < Tmin_ma  | mdf3$MGSQ > Tmax_ma)]



# preforming assumption checks (ANOVA)
# normality 

cond<- list('PI', 'brochure', 'PI+brochure', 'Cont' )
mes<- list('MGSQ', 'ATSPPH', 'SSOSH')

for(mesure in mes) {
  cat("----- Shapiro-Wilk Tests for", mesure, "-----\n")
  for (i in cond) {
    masc_res<- mdf3 %>% 
      filter(Condition == i) %>% 
      pull(.data[[mesure]]) %>% 
      shapiro.test() 
    print(masc_res)
    cat("Condition:", i, "\n")
    cat("\n")
    flush.console()
  }
}


# homogenaity of verience 


lev_res <- mdf3 %>% levene_test(ATSPPH ~ Condition)
print(lev_res)

lev_res2<- mdf3 %>% levene_test(SSOSH ~ Condition)
print(lev_res2)


#-------------------------------randomization checks-----------------------------------#


dep.aov<- aov(PHQ_9 ~ PI * Brochure,data = mdf3)
summary(dep.aov)

masc.aov <- aov(MGSQ ~ PI * Brochure, data = mdf3)
summary(masc.aov)

masc.lm <- lm(MGSQ ~ PI * Brochure, data = mdf3)
summary(masc.lm)

# performing t-tests 
test123<- mdf3 %>% 
  t_test(MGSQ ~ Condition, paired = FALSE, detailed = TRUE)
print(test123)


###-------------------- Running the analysis ANOVA ---------------------------------------------
# ANOVA with attitudes as DV 

att.aov<- aov(ATSPPH ~ PI * Brochure, data = mdf3)
summary(att.aov)
# calculating etta sqr 
eta_squared(att.aov)
# checking nomality of residuals 

att.res<- residuals(att.aov)
qqnorm(att.res)
shapiro.test(att.res)

# ------------------------------preforming LOg transfomation 

mdf3$L_ATSPPH <-  log(mdf3$ATSPPH)

Latt.aov<- aov(L_ATSPPH ~ PI * Brochure, data = mdf3)
summary(Latt.aov)

# calculating etta sqr 
eta_squared(Latt.aov)


# checking nomality of residuals 

Latt.res<- residuals(Latt.aov)
qqnorm(Latt.res)
shapiro.test(Latt.res)
# ------------------------------preforming  squate root transfomation 

mdf3$S_ATSPPH <-  sqrt(mdf3$ATSPPH)

Satt.aov<- aov(S_ATSPPH ~ PI * Brochure, data = mdf3)
summary(Satt.aov)

# calculating etta sqr 
eta_squared(Satt.aov)


# checking nomality of residuals 

Satt.res<- residuals(Satt.aov)
qqnorm(Satt.res)
shapiro.test(Satt.res)

#here 

# --------------Anova with stigma as DV 
stig.aov<- aov(SSOSH ~ PI * Brochure, data = mdf3)
summary(stig.aov)

# calculating etta sqr 

eta_squared(stig.aov)

# checking nomality of residuals

stig.res<- residuals(stig.aov)
qqnorm(stig.res)
shapiro.test(stig.res)



# conducting t-tests for significant effect found by anova 

ttest_2<- mdf3 %>% 
  t_test(SSOSH ~ Condition, paired = FALSE, detailed = TRUE)
print(ttest_2)

#-------------------------- regression assumptions and aynalsis 

# linearity of MGSQ and the DVS 

plot(mdf3$MGSQ, mdf3$ATSPPH)
line_1<- lm(ATSPPH ~ MGSQ, data = mdf3 )
abline(line_1, col = "red")

plot(mdf3$MGSQ, mdf3$SSOSH_OR)
line_2<- lm(SSOSH ~ MGSQ, data = mdf3 )
abline(line_2, col = "red")


#running regression  with attitudes as DV 

att.reg<- lm(ATSPPH ~ PI + Brochure + + PI*Brochure + PI*Brochure*MGSQ, data = mdf3)
summary(att.reg)

# creating residuals
atrg_res<- residuals(att.reg)


# creating fitted values 

atrg_fit<- predict(att.reg)

# examining homoscedasticity
# Residual vs. Fitted Values Plot

plot(atrg_fit, atrg_res)
abline(h = 0, col = "red")

# employing Breusch-Pagan Test



bptest(atrg_res ~ PI + Brochure + + PI*Brochure + PI*Brochure*MGSQ, data = mdf3)

# testing for autocorelation 

acf(atrg_res, main = "Autocorrelation of Residuals")
dwtest(att.reg)

# normaility of residuals 

qqnorm(atrg_res)
shapiro.test(atrg_res)



#------running regression  with stigma as DV 

stig.reg<- lm(SSOSH ~ PI + Brochure +  PI*Brochure + PI*Brochure*MGSQ, data = mdf3)
summary(stig.reg)

# creating residuals
strg_res<- residuals(stig.reg)

# creating fitted values 
strg_fit<- predict(stig.reg)

# examining homoscedasticity
# Residual vs. Fitted Values Plot

plot(strg_fit, strg_res)
abline(h = 0, col = "red")

# employing Breusch-Pagan Test

bptest(strg_res ~ PI + Brochure +  PI*Brochure + PI*Brochure*MGSQ, data = mdf3)

# testing for autocorelation 

acf(strg_res, main = "Autocorrelation of Residuals")
dwtest(stig.reg)

# normaility of residuals 

qqnorm(strg_res)
shapiro.test(strg_res)

#----------- running transfomations on DV----------------
mdf3$L_SSOSH <- log(mdf3$SSOSH)
mdf3$S_SSOSH <- sqrt(mdf3$SSOSH) 

# tesdting log transformed 

stig.reg_L<- lm(L_SSOSH ~ PI + Brochure +  PI*Brochure + PI*Brochure*MGSQ, data = mdf3)
summary(stig.reg_L)

# creating residuals
strg_res_L<- residuals(stig.reg_L)

# creating fitted values 
strg_fit_L<- predict(stig.reg_L)

# normaility of residuals 

qqnorm(strg_res_L)
shapiro.test(strg_res_L)



library(rlist)

train <- read.csv('C:/Users/Robin/Box/EMR/data/Imputed_data/train_dat(phq9)_v2.csv',row.names = 1)
label <- read.csv('C:/Users/Robin/Box/EMR/data/Imputed_data/label(phq9).csv',row.names = 1)
race <- read.csv('C:/Users/Robin/Box/EMR/data/Imputed_data/race_2414.csv',row.names = 1)
phq_total <- read.csv('C:/Users/Robin/Box/EMR/data/Imputed_data/phq_9_distribution.csv')

phq_total <- phq_total$phq9_total
phq_total_b <- rep(0,2414)
phq_total_b[phq_total<5] <- 'control'
phq_total_b[phq_total>8] <- 'case'

colnames(label) <- c('phq9')
train <- cbind(train,race)

case_train <- train[label$phq9==1,]
control_train <- train[label$phq9==0,]

# Federal aid
prop.test(c(365, 701), c(429, 876)) ## *
prop.test(c(22, 124), c(38, 125)) ## *
prop.test(c(125, 159), c(151, 512)) ## *
prop.test(c(22, 91), c(39, 181)) ## *

prop.test(c(1066, 86), c(1305,226))  # B vs W
prop.test(c(1066, 284), c(1305, 663)) # B vs H
prop.test(c(86, 284), c(226, 663)) # W vs H
# planned pregnancy
prop.test(c(147, 317), c(429, 876)) ## missing
prop.test(c(13, 68), c(38, 188)) ## missing
prop.test(c(56, 218), c(151, 512)) ## missing

prop.test(c(57, 188), c(429, 876)) ## *
prop.test(c(20, 143), c(38, 188)) ## *
prop.test(c(46, 271), c(151, 512)) ## *
prop.test(c(18, 115), c(39, 181)) ## 


prop.test(c(464, 81), c(1305,226))
prop.test(c(464, 274), c(1305, 663))
prop.test(c(81, 274), c(226, 663))

#tabacco use
prop.test(c(160, 383), c(429, 876)) # missing
prop.test(c(16, 85), c(38, 188)) # missing
prop.test(c(57, 220), c(151, 512)) # missing

prop.test(c(71, 88), c(429, 876)) # *
prop.test(c(9, 9), c(38, 188)) #  *
prop.test(c(19, 29), c(151, 512)) # *
prop.test(c(3, 3), c(39, 181)) # 


#pain
prop.test(c(12, 57), c(429, 876)) # *
prop.test(c(2, 9), c(38, 188))
prop.test(c(12, 30), c(151, 512))

prop.test(c(992, 41), c(1882, 49)) ##  <6  vs >=6

#single
prop.test(c(398,785), c(429, 876)) ## 
prop.test(c(23, 71), c(38, 188))   ## *
prop.test(c(121, 327), c(151, 512)) ## *
prop.test(c(23, 75), c(39, 181)) ## 

prop.test(c(398,23), c(429, 38)) ## B vs W  Case  *
prop.test(c(398,121), c(429, 151)) ## B vs H  *
prop.test(c(398,23), c(429, 39)) ## B vs Other  *

prop.test(c(785,71), c(876, 188)) ## B vs W  Control  * 
prop.test(c(785,327), c(876, 512)) ## B vs H  *
prop.test(c(785,75), c(876, 181)) ## B vs Other  *
# Within single, black significantly higher than other

#vitamin
#employ
prop.test(c(47,147), c(429, 876)) ## *
prop.test(c(8, 56), c(38, 188))    
prop.test(c(19, 89), c(151, 512)) 
prop.test(c(4, 46), c(39, 181))

#age
#---black *
#---hispanic *

prop.test(c(7,29), c(18, 129)) ## *

#BMI
#--None

#------------------------------------------- within case/control comparison for Supplement S3 and S4
# nhb,nhw,h,other
#case,control

length(phq_9_total_b)
single <- c(398,23,121,23,785,71,327,75)
insurance <- c(64,16,26,17,175,124,159,90)
pregnancy <- c(57,20,46,18,188,143,271,115)
employed <- c(47,8,19,4,147,56,89,46)
tobacco <- c(71,9,19,3,88,9,29,3)

# Variables of interest
variables_of_interest <- c("single","private_insurance","mr_v1_pregnancyinfo_planned_pregnancy","employed","Parttime","Student",
                           "mr_v1_sh_tobacco", "mr_v1_sh_alcohol","mr_v1_sh_sa")

# Initialize an empty list to store the results
results_list <- list()

# Loop through each variable
for (var in variables_of_interest) {
  # Calculate counts using table
  counts <- table(race$race, phq_total_b, train_2414[[var]])
  
  # Extract counts for cases and controls for each race category
  black_cases <- counts["black", "case", 2]
  white_cases <- counts["white", "case", 2]
  hispanic_cases <- counts["hispanic", "case", 2]
  other_cases <- counts["Other", "case", 2]
  
  black_controls <- counts["black", "control", 2]
  white_controls <- counts["white", "control", 2]
  hispanic_controls <- counts["hispanic", "control", 2]
  other_controls <- counts["Other", "control", 2]
  
  # Combine into a single vector and store in the results list with the variable name as the key
  results_list[[var]] <- c(black_cases, white_cases, hispanic_cases, other_cases,
                           black_controls, white_controls, hispanic_controls, other_controls)
}

# If you want to convert the results list to a dataframe
results_df <- do.call(rbind, results_list)
rownames(results_df) <- variables_of_interest
results_df
dat <- as.data.frame(t(results_df))
dat['label'] <- c(rep('case',4),rep('control',4))
dat['race'] <- c(429,38,151,39,876,188,512,181)

colnames(dat) <- c('single','insurance','planned pregnancy','employed','parttime','student',
                   'tobacco','alcohol','subtance abuse','label','race')

dat <- data.frame(race = c(429,38,151,39,876,188,512,181),
                  single = single,
                  insurance = insurance,
                  pregnancy = pregnancy,
                  employed = employed,
                  tobacco = tobacco,
                  label = c(rep('case',4),rep('control',4)))

  
dat_demo <- data.frame(bmi=train$bmi,
                       age=train$age,
                       phq_total=phq_total,
                       race = race$race)

final_result <- list()
final_stat <- list()
lable_type <- 'case'

for( i in 1:9){
feature <- dat[dat$label=='case',i]

nhb <- dat[dat$label==lable_type,'race'][1]
nhw <- dat[dat$label==lable_type,'race'][2]
h <- dat[dat$label==lable_type,'race'][3]
other <-dat[dat$label==lable_type,'race'][4]

result <- 0
stat_result <- 0
result[1] <- prop.test(c(feature[2],feature[1]), c(nhw, nhb))$p.value
stat_result[1]<- prop.test(c(feature[2],feature[1]), c(nhw, nhb))$statistic[[1]]

result[2] <- prop.test(c(feature[3],feature[1]), c(h, nhb))$p.value
stat_result[2] <- prop.test(c(feature[3],feature[1]), c(h, nhb))$statistic[[1]]

result[3] <- prop.test(c(feature[4],feature[1]), c(other, nhb))$p.value
stat_result[3] <- prop.test(c(feature[4],feature[1]), c(other, nhb))$statistic[[1]]
  
result[4] <- prop.test(c(feature[2],feature[2]), c(h, nhw))$p.value
stat_result[4] <- prop.test(c(feature[2],feature[2]), c(h, nhw))$statistic[[1]]

result[5] <- prop.test(c(feature[3],feature[2]), c(h, nhw))$p.value
stat_result[5] <- prop.test(c(feature[3],feature[2]), c(h, nhw))$statistic[[1]]
  
result[6] <- prop.test(c(feature[4],feature[3]), c(other, h))$p.value
stat_result[6] <- prop.test(c(feature[4],feature[3]), c(other, h))$statistic[[1]]

result <- p.adjust(result,method="bonferroni")
final_result <- list.append(final_result,result)
final_stat <- list.append(final_stat,stat_result)

  if(i==9){
    for( x in 1:3){
    dat_demo_sub <- dat_demo[label$phq9==0,]
    feature <- dat_demo_sub[,x]

    nhb <- feature[dat_demo_sub$race=='black']
    nhw <- feature[dat_demo_sub$race=='white']
    h <- feature[dat_demo_sub$race=='hispanic']
    o <- feature[dat_demo_sub$race=='Other']

    result <- 0
    stat_result <- 0
    result[1] <- wilcox.test(nhw,nhb)$p.value
    stat_result[1] <- wilcox.test(nhw,nhb)$statistic
    
    result[2] <- wilcox.test(h,nhb)$p.value
    stat_result[2] <- wilcox.test(h,nhb)$statistic
    
    result[3] <- wilcox.test(o,nhb)$p.value
    stat_result[3] <- wilcox.test(o,nhb)$statistic
    
    result[4] <- wilcox.test(h,nhw)$p.value
    stat_result[4] <- wilcox.test(h,nhw)$statistic
    
    result[5] <- wilcox.test(o,nhw)$p.value
    stat_result[5] <- wilcox.test(o,nhw)$statistic
    
    result[6] <- wilcox.test(o,h)$p.value
    stat_result[6] <- wilcox.test(o,h)$statistic
    
    result <- p.adjust(result,method="bonferroni")
    final_result <- list.append(final_result,result)
    final_stat <- list.append(final_stat,stat_result)
    }
  }
}

final_result <- data.frame(final_result)
final_stat <- data.frame(final_stat)

colnames(final_result) <- c(colnames(dat)[1:9],colnames(dat_demo)[1:3])
colnames(final_stat) <- c(colnames(dat)[1:9],colnames(dat_demo)[1:3])


View(t(round(final_stat,2)))
View(t(round(final_result,4)))
#-----------------------------------  case vs control
final_result_2 <- list()
final_stat_2 <- list()

case_dat <- dat[dat$label=="case",]
control_dat <- dat[dat$label=="control",]
  
for( i in 1:9){
feature_case <- case_dat[,i]
feature_control <- control_dat[,i]

nhb_case <- case_dat$race[1]
nhw_case <- case_dat$race[2]
h_case <- case_dat$race[3]
other_case <-case_dat$race[4]

nhb_control <- control_dat$race[1]
nhw_control <- control_dat$race[2]
h_control <- control_dat$race[3]
other_control <-control_dat$race[4]

result_2 <- 0
stat_result_2 <- 0
result_2[1] <- prop.test(c(feature_control[1],feature_case[1]), c(nhb_control, nhb_case))$p.value
stat_result_2[1] <- prop.test(c(feature_control[1],feature_case[1]), c(nhb_control, nhb_case))$statistic[[1]]

result_2[2] <- prop.test(c(feature_control[2],feature_case[2]), c(nhw_control, nhw_case))$p.value
stat_result_2[2] <- prop.test(c(feature_control[2],feature_case[2]), c(nhw_control, nhw_case))$statistic[[1]]

result_2[3] <- prop.test(c(feature_control[3],feature_case[3]), c(h_control, h_case))$p.value
stat_result_2[3] <- prop.test(c(feature_control[3],feature_case[3]), c(h_control, h_case))$statistic[[1]]

result_2[4] <- prop.test(c(feature_control[4],feature_case[4]), c(other_control, other_case))$p.value
stat_result_2[4] <- prop.test(c(feature_control[4],feature_case[4]), c(other_control, other_case))$statistic[[1]]


result_2 <- p.adjust(result_2,method="bonferroni")
final_result_2 <- list.append(final_result_2,result_2)
final_stat_2 <- list.append(final_stat_2,stat_result_2)

  if(i == 9){
    dat_demo_case <- dat_demo[label$phq9==1,]
    dat_demo_control <- dat_demo[label$phq9==0,]
    
    for(x in 1:3){
      nhb_case <-  dat_demo_case[,x][dat_demo_case$race=='black']
      nhw_case  <- dat_demo_case[,x][dat_demo_case$race=='white']
      h_case  <- dat_demo_case[,x][dat_demo_case$race=='hispanic']
      o_case  <- dat_demo_case[,x][dat_demo_case$race=='Other']
    
      nhb_control <- dat_demo_control[,x][dat_demo_control$race=='black']
      nhw_control <- dat_demo_control[,x][dat_demo_control$race=='white']
      h_control <- dat_demo_control[,x][dat_demo_control$race=='hispanic']
      o_control <- dat_demo_control[,x][dat_demo_control$race=='Other']
    
      result_2 <- 0
      stat_result_2 <- 0
      
      result_2[1] <- wilcox.test(nhb_case,nhb_control)$p.value
      stat_result_2[1] <- wilcox.test(nhb_case,nhb_control)$statistic
      
      result_2[2] <- wilcox.test(nhw_case,nhw_control)$p.value
      stat_result_2[2] <- wilcox.test(nhw_case,nhw_control)$statistic
      
      result_2[3] <- wilcox.test(h_case,h_control)$p.value
      stat_result_2[3] <- wilcox.test(h_case,h_control)$statistic
      
      result_2[4] <- wilcox.test(o_case,o_control)$p.value
      stat_result_2[4] <- wilcox.test(o_case,o_control)$statistic

      result_2 <- p.adjust(result_2,method="bonferroni")
      final_result_2 <- list.append(final_result_2,result_2)
      final_stat_2 <- list.append(final_stat_2,stat_result_2)
    }
  }
}

final_result_2 <- data.frame(final_result_2)
final_stat_2 <- data.frame(final_stat_2)

colnames(final_result_2) <- c(colnames(dat)[1:9],colnames(dat_demo)[1:3])
colnames(final_stat_2) <- c(colnames(dat)[1:9],colnames(dat_demo)[1:3])

View(t(round(final_stat_2,2)))
View(t(round(final_result_2,4)))

alpha <- 0.05
final_result_anno <- as.matrix(final_result)
final_result_anno[which(final_result_anno<alpha)] <- 'sig'

final_result_2_anno <- as.matrix(final_result_2)
final_result_2_anno[which(final_result_2_anno<alpha)] <- 'sig'

View(final_result_anno)
View(final_result_2_anno)



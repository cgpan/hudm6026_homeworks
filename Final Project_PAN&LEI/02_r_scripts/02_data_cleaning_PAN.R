# DATA CLEANING FOR HUDM6026 FINAL PROJECT

# read the HSLS open dataset first
load("~/Desktop/PhD_Learning/HSLS-09_Project/public_dataset/hsls_17_student_pets_sr_v1_0.rdata")

df <- hsls_17_student_pets_sr_v1_0
# extract the related variables 
df <- df[, c("X3TGPASTEM","X1SES","X1TXMTSCOR")]
hsls_sub_raw <- df

save(hsls_sub_raw, file = "/Users/panpeter/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/01_raw_data/hsls_sub_raw.rdata")
# subset the stem GPA greater than 0
df <- df[which(df$X3TGPASTEM >=0),]
# subset the SES greater than -2
df <- df[which(df$X1SES >= -2),]

# save the data
hsls_sub <- df

save(hsls_sub, file =  "~/Desktop/PhD_Learning/HUDM6026 Computational Statistics/HUDM6026_Final_Project/01_data/02_processed_data/hsls_sub.rdata")
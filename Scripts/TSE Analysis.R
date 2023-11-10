
#Load Dataset.
df <- read.csv("TSE Dataset full.csv", header = T)

#Create bar charts to display statistical information.

#Fit a table for instrustors.
SpI <- table(df$instr)
#plot chart to show the number of students per instructor.
barplot(SpI , space =0.25, main="Students per Instructors",
         xlab="Instructors",
         ylab="Students",
         border="Black",
         col="Gold", density =25)

#Fit a table for courses.
SpC <- table(df$class)
#plot chart to show the number of students per course.
barplot(SpC , space =0.25, main="Students per Course",
        xlab="Courses",
        ylab="Students",
        border="Black",
        col="blue", density =25)

#Create dataframe for Variables Q1-Q28.
df1 <- df[,6:33]
#Fit mean scores.
MpQ <- print(colMeans(df1))
#Plot Chart to show mean student scores per question.
barplot(MpQ , space =0.25, main="Mean Student Scores per Question",
        xlab="Courses",
        ylab="Scores",
        border="Black",
        col="purple", density =25)


#MODIFICATION OF DATASET

#Reduce dimensionality of the dataset.
#Remove any incomplete cases/data.
df <- na.omit(df)
#No missing values found

#Create a dataframe with likert-scale items only(Q1-Q28).
df_pca <- df[,6:33]

#Do Keiser-Meyer-Olkin (KMO) to check if dimensionality reduction should be done.
library(psych)
KMO(df_pca)
#Overall MSA reading is > 0.8, which warrants a dimensional reduction on the datset.

#A Principal Component Analysis (PCA) will be used for the dimensional reduction
#as creates a few points(components) that represent/capture the essence of the 28 questions.
#Fit a model for the pca on the likert-scale (df_pca).
Components <- prcomp(df_pca)
#Plot a Scree plot for the points(components) of Correlation.
screeplot(Components, type = "lines")
#The Scree plot suggest that components 1 and 2 has more correlation to the 28 
#questions than the other components.

#Run parallel analysis as a confirmation of the findings shown in the scree plot.
library(paran)
paran(df_pca)
#Confirmed, the parallel analysis retained components 1 and 2 as the factors 
#the survey primarily measures.

#Force the two components into PCA model. 
Components <- pca(df_pca, 2)

#Check loadings to view scores of the 2 components.
Components

#Add the component scores to the original dataset.
df$InstrSat <- Components$scores[,1]
df$CourseSat <- Components$scores[,2]

View(df)

#The scores show that Q1 - Q12 are more correlated to RC2 component because, although the scores 
#from RC1 component are > 0.4, RC2 scores are higher in comparison to RC1. Based on the 
#nature of questions 1-12 this part of the survey was focused on Course Satisfaction.

#The scores show that Q13 - Q28 are more correlated to RC1 component because, 
#although the scores from RC2 component are > 0.4, RC1 scores are higher in comparison to 
#RC2. Based on the nature of questions 13 - 28 this part of the survey was focused on
#Instructor Satisfaction. 



#HYPOTHESIS TESTING
#H0: There is no effect of instr and/or class on InstrSat and CourseSat.    
#H1: There is an effect of instr and/or class on InstrSat and CourseSat.
# InstrSat, CourseSat ~ Instr*Class (DV1, DV2 ~ IV1*IV2)

#1. Normality Test
#Because the dataset has a sample size(observations) > 60, Normality
#will have to be tested using a QQ plot on both DVs. 
qqnorm(df$InstrSat)
qqline(df$InstrSat)
qqnorm(df$CourseSat)
qqline(df$CourseSat)
#The data appears to have a fairly normal distribution

#2. Homogeneity of variance was not tested as all of the sample groups (InstrSat, 
#CourseSat, Instr & Class) have the same sample size. Since the scores obtain are 
#from ordinal data a boxplot was used to show homogeneity.  
boxplot(df$InstrSat ~ df$instr)
boxplot(df$InstrSat ~ df$class)
boxplot(df$CourseSat ~ df$instr)
boxplot(df$CourseSat ~ df$class)
#As seen in the box plots, the size of each sample group are fairly the same.  

#3. Identify factor/categorical variables.
#Display the levels of the chosen Independent variables(IV) to confirm multiple levels.
table(df$instr)
table(df$class)

#Change the IVs above to factors because they contain multiple levels (categories).
df$instr <- as.factor(df$instr)
df$class <- as.factor(df$class)

#4. Run a MANOVA test because its suited to running a test with 2 DV's and factor IV's.
#Also running a regular ANOVA test multiple times can inflate type I errors. MANOVA avoids this. 
fit <- manova(cbind(df$InstrSat, df$CourseSat) ~ df$instr + df$class)
summary(fit)
#MANOVA showed a significant main effect of instr 
#[F(2,5806) = 39.64, p < 0.001, V = 0.03]  and class [F(11,5806) = 11.54, p < 0.001, V = 0.04]. 

#5. Observe the ANOVA results for each DV to view where is being affected.
summary.aov(fit)
#both IVs had an effect on the DVs.

#6. Model individual ANOVA of DVs for depiction of confidence levels and effect size calculation.
es1 <- aov(InstrSat ~ instr, data = df)
es2 <- aov(InstrSat ~ class, data = df)
es3 <- aov(CourseSat ~ instr, data = df)
es4 <- aov(CourseSat ~ class, data = df)

#7. Run effect size calculation
library(lsr)
etaSquared(es1)
etaSquared(es2)
etaSquared(es3)
etaSquared(es4)

#8. Plot confidence level for each ANOVA. 
library(sjPlot)
plot_model(es1, type = "pred")
plot_model(es2, type = "pred")
plot_model(es3, type = "pred")
plot_model(es4, type = "pred")
#The confidence level charts depicts the InstrSat & CourseSat scores for each class and instr.
#All of the confidence level charts show that there are differences 
#in relation to affecting InstrSat and CourseSat, as some of the confidence intervals (points) 
#vary from each other. 

fit1 <- aov(df$InstrSat ~ df$instr+df$class)
fit2 <- aov(df$CourseSat ~ df$instr+df$class)
w
#9. Run Tukey HSDs on ANOVA models to pinpoint where the differences are 
TukeyHSD(fit1)
TukeyHSD(fit2)
#Please see worded report appendices for results and conclusive analysis. 


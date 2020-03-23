```{r}
### General Sentiments of Financial Documents are -Not Significant-
# mod<-lm(Per_Change_T1~Industry+General.Headline+General.First.Paragraph+
#           General.Last.Paragraph+mkvaltq
#         ,data = tr)
# summary(mod)
#######
#T1
#######

library(gmodels)

tr$up <- ifelse(tr$Class1 == "Increase", 1, 0)

logmod <- glm(
    up ~ Industry + Finance.Headline + Finance.First.Paragraph +
        Finance.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

CrossTable(ifelse(predict(logmod, newdata = df, "respons") > .3, 1, 0), ifelse(df$Class1 ==
                                                                                   "Increase", 1, 0))

logmod <- glm(
    up ~ Industry + General.Headline + General.First.Paragraph +
        General.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

CrossTable(ifelse(predict(logmod, newdata = te, "respons") > .5, 1, 0), ifelse(te$Class1 ==
                                                                                   "Increase", 1, 0))

logmod <-
    glm(up ~ Industry + Finance.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod, newdata = te, "respons") > .5, 1, 0), ifelse(te$Class1 ==
                                                                                   "Increase", 1, 0))

logmod <-
    glm(up ~ Industry + General.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod, newdata = te, "respons") > .5, 1, 0), ifelse(te$Class1 ==
                                                                                   "Increase", 1, 0))
#######
#T5
#######
tr$up <- ifelse(tr$Class5 == "Increase", 1, 0)

logmod <- glm(
    up ~ Industry + Finance.Headline + Finance.First.Paragraph +
        Finance.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

logmod <- glm(
    up ~ Industry + General.Headline + General.First.Paragraph +
        General.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

logmod <-
    glm(up ~ Industry + Finance.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)

logmod <-
    glm(up ~ Industry + General.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)
#######
#T30
#######
tr$up <- ifelse(tr$Class30 == "Increase", 1, 0)

logmod <- glm(
    up ~ Industry + Finance.Headline + Finance.First.Paragraph +
        Finance.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

logmod <- glm(
    up ~ Industry + General.Headline + General.First.Paragraph +
        General.Last.Paragraph + mkvaltq,
    data = tr,
    family = "binomial"
)
summary(logmod)

logmod <-
    glm(up ~ Industry + Finance.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)

logmod <-
    glm(up ~ Industry + General.Combined + mkvaltq,
        data = tr,
        family = "binomial")
summary(logmod)




###############################################
###   LOGIT
###############################################



mod <- lm(Per_Change_T1 ~ log_F_H + log_F_P_1 + log_F_P_L + mkvaltq + Delta
          ,
          data = tr)
summary(mod)
prod(1.1 ^ mod$coefficients[2:4])


modgen <- lm(Per_Change_T1 ~ log_G_H + log_G_P_1 + log_G_P_L + mkvaltq +
                 Delta
             ,
             data = tr)
summary(modgen)
prod(1.1 ^ modgen$coefficients[2:4])


sqrt(abs(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared))


mod <- lm(Per_Change_T5 ~ log_F_H + log_F_P_1 + log_F_P_L + mkvaltq + Delta
          ,
          data = tr)

summary(mod)
prod(1.1 ^ mod$coefficients[2:4])

modgen <- lm(Per_Change_T5 ~ log_G_H + log_G_P_1 + log_G_P_L + mkvaltq +
                 Delta
             ,
             data = tr)
summary(modgen)
(prod(1.1 ^ modgen$coefficients[2:4]) - 1) * 100

sqrt(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared)

mod <- lm(Per_Change_T30 ~ log_F_H + log_F_P_1 + log_F_P_L + mkvaltq + Delta
          ,
          data = tr)
summary(mod)
(prod(1.1 ^ mod$coefficients[2:4]) - 1) * 100


modgen <- lm(Per_Change_T30 ~ log_G_H + log_G_P_1 + log_G_P_L + mkvaltq +
                 Delta
             ,
             data = tr)
summary(modgen)

(prod(1.1 ^ modgen$coefficients[2:4]) - 1) * 100


sqrt(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared)

#######################################################
mod <- lm(Per_Change_T1 ~ Industry + log_F_C + mkvaltq + Delta, data = tr)
summary(mod)



modgen <- lm(Per_Change_T1 ~ log_G_C + mkvaltq + Delta
             , data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared)
1.1 ^ mod$coefficients[3:5]
1.1 ^ modgen$coefficients[3:5]

#################
mod <- lm(Per_Change_T5 ~ Industry + log_F_C + mkvaltq + Delta, data = tr)
summary(mod)



modgen <- lm(Per_Change_T5 ~ Industry + log_G_C + mkvaltq + Delta
             , data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared)
1.1 ^ mod$coefficients[3:5]
1.1 ^ modgen$coefficients[3:5]
#########
mod <- lm(Per_Change_T30 ~ Industry + log_F_C + mkvaltq + Delta, data = tr)
summary(mod)



modgen <- lm(Per_Change_T30 ~ Industry + log_G_C + mkvaltq + Delta
             , data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared - summary(modgen)$adj.r.squared)
1.1 ^ mod$coefficients[3:5]
1.1 ^ modgen$coefficients[3:5]

```

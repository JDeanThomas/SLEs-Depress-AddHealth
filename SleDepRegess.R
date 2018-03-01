# For time being, drop found additions in Wave II & III from sleCodes
# Circle back to this. Waves reconstructed to include for throughness
# Will not affect calculations if using original Waves df
table(!sleCodes$AID %in% depress$aid)
sleCodes <- sleCodes[which(depress$aid %in% sleCodes$AID), ]


sleCodesReg <- sleCodes
sleCodesReg[is.na(sleCodes)] <- 0
sleCodesReg <- data.frame(apply(sleCodesReg, 2, function(x) as.numeric(x)))

waveISles <- data.frame(sleCodes$AID)
colnames(waveISles)[1] <- "AID"
waveISles <- sleCodesReg[ ,grep("_W1", names(sleCodesReg))]
summary(waveISles)
which(apply(waveISles, 2, var) == 0)


# Full model (no interactions)
waveIMod <- lm(depress$dep9 ~ ., data = waveISles)
summary(waveIMod)

par(mfrow=c(2, 2))
plot(waveIMod)


# Stepwise model selection using Akaike Information Criterion
features <- names(rev(sort(waveIMod$coefficients[!is.na(waveIMod$coef)][-1])))

waveIModStep <- lm(depress$dep9 ~ ., data = waveISles[features])
step(waveIModStep)
summary(waveIModStep)

pvals <- summary(waveIModStep)$coef[ ,4]
features <- names(rev(sort(waveIModStep$coefficients[pvals < 0.1][-1])))
waveIModStep1 <- lm(depress$dep9 ~ ., data = waveISles[features])
summary(waveIModStep1)

pvals <- summary(waveIModStep1)$coef[ ,4]
features <- names(rev(sort(waveIModStep1$coefficients[pvals < 0.05][-1])))
waveIModStep2 <- lm(depress$dep9 ~ ., data = waveISles[features])
summary(waveIModStep2)

# Check signifigance of factors by sequentially dropping each term from model
drop1(waveIModStep2, test="F")

# ANOVA for fitted model vs full model
anova(waveIModStep2, waveIMod)

par(mfrow=c(2, 2))
plot(waveIModStep2)


# Correct for multiple testing 
# Bonferroni
pvals <- summary(waveIModStep2)$coef[ ,4]
padj <- p.adjust(pvals, method = "bonferroni")
multiP <- coef(waveIModStep2)[padj < 0.05]
multiP
features[features %in% names(multiP)]

# False Discovery Rate correction
padj2 <- p.adjust(pvals, method = "fdr")
multiP2 <- coef(waveIModStep2)[padj2 < 0.05]
multiP2
features[features %in% names(multiP2)]


# Final exploritory model
features <- features[features %in% names(multiP2)]
waveIModStep2 <- lm(depress$dep9 ~ ., data = waveISles[features])
summary(waveIModStep2)

# ANOVA of final model
anova(waveIModStep2)

# Check signifigance of factors by sequentially dropping each term from model
drop1(waveIModStep2, test="F")

rm(pvals, padj, multiP, padj2, multiP2)

# appeares to be long-tailed
# non-constant variance of residuals at low end of fitted values
# Signifigant leverage points

#JailTime can't be correct. Will go back through SLE codes again.

# Subjects with Dep9 scores > 1 SD from mean and 0 Wave I SLES
table(depress$dep9[depress$dep9 > mean(depress$dep9) + sd(depress$dep9) & rowSums(waveISles) == 0])


par(mfrow=c(2, 2))
plot(waveIModStep2)

require(faraway)
par(mfrow=c(1, 1))
halfnorm(coef(waveIModStep2)[-1],labs=names(coef(waveIModStep2)[-1]))

par(mfrow=c(2, 2))
termplot(waveIModStep2, partial.resid=TRUE, col.res = "black", smooth=panel.smooth)

par(mfrow=c(1, 1))
stripchart(data.frame(scale(waveISles[features])), method ="jitter", pch=1, las=2, vertical=TRUE)

plot(waveIModStep2$coef, type="h", ylab="Coefficient")


###################


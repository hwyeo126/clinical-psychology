# clinical-psychology
# Meta-analysis
library(readxl)
library(compute.es)
library(MAd)
library(metafor)
library(meta)
setwd("C:/r_temp")

# Effect-size calculation
SP <- read_xlsx("DATA.xlsx", sheet = 1)
res_SP <- mes(m.1 = m_postT, m.2 = m_postC, sd.1 = sd_postT, sd.2 = sd_postC, n.1 = n_postT, n.2 = n_postC, id=ES.ID, data = SP)
res2_SP <- cbind(SP, res_SP[,c(13:14)])
mSP <- rma(g, var.g, data=res2_SP, measure = "SMD", slab=paste(Author, DOP, sep = ", "))

##forest plot
par(mar=c(4,4,1,2))
forest(mSP, xlim=c(-16,6), ylim=c(-1,8), ilab=cbind(SP$n_postT, SP$n_postC), ilab.xpos=c(-8, -6), cex=0.75, xlab="Standardized Mean Difference", mlab="")

op<- par(cex=0.75, font=4)
text(-16, 7, pos=4, c("Social Perception"))
par(font=4) 

text(c(-8, -6), 6.5, c("tx(n)", "ctr(n)"))
text(-16, 6.5, "Author(s) and Year", pos=4)
text(6, 6.5, "SMD [95% CI]", pos=2)

par(op)

text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                                            .(formatC(mSP$QE, digits=2, format="f")), ", df = ", .(mSP$k - mSP$p),
                                            ", p = ", .(formatC(mSP$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(mSP$I2, digits=1, format="f")), "%)")))

## fail-safe N
fsn(g, var.g, data=res2_SP, type="Rosenthal", alpha=.05, digit=2)

##funnel plot
funnel(res2_SP, xlab="Standardized Mean Difference")

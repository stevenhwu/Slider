# Slider - Statistical Likelihood model for Identifying Differential Expression in R

This package is based on 
**A Statistical Model to Identify Differentially Expressed Proteins in 2D PAGE Gels**

Steven H. Wu, Michael A. Black, Robyn A. North, Kelly R. Atkinson, Allen G. Rodrigo

http://www.ploscompbiol.org/article/info%3Adoi%2F10.1371%2Fjournal.pcbi.1000509

## Installation 
`install.packages("slider_1.*.*.tar.gz")`


## Usage
```R
library(slider)
data(simGel)
result<-gelLRT(simGel$data,simGel$group)
LRT.pvalue<-pchisq(result, df=2, lower.tail = FALSE)
```
     


## Abstract

Two dimensional polyacrylamide gel electrophoresis (2D PAGE) is used to identify differentially expressed proteins and may be applied to biomarker discovery. A limitation of this approach is the inability to detect a protein when its concentration falls below the limit of detection. Consequently, differential expression of proteins may be missed when the level of a protein in the cases or controls is below the limit of detection for 2D PAGE. Standard statistical techniques have difficulty dealing with undetected proteins. To address this issue, we propose a mixture model that takes into account both detected and non-detected proteins. Non-detected proteins are classified either as (a) proteins that are not expressed in at least one replicate, or (b) proteins that are expressed but are below the limit of detection. We obtain maximum likelihood estimates of the parameters of the mixture model, including the group-specific probability of expression and mean expression intensities. Differentially expressed proteins can be detected by using a Likelihood Ratio Test (LRT). Our simulation results, using data generated from biological experiments, show that the likelihood model has higher statistical power than standard statistical approaches to detect differentially expressed proteins. An R package, Slider (Statistical Likelihood model for Identifying Differential Expression in R), 

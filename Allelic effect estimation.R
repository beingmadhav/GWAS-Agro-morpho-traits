############################################### ALLELIC EFFECT ESTIMATION ###################################################

## For allelic effect estimation we need to know lines with favourable and unfavorable allele.
# The favorable and unfavorable allele can be known through GWAS result of GAPIT

##lets estimate allelic effect for maker S5D_529816907

#read in the genotype file used for the GWAS
geno <- read.delim(file.choose(),header=TRUE) 
head(geno)

#Marker information
marker_name <- "S5D_529816907"
major_allele <- "G"
minor_allele<- "A"
hetero<-"R"

# Subset data for the secific marker
marker_data <- subset(geno, rs == marker_name)

# Initialize lists to store allele carrier cultivars
minor_allele_carriers <- character(0)
major_allele_carriers <- character(0)
hetero_carriers<-character(0)
# Loop through rows and check allele carriers
for (i in 1:nrow(marker_data)) {
  row <- marker_data[i, ]
  if (minor_allele %in% row) {
    minor_allele_carriers <- c(minor_allele_carriers, names(row)[row == minor_allele])
  }
  if (major_allele %in% row) {
    major_allele_carriers <- c(major_allele_carriers, names(row)[row == major_allele])
  }
  if (hetero %in% row) {
    hetero_carriers <- c(hetero_carriers, names(row)[row == hetero])
  }
}


# Print the results
cat("Cultivars with Minor Allele (C):\n")
cat(minor_allele_carriers, sep = ", ")

cat("\n\nCultivars with Major Allele (A):\n")
cat(major_allele_carriers, sep = ", ")


cat("\n\nCultivars with Hetero state (AC):\n")
cat(hetero_carriers, sep = ", ")

# Now we can use the list to make a file necessary for allelic effect estimation where in first column we have wheat lines, second column has
#allele information for the line and the third column has the BLUP values. 
#Seperate file was made for each marker. 


#Import the allele effect estimation file
S5D_529816907_phenotype=read.csv("S5D_529816907.csv")
S5D_529816907_phenotype$Taxa=as.factor(S5D_529816907_phenotype$Taxa)
S5D_529816907_phenotype$Genotype=as.factor(S5D_529816907_phenotype$Genotype)

anovaresult=aov(PHT~Genotype, data=S5D_529816907_phenotype) # Calculating variance due to genotypes 
summary(anovaresult)

TUKEY=TukeyHSD(anovaresult) #Performing Tukeys HSD test for pair wise comparison between the genotype groups G, A, R
TUKEY$Genotype

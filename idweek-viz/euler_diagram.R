library(eulerr)

combo <- c("Partner DB"=2418,
           "Genomic DB"=2553,
           "Partner DB&Genomic DB"=904,
           "Index cases"=497,
           "Partner DB&Index cases"=497,
           "Genomic DB&Index cases"=497,
           "Partner DB&Genomic DB&Index cases"=497,
           "Named partners"=154,
           "Partner DB&Named partners"=154,
           "Genomic DB&Named partners"=154,
           "Index cases&Named partners"=154,
           "Partner DB&Genomic DB&Named partners"=154,
           "Partner DB&Index cases&Named partners"=154,
           "Genomic DB&Index cases&Named partners"=154,
           "Partner DB&Genomic DB&Index cases&Named partners"=154)

plot(euler(combo, input="union"), quantities = TRUE)
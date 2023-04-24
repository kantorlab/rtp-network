
## Descriptive analysis of intersection of Partner Contact and Unified Individual Dataset

- These data are in the current "Table 1". 
- Code to produce the table is at 
[descriptives-table.Rmd](https://github.com/kantorlab/rtp-network/blob/69a8cf06cb5f135cb524847e0c5e35e5600eb546/descriptives-table.Rmd).
- This code does not depend on any other R/Rmd files. It builds straight 
from the datasets.


## Mean number of named, diagnosed and sequenced partners by index cases who named partners.

- These data are in the current "Table 2".  
- Code to produce the table is also in the 
[descriptives-table.Rmd](https://github.com/kantorlab/rtp-network/blob/e12d4d9dd34fed8ea2912722b78089ded589a08e/descriptives-table.Rmd#L497-L948) file.
- As above, this code does not depend on any other R/Rmd files. It builds straight 
from the datasets.


## Transmission Clusters and Distribution of Named Partners 

- These data are in the current "Table 3".
- Code to produce the table is in the 
[molecular-cluster-analysis.R](https://github.com/kantorlab/rtp-network/blob/9ab426e60e91d5d84730a6f71e8327d6b577c4cf/molecular-cluster-analysis.R#L46-L263) file.
- This code does not depend on any R/Rmd files.


## Factors associated with linked partners being named and vice versa.

- These data are in the current "Table 4".
- Table 4A: Logistic regression on index case being genetically linked to at least one of their named partners.
- Table 4B:  Logistic regression on index case reporting a named partnership in its genetic cluster(s). 
- This code uses RDS objects generated in the [molecular-cluster-analysis.R](https://github.com/kantorlab/rtp-network/blob/d051ca218896064a5bbb23aa9eeda4b8e3ecb278/molecular-cluster-analysis.R#L274-L280) file.
- [Code](https://github.com/kantorlab/rtp-network/blob/d051ca218896064a5bbb23aa9eeda4b8e3ecb278/table4.rmd#L308) for Table 4A.
- Created a `print-results.Rmd` to make it easier to extract results for writing. Depends on `table4.rmd`.

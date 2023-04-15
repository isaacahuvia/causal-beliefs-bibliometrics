
## see https://www.agreestat.com/books/cac5/chapter7/chap7.pdf

library(irr)
library(irrCAC)

irr::kappa2(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_aqsa))
irrCAC::gwet.ac1.raw(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_aqsa))

irr::kappa2(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_fiona))
irrCAC::gwet.ac1.raw(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_fiona))

irr::kappa2(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_kelly))
irrCAC::gwet.ac1.raw(cbind(clean_screening_results$decision_isaac, clean_screening_results$decision_kelly))
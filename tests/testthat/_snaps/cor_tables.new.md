# cor_matrix works with FIML and bootstrapping

    Code
      fiml_boot_cor_matrix <- cor_matrix(dplyr::select(ess_health_sample, -pspwght),
      missing = "fiml", bootstrap = 100, seed = 12345)
    Message
      Starting to bootstrap 100 resamples for BCa CIs. This might take a while.
    Condition
      Warning in `norm.inter()`:
      extreme order statistics used as endpoints


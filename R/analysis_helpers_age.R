#Function to calculate the spectral age-dependent transmission of the eye, according to the DIN/TS 5031-100
prerecep_filter <- function(wavelength, age) {
  #Density
  Dichte <- 
    (0.15 + 0.000031*age^2)*
    (400/wavelength)^4+151.5492*exp(-((0.057*(wavelength-273))^2)) +
    (1.05-0.000063*age^2)*2.13*exp(-((0.029*(wavelength-370))^2)) +
    (0.059 + 0.000186*age^2)*11.95*exp(-((0.021*(wavelength-325))^2)) +
    (0.016 + 0.000132*age^2)*1.43*exp(-((0.008*(wavelength-325))^2))+0.06
  #Return value
  10^(-Dichte)
}

#Prereceptoral Filtering for a 32yr-old reference observer
Tau32 <- tibble::tibble(Wellenlaenge = 380:780,
                        Tau32 = prerecep_filter(Wellenlaenge, 32))

#Function to calculate the age dependent relative transmission
Tau_rel_fun <- function(age){
  WL <- 380:780
  prerecep_filter(WL, age)/prerecep_filter(WL, 32)
}

#Function to calculate the age dependent transmission (Scalar)
k_trans_fun <- function(age, mel_wt_Spectrum){
  #Lightspectrum*melanopicActionSpectrum(=mel_wt_Spectrum)*rel_Transmission
  WL <- 380:780
  Tau_rel <- Tau_rel_fun(age)
  zahler <- sum(
    mel_wt_Spectrum * Tau_rel
  )
  nenner <- sum(mel_wt_Spectrum)
  zahler / nenner
  
}

#Function to calculate the age-dependent reduction in pupil size
k_pup_fun <- function(age){
  c <- 0.00612
  (1-c*(age-32))^2
}

#Function to calculate the total age-dependent correction factor
k_mel <- function(age, mel_wt_Spectrum) {
  k_trans_fun(age, mel_wt_Spectrum) * k_pup_fun(age)
}

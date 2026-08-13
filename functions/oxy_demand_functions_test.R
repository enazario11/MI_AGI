
#convert DO mol/kg to mmol/m^3 to atm 
do_to_atm <- function(do, t, s) {
  
  po2_atm <- do
  po2_atm[] <- NA_real_

  w_values <- !is.na(do) & !is.na(t) & !is.na(s)
  DO <- do[w_values]; T <- t[w_values]; S <- s[w_values]

  # seawater density to move from a per-kg to a per-volume basis
  rho <- marelac::sw_dens(S = S, t = T, P = 1.013253) # kg/m3

  # mol/kg -> mmol/m3 
  do_mmol_m3 <- DO * 1000 * rho 

  #solubility mmol/m3/bar to atm 
  a_o2_bar <- marelac::gas_solubility(S = S, t = T, species = "O2")
  a_o2_atm <- a_o2_bar / 0.9869

  #po2_atm
  po2_atm[w_values] <- do_mmol_m3 / a_o2_atm

  return(po2_atm)
  }

rast_do_to_atm <- function(do, t, s, filename = ""){

  do_temp <- do
  do_atm <- lapp(sds(do, t, s), fun = do_to_atm)
  values(do_temp) <- values(do_atm)
  units(do_temp) <- "atm"
  do_temp
}

#caclulated metabolic demand 
OxyDemand<- function(Tpref, PO2_thresh, T_C, W = NULL, d = 0.700, K, j2 = 8000, j1 = 4500, 
                      Linf, LwA, LwB){

  # removing K/(1-d) because it cancels out in numerator and denominator
  # Convert C to K temperatues
  T_K <- T_C + 273.15 
  Tpref_K <- Tpref + 273.15
  
  #Convert length to weight using scaling relationship
  Winf <- LwA * Linf**LwB
  W <- Winf * (1/3) #Moree and Clarke both assume W is 1/3 Winf. See moree ms and Clarke email.

  O2_demand <- ((W**(1 - d)) * exp(-j2/T_K) * PO2_thresh * exp(-j1/Tpref_K)) / 
    ((Winf**(1 - d)) * exp(-j1/T_K) * exp(-j2/Tpref_K))
  
  O2_demand
} 

MI_calc <- function(A0, Bn = 0.01, DO, E0, kB = 0.000086173324, T_C){

  T_K = T_C  + 273.15
  
  DO_supply = A0*Bn*DO
  DO_demand = exp(-E0/(kB*T_K))
  
  MI = DO_supply/DO_demand
  return(MI)
 
}

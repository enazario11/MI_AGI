
#convert DO mol/kg to mmol/m^3 to atm 
do_to_atm <- function(do, t, s, thresh = FALSE) {
  
  # if (thresh == TRUE){
  #     #use the salinity and temperature conditions to get unit to mmol/L
  #     do_mmol_L <- respR::convert_DO(do, "mL/L", "mmol/L", S = s, t = t, P = 1.013253)
  #     do <- do_mmol_L/0.001 #convert L to m^3
  #   }

  # If input is in raster format, convert to vector
  rast_format <- inherits(do, "SpatRaster")
  if (rast_format) {
    do_rast <- do
    do <- terra::values(do)[, 1]
    t_rast <- t
    t <- terra::values(t)[, 1]
    s_rast <- s
    s <- terra::values(s)[, 1]
  }

  # seawater density to move from a per-kg to a per-volume basis
  rho <- marelac::sw_dens(S = s, t = t, P = 1.013253) # kg/m3

  # mol/kg -> mmol/m3 
  do_mmol_m3 <- do * 1000 * rho 

  po2_atm <-  rep(NA_real_, length(do_mmol_m3))

  for(i in 1:length(do_mmol_m3)){
    if(!is.na(do[i]) && !is.na(t[i]) && !is.na(s[i])){
        a_o2_bar <- marelac::gas_solubility(S = s[i], t = t[i], species = "O2")
        a_o2_atm <- a_o2_bar / 0.9869
        po2_atm[i] <- do_mmol_m3[i] / a_o2_atm
    }
  }

  # Handle the return value if raster
  if (rast_format) {
    po2_atm_rast <- do_rast
    terra::values(po2_atm_rast) <- po2_atm
    return(po2_atm_rast)
  }

  return(po2_atm)
  }

#caclulated metabolic demand 
AGI_calc <- function(Tpref, PO2_thresh, T_C, W, d = 0.700, K, j2 = 8000, j1 = 4500, 
                      Linf, LwA, LwB){

  # removing K/(1-d) because it cancels out in numerator and denominator
  # Convert C to K temperatues
  T_K <- T_C + 273.15 
  Tpref_K <- Tpref + 273.15
  
  #Convert length to weight using scaling relationship
  Winf <- LwA * Linf**LwB

  O2_demand <- ((W**(1 - d)) * exp(-j2/T_K) * PO2_thresh * exp(-j1/Tpref_K)) / 
    ((Winf**(1 - d)) * exp(-j1/T_K) * exp(-j2/Tpref_K))
  
  O2_demand
} 

MI_calc <- function(A0, Bn, DO_kPa, E0, kB, T_C){

  T_K = T_C  + 273.15
  
  DO_supply = A0*Bn*DO_kPa
  DO_demand = exp(-E0/(kB*T_K))
  
  MI = DO_supply/DO_demand
  return(MI)
 
}

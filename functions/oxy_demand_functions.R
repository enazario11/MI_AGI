#convert DO mmol/m^3 to atm 
DO_to_atm <- function(loc_enviro_dat, depth){
  for(i in 1:nrow(loc_enviro_dat)) {
    # Constants:
    # Constants for the calculation of the saturation concentration of O2 in sea water (from Sarmiento & Gruber (2006), Garcia & Gorden (1992)):
    b1 = 2.00907
    b2 = 3.22014
    b3 = 4.05010
    b4 = 4.94457
    b5 = 0.256847
    b6 = 3.88767
    b7 = 0.00624523
    b8 = 0.00737614
    b9 = 0.010341
    b10 = 0.00817083
    b11 = 0.000000488682
    
    # Other constants:
    R = 8.31             # Gas constant [J/(mol*K)]
    GSmv = 22.3916       # Molar volume O2 [L/mol] (from Sarmiento & Gruber (2006))
    pmv = 0.000032       # Partial molar volume O2 [m3/mol] (from Deutsch et al. (2015, Science))
    KC = 273.15          # Kelvin conversion [K]
    Ks = 0.000086173     # Boltzmann constant [J/K]
    
    #calc in-situ density of seawater
    pressure = (1025 * 9.81 * depth) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
    press_dbar = pressure / 1000000 #get calculated pressure into dbar
    
    Tpot = loc_enviro_dat$votemper_mean[i] #temp in C
    T_scaled = log((298.15 - Tpot) / (KC + Tpot))
    rho = gsw::gsw_rho(SA = loc_enviro_dat$vosaline_mean[i], p = press_dbar, CT = Tpot)
    
    #calculate saturation concentration of O2 in seawater
    l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+loc_enviro_dat$vosaline_mean[i]*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*loc_enviro_dat$vosaline_mean[i]^2
    calc_sat_o2 = (1000/GSmv) * exp(l)
    calc_sat_o2_molm3 = calc_sat_o2/1000 #mol/m^3
    
    #DO solubility at surface 
    DO_sol_surf = calc_sat_o2_molm3/0.209 #mol/m^3*atm
    
    #calc pressure correction
    pressure_corr = exp(((depth*rho*9.81*pmv)/R)/(Tpot+KC)) 
    
    #calc solubility at any depth 
    sol_depth = DO_sol_surf*pressure_corr #mol/m^3*atm
    
    #convert DO to mol/kg
    DO_mol_kg = loc_enviro_dat$o2_mean[i]/1025000 #convert input DO from mmol/m^3 to mol/kg
    
    #calc partial pressure O2 at any loc and depth 
    pO2 = (DO_mol_kg*rho)/(sol_depth) #atm
    
    #create column to save O2 in atm 
    loc_enviro_dat[i, paste0("pO2","_", depth)] <- pO2
  }
  
  return(loc_enviro_dat)
}


#caclulated metabolic demand
OxyDemand <- function(Tpref, PO2_thresh, T_C, W = 51807.63, d = 0.700, K = 0.070, j2 = 8, j1 = 4.5, 
                      Linf = 321, LwA = 0.01670, LwB = 2.847){

  # removing K/(1-d) because it cancels out in numerator and denominator, right?
  # Convert C to K temperatues
  T_K <- T_C + 273.15 
  Tpref_K <- Tpref + 273.15
  
  # Convert length to weight using scaling relationship
  Winf <- LwA * Linf**LwB

  O2_demand <- W**(1 - d) * exp(-j2 / T_K) * PO2_thresh * exp(-j1 / Tpref_K) / 
    (Winf**(1 - d) * exp(-j1 / T_K) * exp(-j2 / Tpref_K))
  
  O2_demand
} 

rast_to_atm <- function(do, temp, so, depth){
    # Constants:
    # Constants for the calculation of the saturation concentration of O2 in sea water (from Sarmiento & Gruber (2006), Garcia & Gorden (1992)):
    b1 = 2.00907
    b2 = 3.22014
    b3 = 4.05010
    b4 = 4.94457
    b5 = 0.256847
    b6 = 3.88767
    b7 = 0.00624523
    b8 = 0.00737614
    b9 = 0.010341
    b10 = 0.00817083
    b11 = 0.000000488682
    
    # Other constants:
    R = 8.31             # Gas constant [J/(mol*K)]
    GSmv = 22.3916       # Molar volume O2 [L/mol] (from Sarmiento & Gruber (2006))
    pmv = 0.000032       # Partial molar volume O2 [m3/mol] (from Deutsch et al. (2015, Science))
    KC = 273.15          # Kelvin conversion [K]
    Ks = 0.000086173     # Boltzmann constant [J/K]
    
    #calc in-situ density of seawater
    pressure = (1025 * 9.81 * depth) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
    press_dbar = pressure / 1000000 #get calculated pressure into dbar
    
    Tpot = temp #temp in C
    T_scaled = log((298.15 - Tpot) / (KC + Tpot))
    #rho = gsw::gsw_rho(SA = so, p = press_dbar, CT = Tpot) #input can't be a raster, using average instead
    rho = 1025
    
    #calculate saturation concentration of O2 in seawater
    l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+so*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*so^2
    calc_sat_o2 = (1000/GSmv) * exp(l)
    calc_sat_o2_molm3 = calc_sat_o2/1000 #mol/m^3
    
    #DO solubility at surface 
    DO_sol_surf = calc_sat_o2_molm3/0.209 #mol/m^3*atm
    
    #calc pressure correction
    pressure_corr = exp(((depth*rho*9.81*pmv)/R)/(Tpot+KC)) 
    
    #calc solubility at any depth 
    sol_depth = DO_sol_surf*pressure_corr #mol/m^3*atm
    
    #convert DO to mol/kg
    DO_mol_kg = do/1025000 #convert input DO from mmol/m^3 to mol/kg
    
    #calc partial pressure O2 at any loc and depth 
    pO2 = (DO_mol_kg*rho)/(sol_depth) #atm
  
  return(pO2)
}

thresh_to_atm <- function(do, temp, so, depth){
  # Constants:
  # Constants for the calculation of the saturation concentration of O2 in sea water (from Sarmiento & Gruber (2006), Garcia & Gorden (1992)):
  b1 = 2.00907
  b2 = 3.22014
  b3 = 4.05010
  b4 = 4.94457
  b5 = 0.256847
  b6 = 3.88767
  b7 = 0.00624523
  b8 = 0.00737614
  b9 = 0.010341
  b10 = 0.00817083
  b11 = 0.000000488682
  
  # Other constants:
  R = 8.31             # Gas constant [J/(mol*K)]
  GSmv = 22.3916       # Molar volume O2 [L/mol] (from Sarmiento & Gruber (2006))
  pmv = 0.000032       # Partial molar volume O2 [m3/mol] (from Deutsch et al. (2015, Science))
  KC = 273.15          # Kelvin conversion [K]
  Ks = 0.000086173     # Boltzmann constant [J/K]
  
  #calc in-situ density of seawater
  pressure = (1025 * 9.81 * depth) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
  press_dbar = pressure / 1000000 #get calculated pressure into dbar
  
  Tpot = temp #temp in C
  T_scaled = log((298.15 - Tpot) / (KC + Tpot))
  rho = gsw::gsw_rho(SA = so, p = press_dbar, CT = Tpot) #input can't be a raster, using average instead
  
  #calculate saturation concentration of O2 in seawater
  l = b1+b2*T_scaled+b3*(T_scaled^2)+b4*(T_scaled^3)+b5*(T_scaled^4)+b6*(T_scaled^5)+so*(b7+b8*T_scaled+b9*(T_scaled^2)+b10*(T_scaled^3)) + b11*so^2
  calc_sat_o2 = (1000/GSmv) * exp(l)
  calc_sat_o2_molm3 = calc_sat_o2/1000 #mol/m^3
  
  #DO solubility at surface 
  DO_sol_surf = calc_sat_o2_molm3/0.209 #mol/m^3*atm
  
  #calc pressure correction
  pressure_corr = exp(((depth*rho*9.81*pmv)/R)/(Tpot+KC)) 
  
  #calc solubility at any depth 
  sol_depth = DO_sol_surf*pressure_corr #mol/m^3*atm
  
  #convert DO to mol/kg
  DO_mol_kg = do/1025000 #convert input DO from mmol/m^3 to mol/kg
  
  #calc partial pressure O2 at any loc and depth 
  pO2 = (DO_mol_kg*rho)/(sol_depth) #atm
  
  return(pO2)
}


thresh_atm <- function(do_mL_L = 2, temp_C, so_psu, depth){
  pressure_Pa = (1025 * 9.81 * 0) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
  press_bar = pressure_Pa/100000 #get calculated pressure into bar
  
      #use the salinity and temperature conditions at the surface to get unit to mmol/L
  thresh_mmol_L <- convert_DO(do_mL_L, "mL/L", "mmol/L", S = 33.499664, t = 19.191122, P = press_bar)
  thresh_mmol_m3 <- thresh_mmol_L/0.001 #convert L to m^3
  
  #change temp, sal, and depth based on condtions at depth
  thresh_atm <- thresh_to_atm(thresh_mmol_m3, temp = temp_C, so = so_psu, depth = depth)
  
  thresh_atm
}

# thresh_atm <- function(do_mL_L, temp_C, so_psu, depth){
#   
#   #convert ml/L to ml/m^3 
#   thresh_mL_m3 <- do_mL_L*1000 #mL/L * 0.001L/m^3
#   
#   #calculate density in g/kg
#   pressure_Pa = (1025 * 9.81 * depth) + 101325 #avg density of sea water (kg/m^3) * gravity * depth + atmospheric pressure at surface in Pa
#   press_bar = pressure_Pa/100000 #get calculated pressure into dbar
#   
#   dens_sw_kg_m3 <- gsw::gsw_rho(SA = so_psu, p = press_dbar, CT = temp_C) #units of density output is kg/m^3
#   dens_sw_kg_ml <- dens_sw_kg_m3/1000000 #1 m^3/1000000 mL
#   dens_sw_g_ml <- dens_sw_kg_ml*1000 #1000g/kg 
#   
#   #convert ml/m^3 to mmol/m^3
#   thresh_g_m3 <- thresh_mL_m3*dens_sw_g_ml #in-situ density from above in g/mL
#   thresh_mol_m3 <- thresh_g_m3/31.9988 #molar weight of DO is 31.99 g/mol
#   thresh_mmol_m3 <- thresh_mol_m3*1000  #1000 mmol in a mol
#   
#   #use rast_to_atm function from above to get thresh from mmol/m^3 to atm 
#   o2_atm <- rast_to_atm(do = thresh_mmol_m3, temp = temp_C, so = so_psu, depth = depth)
#   o2_atm
# }


MI_calc <- function(A0, Bn, DO_kPa, E0, kB, T_C){

  T_K = T_C  + 273.15
  
  DO_supply = A0*Bn*DO_kPa
  DO_demand = exp(-E0/(kB*T_K))
  
  MI = DO_supply/DO_demand
  return(MI)
 
}

#libraries
import xarray as xr
import geopandas as gpd
import rioxarray 
import dask
import matplotlib.pyplot as plt
import numpy as np
import regionmask

#NWA
nwa_union = gpd.read_file("data/enviro/nwa/nwa_union.gpkg") 

#oxygen
#load zarr
o2_nwa_zarr = xr.open_zarr("data/enviro/nwa/do/raw/o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
o2_nwa = o2_nwa_zarr["o2"]  # pick the variable
o2_nwa = o2_nwa.rio.write_crs(nwa_union.crs) #assign crs
o2_nwa = o2_nwa.rio.set_spatial_dims(x_dim="lon", y_dim="lat") #get spatial dims to be named same

o2_nwa_crop = o2_nwa.rio.clip(nwa_union.geometry, nwa_union.crs, drop=True) #crop to union hull

o2_nwa_crop.name = "o2" #rename data variable to o2
o2_nwa_crop_ds = o2_nwa_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in o2_nwa_crop_ds.variables:
    o2_nwa_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_o2_nwa = {v: {"zlib": True, "complevel": 4} for v in o2_nwa_crop_ds.data_vars}

o2_nwa_crop_ds.to_netcdf("data/enviro/nwa/do/processed/o2_nwa_crop.nc", encoding = enc_o2_nwa)

#temperature
#load zarr
temp_nwa_zarr = xr.open_zarr("data/enviro/nwa/temp/raw/temp_zarr.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
temp_nwa = temp_nwa_zarr["thetao"]  # pick the variable
temp_nwa = temp_nwa.rio.write_crs(nwa_union.crs) #assign crs
temp_nwa = temp_nwa.rio.set_spatial_dims(x_dim="lon", y_dim="lat") #get spatial dims to be named same

temp_nwa_crop = temp_nwa.rio.clip(nwa_union.geometry, nwa_union.crs, drop=True) #crop to union hull

temp_nwa_crop.name = "thetao" #rename data variable to o2
temp_nwa_crop_ds = temp_nwa_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in temp_nwa_crop_ds.variables:
    temp_nwa_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_temp_nwa = {v: {"zlib": True, "complevel": 4} for v in temp_nwa_crop_ds.data_vars}

temp_nwa_crop_ds.to_netcdf("data/enviro/nwa/temp/processed/temp_nwa_crop.nc", encoding = enc_temp_nwa)

#salinity
#load zarr
sal_nwa_zarr = xr.open_zarr("data/enviro/nwa/salinity/raw/sal_zarr.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
sal_nwa = sal_nwa_zarr["so"]  # pick the variable
sal_nwa = sal_nwa.rio.write_crs(nwa_union.crs) #assign crs
sal_nwa = sal_nwa.rio.set_spatial_dims(x_dim="lon", y_dim="lat") #get spatial dims to be named same

sal_nwa_crop = sal_nwa.rio.clip(nwa_union.geometry, nwa_union.crs, drop=True) #crop to union hull

sal_nwa_crop.name = "so" #rename data variable to o2
sal_nwa_crop_ds = sal_nwa_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in sal_nwa_crop_ds.variables:
    sal_nwa_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_sal_nwa = {v: {"zlib": True, "complevel": 4} for v in sal_nwa_crop_ds.data_vars}

sal_nwa_crop_ds.to_netcdf("data/enviro/nwa/salinity/processed/sal_nwa_crop.nc", encoding = enc_sal_nwa)

#NEP
nep_union = gpd.read_file("data/enviro/nep/nep_union.gpkg") 

#oxygen
#load zarr
o2_nep_zarr = xr.open_zarr("data/enviro/nep/do/raw/o2_zarr.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
o2_nep = o2_nep_zarr["o2"]  # pick the variable

#rotate so goes from -180 -> 180
o2_nep = o2_nep.assign_coords(lon=(((o2_nep.lon + 180) % 360) - 180))
o2_nep = o2_nep.sortby("lon")

#crop
minx, miny, maxx, maxy = nep_union.total_bounds
box = o2_nep.sel(lon=slice(minx, maxx), lat=slice(miny, maxy))

mask = regionmask.mask_geopandas(nep_union, box.lon, box.lat)
o2_nep_crop = box.where(~np.isnan(mask), drop=True) 

o2_nep_crop.name = "o2" #rename data variable to o2
o2_nep_crop_ds = o2_nep_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in o2_nep_crop_ds.variables:
    o2_nep_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_o2_nep = {v: {"zlib": True, "complevel": 4} for v in o2_nep_crop_ds.data_vars}

o2_nep_crop_ds.to_netcdf("data/enviro/nep/do/processed/o2_nep_crop.nc", encoding = enc_o2_nep)

#temperature
#load zarr
temp_nep_zarr = xr.open_zarr("data/enviro/nep/temp/raw/temp_zarr.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
temp_nep = temp_nep_zarr["thetao"]  # pick the variable

#rotate so goes from -180 -> 180
temp_nep = temp_nep.assign_coords(lon=(((temp_nep.lon + 180) % 360) - 180))
temp_nep = temp_nep.sortby("lon")

#crop
minx, miny, maxx, maxy = nep_union.total_bounds
box = temp_nep.sel(lon=slice(minx, maxx), lat=slice(miny, maxy))

mask = regionmask.mask_geopandas(nep_union, box.lon, box.lat)
temp_nep_crop = box.where(~np.isnan(mask), drop=True) 

temp_nep_crop.name = "thetao" #rename data variable to o2
temp_nep_crop_ds = temp_nep_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in temp_nep_crop_ds.variables:
    temp_nep_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_temp_nep = {v: {"zlib": True, "complevel": 4} for v in temp_nep_crop_ds.data_vars}

temp_nep_crop_ds.to_netcdf("data/enviro/nep/temp/processed/temp_nep_crop.nc", encoding = enc_temp_nep)

#salinity
sal_nep_zarr = xr.open_zarr("data/enviro/nep/salinity/raw/sal_zarr.zarr", chunks = {"time":12})

# tell rioxarray which dims are spatial + what CRS the data is in
sal_nep = sal_nep_zarr["so"]  # pick the variable

#rotate so goes from -180 -> 180
sal_nep = sal_nep.assign_coords(lon=(((sal_nep.lon + 180) % 360) - 180))
sal_nep = sal_nep.sortby("lon")

#crop
minx, miny, maxx, maxy = nep_union.total_bounds
box = sal_nep.sel(lon=slice(minx, maxx), lat=slice(miny, maxy))

mask = regionmask.mask_geopandas(nep_union, box.lon, box.lat)
sal_nep_crop = box.where(~np.isnan(mask), drop=True) 

sal_nep_crop.name = "so" #rename data variable to o2
sal_nep_crop_ds = sal_nep_crop.to_dataset() #xarray to dataset

#clears encoding specific to zarr
for v in sal_nep_crop_ds.variables:
    sal_nep_crop_ds[v].encoding.clear()

# set netcdf encoding to each variable in crop dataset object
enc_sal_nep = {v: {"zlib": True, "complevel": 4} for v in sal_nep_crop_ds.data_vars}

sal_nep_crop_ds.to_netcdf("data/enviro/nep/salinity/processed/sal_nep_crop.nc", encoding = enc_sal_nep)

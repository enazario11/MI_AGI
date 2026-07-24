#libraries
import xarray as xr

#NWA
#oxygen
nc_o2 = xr.open_dataset("data/enviro/nwa/do/raw/o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
nc_o2.to_zarr("data/enviro/nwa/do/raw/o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.zarr")

#temperature
nc_temp = xr.open_dataset("data/enviro/nwa/temp/raw/thetao.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
nc_temp.to_zarr("data/enviro/nwa/temp/raw/temp_zarr.zarr")

#salinity
nc_sal = xr.open_dataset("data/enviro/nwa/salinity/raw/so.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc")
nc_sal.to_zarr("data/enviro/nwa/salinity/raw/sal_zarr.zarr")

#NEP
#oxygen
nc_o2_nep = xr.open_dataset("data/enviro/nep/do/raw/o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc")
nc_o2_nep.to_zarr("data/enviro/nep/do/raw/o2_zarr.zarr")

#temperature
nc_temp_nep = xr.open_dataset("data/enviro/nep/temp/raw/thetao.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc")
nc_temp_nep.to_zarr("data/enviro/nep/temp/raw/temp_zarr.zarr")

#salinity
nc_sal_nep = xr.open_dataset("data/enviro/nep/salinity/raw/so.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc")
nc_sal_nep.to_zarr("data/enviro/nep/salinity/raw/sal_zarr.zarr")


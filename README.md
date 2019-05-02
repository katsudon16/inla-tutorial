# Dengue Forecast in HK

## Folder structure
```
.
├── figure                  # Figure outputs (all figures are ignored except for dummy.txt)
├── dat                     # Data (excel / csv)
|   ├── cases
|   ├── climate
|   └── vector
└── src                     # Source code
    ├── lib
    ├── model
    ├── clean
    └── plot

```

## Data description

In this project, we temporarily divide HK into 3 areas:
- New Territories North (NTN): all districts in the northern island except for Kowloon and Kwai Tsing
- New Territories South (NTS): the southern island of New Territories
- Hong Kong and Kowloon (HKL): Hong Kong, Kowloon, and Kwai Tsing


```
dat
├── climate
|   ├── HKCD                     # Daily climate data obtained from stations
|   ├── HKCD_areas               # Daily climate data of the 5 areas
|   ├── HKCM                     # Monthly climate data obtained from stations
|   └── HKCM_areas               # Monthly climate data of the 5 areas
├── cases
|   ├── hk_monthly_cases         # Monthly incidence data
|   ├── hk_annual_cases          # Annual incidence data
|   ├── hk_annual_cases_district # Annual incidence data of each district
|   └── hk_annual_cases_areas    # Annual incidence data of the 5 areas
└── vector
```

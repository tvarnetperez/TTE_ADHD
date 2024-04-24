library(data.table)

# Create RDS copy of Rdata objects since they are a single object.
if (file.exists(here::here("data", "semiraw", "dt_dates.RDS")) == FALSE) {
  load(here::here("data", "semiraw", "dt_dates.Rdata"))
  saveRDS(dt.dates, here::here("data", "semiraw", "dt_dates.RDS"))
  rm(dt.dates) # Remove the loaded named .Rdata object
}

if (file.exists(here::here("data", "semiraw", "merged.RDS")) == FALSE) {
  load(here::here("data", "semiraw", "merged.Rdata"))
  saveRDS(my_data, here::here("data", "semiraw", "merged.RDS"))
  rm(my_data) # Remove the loaded named .Rdata object
}

if (file.exists(here::here("data", "semiraw", "diags_child.RDS")) == FALSE) {
  load(here::here("data", "semiraw", "Diagnosed+medicated_child4sample.Rdata"))
  saveRDS(diags.child, here::here("data", "semiraw", "diags_child.RDS"))
  rm(diags.child) # Remove the loaded named .Rdata object
}

  # Dispension dates
dt_dates_original      <- readRDS(here::here("data", "semiraw", "dt_dates.RDS"))
dt_dates <- data.table::copy(dt_dates_original) # Explicitly copy rather than assign by reference

  # Data with ids of pupils and variables of interest
dt_merged_original <- readRDS(here::here("data", "semiraw", "merged.RDS")) 
dt_merged   <- data.table::copy(dt_merged_original) 

  # Data with visits and times of different diagnoses
    # ADHD diagnoses are already trimmed at grade 6
dt_diags_original      <- readRDS(here::here("data", "semiraw", "diags_child.RDS"))
dt_diags <- data.table::copy(dt_diags_original) 

  # Data of all children ever with an ADHD diagnosis
dt_diags_all_original <- readRDS(here::here("data", "semiraw", "dates_all_with_ADHD.RDS"))
dt_diags_all <- data.table::copy(dt_diags_all_original)

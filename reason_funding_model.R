

draw <- here::here("data-raw")
dfrs <- fs::path(r"(E:/R_projects/packages/pendata/data-raw/frs)")
dfreason <- fs::path(dfrs, "from_reason")

# source(here::here("data-raw", "libraries.r"))
source(fs::path(draw, "libraries.r"))

frs_constants <- readRDS(fs::path(drds, "frs_constants.rds"))


# get input data ----------------------------------------------------------

# source(here::here("data-raw", "frs", "_common_frs.R"))


# load Reason workspace into its own environment ------------------------------
# load takes some time
wspath <- path(dfreason, "reason_workspace.RData")
load(wspath, rws <- new.env())
objnames <- ls(name=rws) # names of objects in the workspace
objnames
# get(objnames[119], envir=rws) # how to get an object from the workspace

get_object <- function(x){
  # get an object from the workspace
  get(x, envir = rws)
}

reason_classes <- frs_constants$classes |>
  str_replace("seniormanagement", "senior_management")

wf_dataframes <- readRDS(fs::path(dfreason, "wf_dataframes.rds"))
list2env(wf_dataframes, envir = .GlobalEnv)


# example code for getting objects ------------------------------------------------

wflist <- wfnames |>
  purrr::map(\(x) get_object(x))|>
  purrr::set_names(wfclasses)

str(wflist[[1]])
(wfdfnames <- wflist[[1]] |> names())

# get 4 data frames, with classes stacked
nested <- wflist |>
  as_tibble() |>
  mutate(dfname=wfdfnames) |>
  pivot_longer(cols = -dfname, names_to = "class") |>
  pivot_wider(names_from = dfname)

unpack_dframes <- function(colname){
  print(colname)
  df <- nested |>
    select(class, nestcol=all_of(colname)) |>
    unnest_wider(col=nestcol) |>
    unnest_longer(col=-class)
  assign(colname, df, envir = .GlobalEnv)
}

purrr::walk(wfdfnames, unpack_dframes)


# inputs needed for funding_model -----------------------------------------

## arguments ---
# dr_current = dr_current_,
# dr_new = dr_new_,
# cola_tier_1_active_constant = cola_tier_1_active_constant_, yes
# cola_tier_1_active = cola_tier_1_active_,
# cola_tier_2_active = cola_tier_2_active_,
# cola_tier_3_active = cola_tier_3_active_,
# cola_current_retire = cola_current_retire_,
# cola_current_retire_one = cola_current_retire_one_,
# one_time_cola = one_time_cola_,
# retire_refund_ratio = retire_refund_ratio_,
# cal_factor = cal_factor_,
# #inputs below are for the liability model
# non_special_db_new_ratio = non_special_db_new_ratio_,
# special_db_new_ratio = special_db_new_ratio_,
# #inputs below are for the funding model
# return_scen = return_scen_,
# model_return = model_return_,
# amo_period_new = amo_period_new_,
# amo_pay_growth = amo_pay_growth_,
# amo_method = amo_method_

arglist <- read_csv(
"argname
dr_current_
dr_new_
cola_tier_1_active_constant_
cola_tier_1_active_
cola_tier_2_active_
cola_tier_3_active_
")

arglist <- c("", "", "", "", "", "", "", "", "", "", "", "", "", )

## data needed in the workspace ----

## function signature ----



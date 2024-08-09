
# setup ----------------------------------------------------------

draw <- here::here("data-raw")

# Florida FRS data - from the FRS2 project, but saved in the pendata project
dfrs <- fs::path(r"(E:/R_projects/packages/pendata/data-raw/frs)")
drds <- fs::path(dfrs, "rds")
dfreason <- fs::path(dfrs, "from_reason")

# source(here::here("data-raw", "libraries.r"))
source(fs::path(draw, "libraries.r"))

frs_constants <- readRDS(fs::path(drds, "frs_constants.rds"))


# get input data ----------------------------------------------------------

# source(here::here("data-raw", "frs", "_common_frs.R"))


# load Reason FRS workspace into its OWN environment ------------------------------
# load takes some time
wspath <- path(dfreason, "reason_workspace.RData")
load(wspath, rws <- new.env())
objnames <- ls(name=rws) # names of objects in the workspace
objnames # almost 300 objects!; already sorted by name
# get(objnames[119], envir=rws) # how to get an object from the workspace

get_object <- function(x){
  # x is character string
  # get an object from the rws environment
  # it is NOT saved in the global environment - assign if desired
  get(x, envir = rws)
}

get_object("year_range_")


reason_classes <- frs_constants$classes |>
  # why do I do this next statement? maybe because object names have senior_management in them? comment!
  str_replace("seniormanagement", "senior_management") |>
  sort()
reason_classes

# get a list of workforce dataframes from pendata
wf_dataframes <- readRDS(fs::path(dfreason, "wf_dataframes.rds"))
length(wf_dataframes)
names(wf_dataframes) # wf_active_df,... term, refund, retire
list2env(wf_dataframes, envir = .GlobalEnv) # unlist them into the global environment
rm(wf_dataframes) # remove the list


# example code for getting objects ------------------------------------------------

## get the workforce data frames from the reason workspace environment (produced by FRS2) ----
# get workforce dataframe names for objects in the rws environment
(env_wfdf_names <- str_subset(objnames, "wf_data") |> str_subset("get_wf_data", negate=TRUE)) |> sort() # 7 class names

# map the rws environment names Reason used for dataframes to the names Reason used for classes
class_mapping <- tibble(
  envname=env_wfdf_names,
  classname=reason_classes)
class_mapping # inspect to be sure the names are properly aligned


# wfnames: 4 classes
# wfclasses:


wflist <- class_mapping$envname |>
  # get 7 sets of 4 objects from rws (reason workspace) active, term, refund, retire
  purrr::map(\(x) get_object(x))|> # one row per wfname
  purrr::set_names(wfclasses)
names(wflist) # 7 classes
length(wflist)

names(tmp)

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



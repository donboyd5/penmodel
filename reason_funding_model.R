
# setup ----------------------------------------------------------

draw <- here::here("data-raw")

#. directories -- Florida FRS data - from the FRS2 project, but saved in the pendata project ----
dfrs <- fs::path(r"(E:/R_projects/packages/pendata/data-raw/frs)")
drds <- fs::path(dfrs, "rds")
dfreason <- fs::path(dfrs, "from_reason") # dfreason = directory -- from reason

#. libraries ----
# source(here::here("data-raw", "libraries.r"))
source(fs::path(draw, "libraries.r"))

# . constants ----
frs_constants <- readRDS(fs::path(drds, "frs_constants.rds"))


# unpack workforce data ---------------------------------------------------
# regular_wf_data <- readRDS("regular_wf_data.rds")
# special_wf_data <- readRDS("special_wf_data.rds")
# admin_wf_data <- readRDS("admin_wf_data.rds")
# eco_wf_data <- readRDS("eco_wf_data.rds")
# eso_wf_data <- readRDS("eso_wf_data.rds")
# judges_wf_data <- readRDS("judges_wf_data.rds")
# senior_management_wf_data <- readRDS("senior_management_wf_data.rds")

wfnames <- paste0(reason_classes, "_wf_data")
wfnames # names of wf objects to extract from Reason workspace

wfclasses <- str_remove(wfnames, "_wf_data")


# get_object(wfnames[1])

# get one big list of lists of data frames
# outer list has 7 lists, one for each class
# each inner list has 4 data frames
#   wf_active_df, term, refund, retire
# they are similar but not the same in structure, but the same structure for each class
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

count(wf_refund_df, class)
count(wf_refund_df, class)

# create a list of wf data frames and save it as an rds file
wf_dataframes <- mget(wfdfnames)
saveRDS(wf_dataframes, fs::path(dfreason, "wf_dataframes.rds"))

# list2env(wf_dataframes, envir = .GlobalEnv) # put the dataframes into the environment




# get input data ----------------------------------------------------------

# source(here::here("data-raw", "frs", "_common_frs.R"))


# load Reason FRS workspace into its OWN environment ------------------------------
#   workspace was saved on 4/24/2024:
#      in my variant of Reason FRS project -- E:/R_projects/projects/Florida-FRS-main_v2/boyd_save_to_pendata.R
#      to pendata: save.image(fs::path(dfreason, "reason_workspace.RData"))

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



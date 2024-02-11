# Moral-Political-Signalling-ABM

This repository is for all the code related to our agent-based model on political identity inference and signalling under uncertainty via moral values. The model is programmed in NetLogo, but code specific to data collection is written in R. We are currently working on adding new features to the model (iteration 1.5). For additional information on the Iteration 1 model, see Pedersen and Moore (2023; https://escholarship.org/uc/item/0d143859) and the model manual (https://osf.io/gye7t/).

This repository contains:

* Archived code for the iteration 1 model and files for simulating the data used in our submission to CogSci 2023 (https://escholarship.org/uc/item/0d143859) and poster for SPSP 2024 (https://whova.com/portal/webapp/spspa_202402/Agenda/3539467; link only accessible to attendees)

* Code for the extended model under development (iteration 1.5)

* Code and files for simulating the data used for our submission to CogSci 2024 (sim pack CogSci 2024)

**Please read before accessing/using the model code**

1. You will need to download NetLogo to run the code (https://ccl.northwestern.edu/netlogo/download.shtml)

2. With vs. without data collection files: TLDR - unless you want to have all information about agents across time saved to your computer while running the model, use the without data collection file. The model consists of 2,000 agents with multiple associated values, many of which change for each iteration/time point. With data collection files have additional code in R and NetLogo to document these agent-level values for a set number of iterations and repeat simulations in .csv files. For Iteration 1.5, it also takes a set of parameter values to simulate multiple settings without manual resetting in-between. Due to the considerable number of values in these .csv files, they split simulations into 10 iterations sets and numbers them for reassembly. This also means that the data collection versions runs much slower, therefore, we highly recommend to use the without data collection versions unless you are specifically interested in conducting and documenting a set of simulations.

3. All vs. limited parameter files (Iteration 1 only): There were more flexible parameters in the model than we have varied during existing simulations. The "limited parameter" files fixes values of parameters not varied during our simulations of the Iteration 1 model (e.g., number of moral signals) while the "all parameters" files contain the full set of parameters that we currently could allow to vary.

**Good to knows**

* We are planning to transition the entire model into python code

* The existing data collection code runs on tidyverse syntax, which we recently discovered was ill-suited for the size of the data frames. If you are interested in simulating data, consider rewriting this into the syntax of the data.table package and it should run more efficiently (https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html).

* Iteration 1.5 is beginning to incorporate media influences into agents' moral decision making
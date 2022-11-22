using Queryverse
using Statistics
using GLM
using Pipe: @pipe

##################################################################
## Functional Form - Maximize the following for each household: ##
##################################################################
# V = X*α_x + α_i*INC + α_p*hPRICE + α_d*DIST + α_a*ACCESS + γ + ϵ
# Where any α_k = α0 + α1_k*INC + α2_k*uni + α3_k*minority
##################################################################

# Load structural data
hprice = CSV.read("data/zhvi_zip.csv", DataFrames)
income = CSV.read("data/median_income.csv", DataFrames)
#dist = Load Distance Var
#access = Load Access Var





B = 25 #250 ### num bootstrap samples #25 #250
# HYPERPARAM_COMBO_IDX = 1

######################################
### G == num games
### N == num plays per game
### K == num plays to keep per game
######################################

if (HYPERPARAM_COMBO_IDX == 1) {
  ### same hyperparams as observed data
  G = 4101
  N = 53
  K = N
} else if (HYPERPARAM_COMBO_IDX == 2) {
  ### same number of plays as observed data, but now each play is independent
  G = 4101*53
  N = 53
  K = 1
} else if (HYPERPARAM_COMBO_IDX == 3) {
  ### pretend we have 5x as many games as our current dataset
  G = 4101*5
  N = 53
  K = N
} 

print(paste0("$$$$$ hyperparam combo #", HYPERPARAM_COMBO_IDX, ": G=",G,", N=",N, ", K=",K, " $$$$$"))

################################################################################
L = 4 # (EVEN NUMBER) number of yardline bins {0=oTD, 1,2,...,L-1, L=TD}, set so that the mean # epochs per game == actual mean # epochs per game (11.6)
SI = 1 # scoring increment (touchdown)
MIDFIELD = L/2 # midfield field position x = L/2;  L needs to be an even number
MAX_TD_SURPLUS = round(N/MIDFIELD-1) # (nearly) the max possible score
I = G*N # total number of plays in simulation
# G = 6; N = 10; L = 4; SI = 7; MAX_TD_SURPLUS = 3; I = G*N; # for testing




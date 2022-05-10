
import numpy as np
import blotto

# First I chose to set 2 battalions on the first field, 32 on the next three and 1 battalions om the last two and random placing of the battalions.This gave winning 0.8. I then 
# removed the random line in the function which gave me score of 1. This will be my strategi.

def player_strategy(n_battalions,n_fields):
    #defining the array:
    battalions=np.zeros(n_fields,dtype=int)
    
    #assigning 25 battalions to the first four battle fields:
    battalions[0:1]=2
    battalions[1:4]=32
    battalions[4:]=1

  
    #asserting that all and no more than all battalions are used:
    assert sum(battalions)==n_battalions
    
    return battalions

# making a strategi for the computer:
def computer_strategy(n_battalions,n_fields):
    battalions=np.zeros(n_fields,dtype=int)
    battalions[0:1]=8
    battalions[1:4]=30
    battalions[4:]=1 
    assert sum(battalions)==n_battalions 
    return battalions


blotto.run(6,100,player_strategy,computer_strategy) 

# testing the strategies:

def call_battle(n_battalions,n_fields, player_strategy, computer_strategy):
    c_battlions=computer_strategy(n_battalions,n_fields)
    p_battlions=player_strategy(n_battalions,n_fields)

    diff=p_battlions-c_battlions
    points=sum(diff>0)-sum(diff<0)
 
    return int(points>0)-int(points<0)

def test_strategies(n_fields,n_battalions,player_strategy, computer_strategy):
    n_tests=100000
    r=0
    record=[]
    for i in range(n_tests):
        p=call_battle(n_battalions,n_fields,
            player_strategy, computer_strategy)
        record.append(p)
        r+=p
    return r/n_tests

r=test_strategies(6,100,player_strategy,computer_strategy)
print(r)
 

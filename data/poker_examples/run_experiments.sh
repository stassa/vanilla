#!/bin/bash
# Currently the two sets of experiments below must be run with a manual setting
# in poker_configuration.pl.

# CSV and log file timestamp. Common to all experiments in a batch to help
# identifie them later. Useful when tryin got understand how a change in an
# experiments' configs affects each experiment.
#
ts=$(date +%d-%m-%Y_%H-%M-%S)
# CSV and log file prefix
tp="test_protocol_"
# Experiment iterations.
reps=10

# GFG low uncertainty regime experiments
#
# In lib/poker/poker_configuration.pl make sure to set the following option (and
# unset all other experiment_file/2 options):
#
# experiment_file(data('poker_examples/experiment_script_weak_cgnf.pl'),exp_script_wcgnf).
#
csv="output/${tp}anbn_range_${ts}.csv"
log="output/${tp}anbn_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,anbn_range($reps,'$csv',false),'$log'))." -t halt &
## Sleep two seconds to give logging the chance to create a new log file.
sleep 2

csv="output/${tp}anbm_range_${ts}.csv"
log="output/${tp}anbm_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,anbm_range($reps,'$csv',false),'$log'))." -t halt &
sleep 2

csv="output/${tp}parens_range_${ts}.csv"
log="output/${tp}parens_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,parens_range($reps,'$csv',false),'$log'))." -t halt &
sleep 2

csv="output/${tp}palindrome_range_${ts}.csv"
log="output/${tp}palindrome_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,palindrome_range($reps,'$csv',false),'$log'))." -t halt &
sleep 2

## L-System moderate uncertainty regime experiments
## Uncomment this block and comment the one above to run this set of experiments.
##
## In lib/poker/poker_configuration.pl make sure to set the following option (and
## unset all other experiment_file/2 options):
##
## experiment_file(data('poker_examples/experiment_script_lnf.pl'),exp_script_lnf).
##
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_dragon_to_hilbert_curve_range($reps,'output/test_protocol_dragon_hilbert.csv'),'output/test_protocol_dragon_hilbert.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_hilbert_to_dragon_curve_range($reps,'output/test_protocol_hilbert_dragon.csv'),'output/test_protocol_hilbert_dragon.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_dragon_curve_range($reps,'output/test_protocol_koch_dragon.csv'),'output/test_protocol_koch_dragon.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_hilbert_curve_range($reps,'output/test_protocol_koch_hilbert.csv'),'output/test_protocol_koch_hilbert.log'))." -t halt &
#sleep 2

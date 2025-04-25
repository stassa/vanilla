#!/bin/bash
# Currently the two sets of experiments below must be run with a manual setting
# in poker_configuration.pl.

# CSV and log file timestamp. Common to all experiments in a batch to help
# identifie them later. Useful when tryin got understand how a change in an
# experiments' configs affects each experiment.
#
ts=$(date +%d-%m-%Y_%H-%M-%S)
# CSV and log file prefix
tp="output/test_protocol_"
# Experiment iterations.
reps=10

# GFG low uncertainty regime experiments
#
# In lib/poker/poker_configuration.pl make sure to set the following option (and
# unset all other experiment_file/2 options):
#
# experiment_file(data('poker_examples/experiment_script_weak_cgnf.pl'),exp_script_wcgnf).
#
csv="${tp}anbn_range_${ts}.csv"
log="${tp}anbn_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,anbn_range($reps,'$csv',false),'$log'))." -t halt &
## Sleep two seconds to give logging the chance to create a new log file.
sleep 2

csv="${tp}anbm_range_${ts}.csv"
log="${tp}anbm_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,anbm_range($reps,'$csv',false),'$log'))." -t halt &
sleep 2

csv="${tp}parens_range_${ts}.csv"
log="${tp}parens_range_${ts}.log"
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/2,parens_range($reps,'$csv',false),'$log'))." -t halt &
sleep 2

csv="${tp}palindrome_range_${ts}.csv"
log="${tp}palindrome_range_${ts}.log"
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
#csv="${tp}dragon_to_hilbert_curve_range_${ts}.csv"
#log="${tp}dragon_to_hilbert_curve_range_${ts}.log"
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,dragon_to_hilbert_curve_range($reps,'$csv',false),'$log'))." -t halt &
#sleep 2
#
#csv="${tp}hilbert_to_dragon_curve_range_${ts}.csv"
#log="${tp}hilbert_to_dragon_curve_range_${ts}.log"
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,hilbert_to_dragon_curve_range($reps,'$csv',false),'$log'))." -t halt &
#sleep 2
#
#csv="${tp}koch_dragon_curve_range_${ts}.csv"
#log="${tp}koch_dragon_curve_range_${ts}.log"
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,koch_to_dragon_curve_range($reps,'$csv',false),'$log'))." -t halt &
#sleep 2
#
#csv="${tp}koch_to_hilbert_curve_range_${ts}.csv"
#log="${tp}koch_to_hilbert_curve_range_${ts}.log"
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,koch_to_hilbert_curve_range($reps,'$csv',false),'$log'))." -t halt &
#sleep 2

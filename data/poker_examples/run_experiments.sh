# Currently the two sets of experiments below must be run with a manual setting
# in poker_configuration.pl.

# GFG low uncertainty regime experiments
#
# In lib/poker/poker_configuration.pl make sure to set the following option (and
# unset all other experiment_file/2 options):
#
# experiment_file(data('poker_examples/experiment_script_weak_cgnf.pl'),exp_script_wcgnf).
#
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,anbn_range(10,user_output),'output/test_protocol_anbn_range.log'))." -t halt &
# Sleep two seconds to give logging the chance to create a new log file.
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,anbm_range(10,user_output),'output/test_protocol_anbm_range.log'))." -t halt &
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,parens_range(10,user_output),'output/test_protocol_parens_range.log'))." -t halt &
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,palindrome_range(10,user_output),'output/test_protocol_palindrome_range.log'))." -t halt &
sleep 2

## L-System moderate uncertainty regime experiments
## Uncomment this block and comment the one above to run this set of experiments.
##
## In lib/poker/poker_configuration.pl make sure to set the following option (and
## unset all other experiment_file/2 options):
##
## experiment_file(data('poker_examples/experiment_script_lnf.pl'),exp_script_lnf).
##
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_dragon_to_hilbert_curve_range(10,user_output),'output/test_protocol_dragon_hilbert.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_hilbert_to_dragon_curve_range(10,user_output),'output/test_protocol_hilbert_dragon.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_dragon_curve_range(10,user_output),'output/test_protocol_koch_dragon.log'))." -t halt &
#sleep 2
#swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_hilbert_curve_range(10,user_output),'output/test_protocol_koch_hilbert.log'))." -t halt &
#sleep 2

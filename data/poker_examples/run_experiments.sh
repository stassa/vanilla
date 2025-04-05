cd ../../
pwd
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_dragon_to_hilbert_curve_range(10,user_output),'test_protocol_dragon_hilbert.log'))." -t halt &
# Sleep two seconds to give logging the chance to create a new log file.
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_hilbert_to_dragon_curve_range(10,user_output),'test_protocol_hilbert_dragon.log'))." -t halt &
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_dragon_curve_range(10,user_output),'test_protocol_koch_dragon.log'))." -t halt &
sleep 2
swipl -g "([load_headless], experiment_output:run_experiment_protocol(s/3,test_koch_to_hilbert_curve_range(10,user_output),'test_protocol_koch_hilbert.log'))." -t halt &
sleep 2

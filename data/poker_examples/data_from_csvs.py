import csv

# experiment: string, the name of the experiment
# path: string, path to data CSV
# experiment_sets: string, 'generated' or 'unlabelled', incremented in experiment sets
# debug: boolean, whereas to debug CSV data to output or not.
def csv_data(experiment,path,experiment_sets='generated',debug=True):

    iterations = []
    generated = []
    labelled = []
    unlabelled = []
    labellingAcc = []
    labellingTPR = []
    labellingTNR = []
    programAcc = []
    programTPR = []
    programTNR = []

    labellingAccSE = []
    labellingTPRSE = []
    labellingTNRSE = []
    programAccSE = []
    programTPRSE = []
    programTNRSE = []

    # Data munging munge munge munge
    with open(path, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:

            iteration = int(row['Iteration'])

            gen = int(row['Generated'])
            ini = int(row['Labelled'])

            unl = int(row['Unlabelled'])

            accl = float(row['LabAccM'])
            tprl = float(row['LabTPRM'])
            tnrl = float(row['LabTNRM'])

            accp = float(row['ProgAccM'])
            tprp = float(row['ProgTPRM'])
            tnrp = float(row['ProgTNRM'])

            acclSe = float(row['LabAccSE'])
            tprlSe = float(row['LabTPRSE'])
            tnrlSe = float(row['LabTNRSE'])

            accpSe = float(row['ProgAccSE'])
            tprpSe = float(row['ProgTPRSE'])
            tnrpSe = float(row['ProgTNRSE'])

            iterations.append(iteration)

            generated.append(gen)
            labelled.append(ini)

            unlabelled.append(unl)

            labellingAcc.append(accl)
            labellingTPR.append(tprl)
            labellingTNR.append(tnrl)

            programAcc.append(accp)
            programTPR.append(tprp)
            programTNR.append(tnrp)

            labellingAccSE.append(acclSe)
            labellingTPRSE.append(tprlSe)
            labellingTNRSE.append(tnrlSe)

            programAccSE.append(accpSe)
            programTPRSE.append(tprpSe)
            programTNRSE.append(tnrpSe)

    max_iteration = max(iterations)
    num_iterations = len(iterations)

    if (debug):

        # Debugging
        print("iterations: ",iterations)
        print("max_iteration: ",max_iteration)
        print("generated: ",generated)
        print("labelled: ",labelled)
        print("unlabelled: ",unlabelled)
        print("labellingAcc: ",labellingAcc)
        print("labellingTPR: ",labellingTPR)
        print("labellingTNR: ",labellingTNR)
        print("programAcc: ",programAcc)
        print("programTPR: ",programTPR)
        print("programTNR: ",programTNR)

        if experiment_sets == 'generated':
            print(f'experiment_sets: {experiment_sets}')
            print("generated:", generated[::max_iteration])
            print("labelled:", labelled[0:max_iteration])
            print("unlabelled:", unlabelled[0:max_iteration])
        elif experiment_sets == 'unlabelled':
            print(f'experiment_sets: {experiment_sets}')
            print("generated:", generated[0:max_iteration])
            print("labelled:", labelled[0:max_iteration])
            print("unlabelled:", unlabelled[0:max_iteration])
        else:
            print(f'experiment_sets: {experiment_sets}')

        for i in range(0,num_iterations,max_iteration):
            j = i + max_iteration
            print('i: ',str(i),'j: ',str(j))
            print("labellingAcc:", labellingAcc[i:j])
            print("labellingTPR:", labellingTPR[i:j])
            print("labellingTNR:", labellingTNR[i:j])
            print("programAcc:", programAcc[i:j])
            print("programTPR:", programTPR[i:j])
            print("programTNR:", programTNR[i:j])

            print("labellingAccSE:", labellingAccSE[i:j])
            print("labellingTPRSE:", labellingTPRSE[i:j])
            print("labellingTNRSE:", labellingTNRSE[i:j])
            print("programAccSE:", programAccSE[i:j])
            print("programTPRSE:", programTPRSE[i:j])
            print("programTNRSE:", programTNRSE[i:j])

    data = {'experiment_sets': experiment_sets
            ,'experiment': experiment
            ,'iterations': iterations
            ,'generated': generated
            ,'labelled': labelled
            ,'unlabelled': unlabelled 
            ,'labellingAcc': labellingAcc
            ,'labellingTPR': labellingTPR
            ,'labellingTNR': labellingTNR
            ,'programAcc': programAcc
            ,'programTPR': programTPR
            ,'programTNR': programTNR
            ,'labellingAccSE': labellingAccSE
            ,'labellingTPRSE': labellingTPRSE
            ,'labellingTNRSE': labellingTNRSE
            ,'programAccSE': programAccSE
            ,'programTPRSE': programTPRSE
            ,'programTNRSE': programTNRSE
            ,'max_iteration': max_iteration
            ,'num_iterations': num_iterations
            }

    return data

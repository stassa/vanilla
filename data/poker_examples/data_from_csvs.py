import csv


def csv_data(experiment,path,debug=True,has_unlabelled=False):

    iterations = []
    generated = []
    initial = []
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

    unl = ''

    # Data munging munge munge munge
    with open(path, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:

            iteration = int(row['Iteration'])

            gen = int(row['Generated'])
            ini = int(row['Labelled'])

            if has_unlabelled:
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
            initial.append(ini)

            if has_unlabelled:
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
    experiment_sets = len(iterations)

    #print(np.shape(labellingAcc))
    #print(np.shape(labellingAccSE))

    if (debug):

        # Debugging
        print("iterations: ",iterations)
        print("generated: ",generated)
        print("initial: ",initial)
        print("unlabelled: ",unlabelled)
        print("labellingAcc: ",labellingAcc)
        print("labellingTPR: ",labellingTPR)
        print("labellingTNR: ",labellingTNR)
        print("programAcc: ",programAcc)
        print("programTPR: ",programTPR)
        print("programTNR: ",programTNR)

        print("generated:", generated[::max_iteration])
        print("initial:", initial[0:max_iteration])
        print("unlabelled:", unlabelled[0:max_iteration])

        for i in range(0,experiment_sets,max_iteration):
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

    data = {'experiment': experiment
            ,'iterations': iterations
            ,'generated': generated
            ,'initial': initial
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
            ,'experiment_sets': experiment_sets
            }

    return data

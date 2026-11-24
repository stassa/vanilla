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

    programLength = []
    programLengthSE = []
    generatorAcc = []
    generatorAccSE = []

    labellingAcc = []
    labellingErr = []
    labellingTPR = []
    labellingTNR = []
    labellingFPR = []
    labellingFNR = []
    labellingPRE = []
    labellingREC = []
    labellingFSC = []

    labellingAccSE = []
    labellingErrSE = []
    labellingTPRSE = []
    labellingTNRSE = []
    labellingFPRSE = []
    labellingFNRSE = []
    labellingPRESE = []
    labellingRECSE = []
    labellingFSCSE = []

    programAcc = []
    programErr = []
    programTPR = []
    programTNR = []
    programFPR = []
    programFNR = []
    programPRE = []
    programREC = []
    programFSC = []

    programAccSE = []
    programErrSE = []
    programTPRSE = []
    programTNRSE = []
    programFPRSE = []
    programFNRSE = []
    programPRESE = []
    programRECSE = []
    programFSCSE = []

    # Data munging munge munge munge
    with open(path, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:

            iterations.append( int(row['Iteration']) )

            generated.append( int(row['Generated']) )
            labelled.append( int(row['Labelled']) )
            unlabelled.append( int(row['Unlabelled']) )

            programLength.append( float(row['LengthM']) )
            programLengthSE.append( float(row['LengthSE']) )

            generatorAcc.append( float(row['GenM']) )
            generatorAccSE.append( float(row['GenSE']) )

            labellingAcc.append( float(row['LabAccM']) )
            labellingErr.append( float(row['LabErrM']) )
            labellingTPR.append( float(row['LabTPRM']) )
            labellingTNR.append( float(row['LabTNRM']) )
            labellingFPR.append( float(row['LabFPRM']) )
            labellingFNR.append( float(row['LabFNRM']) )
            labellingPRE.append( float(row['LabPREM']) )
            labellingREC.append( float(row['LabRECM']) )
            labellingFSC.append( float(row['LabFSCM']) )

            labellingAccSE.append( float(row['LabAccSE']) )
            labellingErrSE.append( float(row['LabErrSE']) )
            labellingTPRSE.append( float(row['LabTPRSE']) )
            labellingTNRSE.append( float(row['LabTNRSE']) )
            labellingFPRSE.append( float(row['LabFPRSE']) )
            labellingFNRSE.append( float(row['LabFNRSE']) )
            labellingPRESE.append( float(row['LabPRESE']) )
            labellingRECSE.append( float(row['LabRECSE']) )
            labellingFSCSE.append( float(row['LabFSCSE']) )

            programAcc.append( float(row['ProgAccM']) )
            programErr.append( float(row['ProgErrM']) )
            programTPR.append( float(row['ProgTPRM']) )
            programTNR.append( float(row['ProgTNRM']) )
            programFPR.append( float(row['ProgFPRM']) )
            programFNR.append( float(row['ProgFNRM']) )
            programPRE.append( float(row['ProgPREM']) )
            programREC.append( float(row['ProgRECM']) )
            programFSC.append( float(row['ProgFSCM']) )

            programAccSE.append( float(row['ProgAccSE']) ) 
            programErrSE.append( float(row['ProgErrSE']) )
            programTPRSE.append( float(row['ProgTPRSE']) )
            programTNRSE.append( float(row['ProgTNRSE']) )
            programFPRSE.append( float(row['ProgFPRSE']) )
            programFNRSE.append( float(row['ProgFNRSE']) )
            programPRESE.append( float(row['ProgPRESE']) )
            programRECSE.append( float(row['ProgRECSE']) )
            programFSCSE.append( float(row['ProgFSCSE']) )

    max_iteration = max(iterations)
    num_iterations = len(iterations)

    if (debug):

        # Debugging
        print("experiment: ",experiment)
        print("iterations: ",iterations)
        print("max_iteration: ",max_iteration)

        print("generated: ",generated)
        print("labelled: ",labelled)
        print("unlabelled: ",unlabelled)

        print("programLength: ",programLength)
        print("programLengthSE: ",programLengthSE)
        print("generatorAcc: ",generatorAcc)
        print("generatorAccSE: ",generatorAccSE)

        print("labellingAcc: ",labellingAcc)
        print("labellingErr: ",labellingErr)
        print("labellingTPR: ",labellingTPR)
        print("labellingTNR: ",labellingTNR)
        print("labellingFPR: ",labellingFPR)
        print("labellingFNR: ",labellingFNR)
        print("labellingPRE: ",labellingPRE)
        print("labellingREC: ",labellingREC)
        print("labellingFSC: ",labellingFSC)

        print("labellingAccSE: ",labellingAccSE)
        print("labellingErrSE: ",labellingErrSE)
        print("labellingTPRSE: ",labellingTPRSE)
        print("labellingTNRSE: ",labellingTNRSE)
        print("labellingFPRSE: ",labellingFPRSE)
        print("labellingFNRSE: ",labellingFNRSE)
        print("labellingPRESE: ",labellingPRESE)
        print("labellingRECSE: ",labellingRECSE)
        print("labellingFSCSE: ",labellingFSCSE)

        print("programAcc: ",programAcc)
        print("programErr: ",programErr)
        print("programTPR: ",programTPR)
        print("programTNR: ",programTNR)
        print("programFPR: ",programFPR)
        print("programFNR: ",programFNR)
        print("programPRE: ",programPRE)
        print("programREC: ",programREC)
        print("programFSC: ",programFSC)

        print("programAccSE: ",programAccSE)
        print("programErrSE: ",programErrSE)
        print("programTPRSE: ",programTPRSE)
        print("programTNRSE: ",programTNRSE)
        print("programFPRSE: ",programFPRSE)
        print("programFNRSE: ",programFNRSE)
        print("programPRESE: ",programPRESE)
        print("programRECSE: ",programRECSE)
        print("programFSCSE: ",programFSCSE)

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

            print("programLength:", programLength[i:j])
            print("programLengthSE:", programLengthSE[i:j])
            print("generatorAcc:", generatorAcc[i:j])
            print("generatorAccSE:", generatorAccSE[i:j])

            print("labellingAcc:", labellingAcc[i:j])
            print("labellingErr:", labellingErr[i:j])
            print("labellingTPR:", labellingTPR[i:j])
            print("labellingTNR:", labellingTNR[i:j])
            print("labellingFPR:", labellingFPR[i:j])
            print("labellingFNR:", labellingFNR[i:j])
            print("labellingPRE:", labellingPRE[i:j])
            print("labellingREC:", labellingREC[i:j])
            print("labellingFSC:", labellingFSC[i:j])

            print("labellingAccSE:", labellingAccSE[i:j])
            print("labellingErrSE:", labellingErrSE[i:j])
            print("labellingTPRSE:", labellingTPRSE[i:j])
            print("labellingTNRSE:", labellingTNRSE[i:j])
            print("labellingFPRSE:", labellingFPRSE[i:j])
            print("labellingFNRSE:", labellingFNRSE[i:j])
            print("labellingPRESE:", labellingPRESE[i:j])
            print("labellingRECSE:", labellingRECSE[i:j])
            print("labellingFSCSE:", labellingFSCSE[i:j])

            print("programAcc:", programAcc[i:j])
            print("programErr:", programErr[i:j])
            print("programTPR:", programTPR[i:j])
            print("programTNR:", programTNR[i:j])
            print("programFPR:", programFPR[i:j])
            print("programFNR:", programFNR[i:j])
            print("programPRE:", programPRE[i:j])
            print("programREC:", programREC[i:j])
            print("programFSC:", programFSC[i:j])

            print("programAccSE:", programAccSE[i:j])
            print("programErrSE:", programErrSE[i:j])
            print("programTPRSE:", programTPRSE[i:j])
            print("programTNRSE:", programTNRSE[i:j])
            print("programFPRSE:", programFPRSE[i:j])
            print("programFNRSE:", programFNRSE[i:j])
            print("programPRESE:", programPRESE[i:j])
            print("programRECSE:", programRECSE[i:j])
            print("programFSCSE:", programFSCSE[i:j])

    data = {'experiment_sets': experiment_sets
            ,'experiment': experiment
            ,'max_iteration': max_iteration
            ,'num_iterations': num_iterations

            ,'iterations': iterations
            ,'generated': generated
            ,'labelled': labelled
            ,'unlabelled': unlabelled 

            ,'programLength': programLength
            ,'programLengthSE': programLengthSE
            ,'generatorAcc': generatorAcc
            ,'generatorAccSE': generatorAccSE

            ,'labellingAcc': labellingAcc
            ,'labellingErr': labellingErr
            ,'labellingTPR': labellingTPR
            ,'labellingTNR': labellingTNR
            ,'labellingFPR': labellingFPR
            ,'labellingFNR': labellingFNR
            ,'labellingPRE': labellingPRE
            ,'labellingREC': labellingREC
            ,'labellingFSC': labellingFSC

            ,'labellingAccSE': labellingAccSE
            ,'labellingErrSE': labellingErrSE
            ,'labellingTPRSE': labellingTPRSE
            ,'labellingTNRSE': labellingTNRSE
            ,'labellingFPRSE': labellingFPRSE
            ,'labellingFNRSE': labellingFNRSE
            ,'labellingPRESE': labellingPRESE
            ,'labellingRECSE': labellingRECSE
            ,'labellingFSCSE': labellingFSCSE

            ,'programAcc': programAcc
            ,'programErr': programErr
            ,'programTPR': programTPR
            ,'programTNR': programTNR
            ,'programFPR': programFPR
            ,'programFNR': programFNR
            ,'programPRE': programPRE
            ,'programREC': programREC
            ,'programFSC': programFSC

            ,'programAccSE': programAccSE
            ,'programErrSE': programErrSE
            ,'programTPRSE': programTPRSE
            ,'programTNRSE': programTNRSE
            ,'programFPRSE': programFPRSE
            ,'programFNRSE': programFNRSE
            ,'programPRESE': programPRESE
            ,'programRECSE': programRECSE
            ,'programFSCSE': programFSCSE
            }

    return data

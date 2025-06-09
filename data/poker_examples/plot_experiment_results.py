import data_from_csvs as csv
import numpy as np
import matplotlib.pyplot as plt
import itertools
from matplotlib.lines import Line2D
import cmasher as cmr

# family 'normal' is not recognised?
font = {#'family' : 'normal',
        #'weight' : 'bold',
        'size'   : 12}

plt.rc('font', **font)

plt.rc('lines',linewidth = 3.0)
plt.rc('lines',markersize = 8)

plt.rc('legend',fontsize = 19)

style = 'seaborn-v0_8-colorblind'

plt.style.use(style)

#rows, cols
fig, axs = plt.subplots(3,7,constrained_layout=True)
# width, height
fig.set_size_inches(25,8)

# experiment: string, the name of the experiment
# path: string, path to data csv
# experiment_sets: string, 'generated' or 'unlabelled', incremented in experiment sets
# debug: boolean, whereas to debug CSV data to output or not.
def plot_data(experiment,path,experiment_sets='generated',debug=False):

    data = csv.csv_data(experiment,path,experiment_sets,debug)

    num_iterations = data['num_iterations']
    max_iteration = data['max_iteration']
    generated = data['generated']
    unlabelled = data['unlabelled']
    labelled = data['labelled']

    programLength = data['programLength']
    programLengthSE = data['programLengthSE']
    generatorAcc = data['generatorAcc']
    generatorAccSE = data['generatorAccSE']

    labellingAcc = data['labellingAcc']
    labellingErr = data['labellingErr']
    labellingTPR = data['labellingTPR']
    labellingTNR = data['labellingTNR']
    labellingFPR = data['labellingFPR']
    labellingFNR = data['labellingFNR']
    labellingPRE = data['labellingPRE']
    labellingREC = data['labellingREC']
    labellingFSC = data['labellingFSC']

    labellingAccSE = data['labellingAccSE']
    labellingErrSE = data['labellingErrSE']
    labellingTPRSE = data['labellingTPRSE']
    labellingTNRSE = data['labellingTNRSE']
    labellingFPRSE = data['labellingFPRSE']
    labellingFNRSE = data['labellingFNRSE']
    labellingPRESE = data['labellingPRESE']
    labellingRECSE = data['labellingRECSE']
    labellingFSCSE = data['labellingFSCSE']

    programAcc = data['programAcc']
    programErr = data['programErr']
    programTPR = data['programTPR']
    programTNR = data['programTNR']
    programFPR = data['programFPR']
    programFNR = data['programFNR']
    programPRE = data['programPRE']
    programREC = data['programREC']
    programFSC = data['programFSC']

    programAccSE = data['programAccSE']
    programErrSE = data['programErrSE']
    programTPRSE = data['programTPRSE']
    programTNRSE = data['programTNRSE']
    programFPRSE = data['programFPRSE']
    programFNRSE = data['programFNRSE']
    programPRESE = data['programPRESE']
    programRECSE = data['programRECSE']
    programFSCSE = data['programFSCSE']

    # Data structures used to set colours, linestyles and markers for labels to be used in a custom legend.
    # Setup colour ranges. Still trying things here.
    #cmap = plt.cm.jet
    #cmap = plt.cm.managua
    #cmap = cmr.ocean
    #cmap = cmr.rainforest
    cmap = cmr.pepper
    #cmap = cmr.bubblegum
    #cmap = cmr.gem
    #colors = cmap(np.linspace(0.3, 0.7, max_iteration))
    colors = cmap(np.linspace(0.1, 0.7, max_iteration))
    #colors = cmap(np.linspace(0, 1, max_iteration))
    palette = itertools.cycle(colors)
    range_colors = {}

    # Setup marker ranges
    markers_ = ['o','v','^','<','>',8,'s','p','*','D','d','P','X','H']
    markers = itertools.cycle(markers_)
    range_markers = {}

    # Setup linestyle ranges
    linestyles_ = ['-'
                  ,':'
                  ,'--'
                  ,'-.'
                  ,(0,(1,5))
                  ,(0,(1,1))
                  ,(5,(10,3))
                  ,(0,(5,5))
                  ,(0,(3,1,1,1))
                  ,(0,(3,5,1,5,1,5))]
    linestyles = itertools.cycle(linestyles_)
    range_linestyles = {}

    # Stores plots to get labels later.
    plots = []

    row = 0
    col = 0

    xticks = labelled[0:max_iteration]
    xlabels = labelled[0:max_iteration]
    xlabel = 'Labelled Examples'

    axs[row,col].set_title("Labelling")
    axs[row,col+1].set_title("Hypothesis")
    axs[row,col+2].set_title("Labelling")
    axs[row,col+3].set_title("Hypothesis")
    axs[row,col+4].set_title("Labelling")
    axs[row,col+5].set_title("Hypothesis")

    axs[row,col+6].set_title("Generator")
    axs[row+1,col+6].set_title("Hypothesis Size")
    axs[row+2,col+6].set_title("Hypothesis Size (k > 0)")

    #   0   2   4   6
    # 0 Acc PRE REC Gen
    # 1 Err TPR TNR PLn
    # 2 FSC FPR FNR PL1

    contents = {'ACC': (0,0,'Accuracy',labellingAcc,labellingAccSE,programAcc,programAccSE)
               ,'PRE': (0,2,'Precision',labellingPRE,labellingPRESE,programPRE,programPRESE)
               ,'REC': (0,4,'Recall',labellingREC,labellingRECSE,programREC,programRECSE)
               ,'Gen': (0,6,'Generative Acc.',generatorAcc,generatorAccSE)
               ,'ERR': (1,0,'Error',labellingErr,labellingErrSE,programErr,programErrSE)
               ,'TPR': (1,2,'TPR',labellingTPR,labellingTPRSE,programTPR,programTPRSE)
               ,'TNR': (1,4,'TNR',labellingTNR,labellingTNRSE,programTNR,programTNRSE)
               ,'HLN': (1,6,'Hyp. Size',programLength,programLengthSE)
               ,'FSC': (2,0,'F1-Score',labellingFSC,labellingFSCSE,programFSC,programFSCSE)
               ,'FPR': (2,2,'FPR',labellingFPR,labellingFPRSE,programFPR,programFPRSE)
               ,'FNR': (2,4,'FNR',labellingFNR,labellingTNRSE,programTNR,programTNRSE)
               ,'HL1': (2,6,'Hyp. Size 1+',programLength,programLengthSE)
               }

    #face_colour = 'xkcd:eggshell'
    face_colour = 'xkcd:light grey'
    #face_colour = 'xkcd:silver'
    #face_colour = 'xkcd:very light pink'

    for i in range(0,num_iterations,max_iteration):
        j = i + max_iteration

        lab = ''

        if experiment_sets == 'generated':
            lab = str(generated[i])
        elif experiment_sets == 'unlabelled':
            lab = str(unlabelled[i])

        # Add a new colour to the range if it doesn't exist.
        if not lab in range_colors:
            range_colors[lab] = next(palette)
        color = range_colors[lab]

        if not lab in range_markers:
            range_markers[lab] = next(markers)
        marker = range_markers[lab]

        if not lab in range_linestyles:
            range_linestyles[lab] = next(linestyles)
        linestyle = range_linestyles[lab]

        for k,v in contents.items():

            row = v[0]
            col = v[1]

            if k not in ['Gen','HLN','HL1']:
                axs[v[0], v[1]].set_ylabel(v[2])

                plot = axs[row,col].errorbar(labelled[0:max_iteration],v[3][i:j],yerr=v[4][i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
                axs[row,col].grid(visible=True, which='major', axis='both')
                axs[row,col].set_xticks(xticks, labels=xlabels)

                axs[row,col].set_facecolor(face_colour)

                axs[row,col+1].errorbar(labelled[0:max_iteration],v[5][i:j],yerr=v[6][i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
                axs[row,col+1].grid(visible=True, which='major', axis='both')
                axs[row,col+1].set_xticks(xticks, labels=xlabels)

                axs[row,col+1].set_facecolor(face_colour)

                if k == 'ACC':
                    plots.append(plot)

            if k in ['Gen','HLN']:

                plot = axs[row,col].errorbar(labelled[0:max_iteration],v[3][i:j],yerr=v[4][i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
                axs[row,col].grid(visible=True, which='major', axis='both') 
                axs[row,col].set_xticks(xticks, labels=xlabels)

                axs[row,col].set_facecolor(face_colour)

            if k == 'HL1':

                if i > 0:
                    #axs[row+1,col+2].set_yscale('symlog')
                    axs[row,col].errorbar(labelled[0:max_iteration],v[3][i:j],yerr=v[4][i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
                    axs[row,col].grid(visible=True, which='major', axis='both')  
                    axs[row,col].set_xticks(xticks, labels=xlabels)

    fig.align_labels() 
    fig.align_titles()

    fig.supxlabel(xlabel)
    fig.suptitle(experiment)

    # Get the labels added to the plot.
    # I could just have a labels dict and add them there though.
    labels = [int(p.get_label()) for p in plots]
    # Then I wouldn't need to sort either.
    labels = sorted(set(labels))

    # List of new lines to add to legend.
    legend_elements = []

    for lab in labels:
        slab = str(lab)
        color = range_colors[slab]
        marker = range_markers[slab]
        linestyle = range_linestyles[slab]
        line = Line2D([0], [0], color=color, marker=marker, linestyle=linestyle, label=slab)
        legend_elements.append(line)

    if experiment_sets == 'generated':
        title = 'Generated'
    elif experiment_sets == 'unlabelled':
        title = 'Unlabelled'

    fig.legend(handles=legend_elements,loc='outside right center',fontsize='medium',title=f'{title}\nexamples',title_fontsize='medium')

    #plt.savefig(f'output/figures/{experiment}.png')

    plt.show()

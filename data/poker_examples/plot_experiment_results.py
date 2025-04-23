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

plt.rc('lines',linewidth = 4.0)
plt.rc('lines',markersize = 10)

plt.rc('legend',fontsize = 19)

style = 'seaborn-v0_8-colorblind'

plt.style.use(style)

def plot_data(experiment,path,debug=False,has_unlabelled=False):

    data = csv.csv_data(experiment,path,debug,has_unlabelled)

    experiment_sets = data['experiment_sets']
    max_iteration = data['max_iteration']
    generated = data['generated']
    unlabelled = data['unlabelled']
    labelled = data['initial']
    labellingAcc = data['labellingAcc']
    labellingAccSE = data['labellingAccSE']
    labellingTPR = data['labellingTPR']
    labellingTPRSE = data['labellingTPRSE']
    labellingTNR = data['labellingTNR']
    labellingTNRSE = data['labellingTNRSE']
    programAcc = data['programAcc']
    programAccSE = data['programAccSE']
    programTPR = data['programTPR']
    programTPRSE = data['programTPRSE']
    programTNR = data['programTNR']
    programTNRSE = data['programTNRSE']

    # Data structures used to set colours, linestyles and markers for labels to be used in a custom legend.
    # Setup colour ranges. Still trying things here.
    #cmap = plt.cm.jet
    cmap = plt.cm.managua
    #cmap = cmr.ocean
    #cmap = cmr.rainforest
    #cmap = cmr.guppy
    #colors = cmap(np.linspace(0.3, 0.7, max_iteration))
    #colors = cmap(np.linspace(0.1, 0.7, max_iteration))
    colors = cmap(np.linspace(0, 1, max_iteration))
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

    #print(f'max_iteration: {max_iteration}')

    row = 0
    col = 0

    # Always used
    xticks_labelled = labelled[0:max_iteration]
    xlabels_labelled = labelled[0:max_iteration]
    xlabel_labelled = 'Labelled Examples'

    xticks_unlabelled = unlabelled[0:max_iteration]
    xlabels_unlabelled = unlabelled[0:max_iteration]
    xlabel_unlabelled = 'Unlabelled Examples'

    # If we have no unlabelled examples we have only six sub-plots.
    if unlabelled == []:
        fig, axs = plt.subplots(3,2,constrained_layout=True)
        fig.set_size_inches(10,8)

    # If we have unlabelled examples we have 12 sub-plots. 
    elif unlabelled != []:

        fig, axs = plt.subplots(3,4,constrained_layout=True)
        fig.set_size_inches(13,8)
            
        axs[row,col+3].set_title("Hypothesis")
        axs[row,col+2].set_title("Labelling")

        axs[row+2,col+2].set_xlabel(xlabel_unlabelled)
        axs[row+2,col+3].set_xlabel(xlabel_unlabelled)

    # Titles and labels for the labelled examples sub-plots
    axs[row,col].set_title("Labelling")
    axs[row,col+1].set_title("Hypothesis")

    axs[row,col].set_ylabel("Accuracy")
    axs[row+1,col].set_ylabel("TPR")
    axs[row+2,col].set_ylabel("TNR")

    axs[row+2,col].set_xlabel(xlabel_labelled)
    axs[row+2,col+1].set_xlabel(xlabel_labelled)

    for i in range(0,experiment_sets,max_iteration):
        j = i + max_iteration
        lab = str(generated[i])

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

        # Upper left
        plot = axs[row,col].errorbar(labelled[0:max_iteration],labellingAcc[i:j],yerr=labellingAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        plots.append(plot)
        axs[row,col].grid(visible=True, which='major', axis='both')  
        axs[row,col].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Middle left
        axs[row+1,col].errorbar(labelled[0:max_iteration],labellingTPR[i:j],yerr=labellingTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+1,col].grid(visible=True, which='major', axis='both')  
        axs[row+1,col].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Lower left
        axs[row+2,col].errorbar(labelled[0:max_iteration],labellingTNR[i:j],yerr=labellingTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+2,col].grid(visible=True, which='major', axis='both')
        axs[row+2,col].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Upper Right
        axs[row,col+1].errorbar(labelled[0:max_iteration],programAcc[i:j],yerr=programAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row,col+1].grid(visible=True, which='major', axis='both')
        axs[row,col+1].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Upper Right
        axs[row+1,col+1].errorbar(labelled[0:max_iteration],programTPR[i:j],yerr=programTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+1,col+1].grid(visible=True, which='major', axis='both')
        axs[row+1,col+1].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Lower left
        axs[row+2,col+1].errorbar(labelled[0:max_iteration],programTNR[i:j],yerr=programTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+2,col+1].grid(visible=True, which='major', axis='both')
        axs[row+2,col+1].set_xticks(xticks_labelled, labels=xlabels_labelled)

        # Plot unlabelled examples
        if unlabelled != []:

            ## Thid column left
            axs[row,col+2].errorbar(unlabelled[0:max_iteration],labellingAcc[i:j],yerr=labellingAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row,col+2].grid(visible=True, which='major', axis='both')  
            axs[row,col+2].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

            # Middle left
            axs[row+1,col+2].errorbar(unlabelled[0:max_iteration],labellingTPR[i:j],yerr=labellingTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row+1,col+2].grid(visible=True, which='major', axis='both')  
            axs[row+1,col+2].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

            # Lower left
            axs[row+2,col+2].errorbar(unlabelled[0:max_iteration],labellingTNR[i:j],yerr=labellingTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row+2,col+2].grid(visible=True, which='major', axis='both')
            axs[row+2,col+2].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

            # Upper Right
            axs[row,col+3].errorbar(unlabelled[0:max_iteration],programAcc[i:j],yerr=programAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row,col+3].grid(visible=True, which='major', axis='both')
            axs[row,col+3].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

            # Upper Right
            axs[row+1,col+3].errorbar(unlabelled[0:max_iteration],programTPR[i:j],yerr=programTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row+1,col+3].grid(visible=True, which='major', axis='both')
            axs[row+1,col+3].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

            # Lower left
            axs[row+2,col+3].errorbar(unlabelled[0:max_iteration],programTNR[i:j],yerr=programTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
            axs[row+2,col+3].grid(visible=True, which='major', axis='both')
            axs[row+2,col+3].set_xticks(xticks_unlabelled, labels=xlabels_unlabelled)

    fig.align_labels() 
    fig.align_titles()

    #fig.supxlabel(xlabel)
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

    # Creates a custom legend.
    fig.legend(handles=legend_elements,loc='outside right center',fontsize='small',title='Generated\nexamples',title_fontsize='small')

    plt.show()

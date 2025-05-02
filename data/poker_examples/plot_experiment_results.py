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

fig, axs = plt.subplots(3,2,constrained_layout=True)
fig.set_size_inches(10,8)

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
    #cmap = plt.cm.managua
    #cmap = cmr.ocean
    #cmap = cmr.rainforest
    cmap = cmr.pepper
    #cmap = cmr.bubblegum
    #cmap = cmr.gem
    colors = cmap(np.linspace(0.3, 0.7, max_iteration))
    #colors = cmap(np.linspace(0.1, 0.7, max_iteration))
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

    axs[row,col].set_ylabel("Accuracy")
    axs[row+1,col].set_ylabel("TPR")
    axs[row+2,col].set_ylabel("TNR")

    axs[row+2,col].set_xlabel(xlabel)
    axs[row+2,col+1].set_xlabel(xlabel)

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

        # Upper left
        plot = axs[row,col].errorbar(labelled[0:max_iteration],labellingAcc[i:j],yerr=labellingAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        plots.append(plot)
        axs[row,col].grid(visible=True, which='major', axis='both')  
        axs[row,col].set_xticks(xticks, labels=xlabels)

        axs[row,col].set_facecolor(face_colour)

        # Middle left
        axs[row+1,col].errorbar(labelled[0:max_iteration],labellingTPR[i:j],yerr=labellingTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+1,col].grid(visible=True, which='major', axis='both')  
        axs[row+1,col].set_xticks(xticks, labels=xlabels)

        axs[row+1,col].set_facecolor(face_colour)

        # Lower left
        axs[row+2,col].errorbar(labelled[0:max_iteration],labellingTNR[i:j],yerr=labellingTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+2,col].grid(visible=True, which='major', axis='both')
        axs[row+2,col].set_xticks(xticks, labels=xlabels)

        axs[row+2,col].set_facecolor(face_colour)

        # Upper Right
        axs[row,col+1].errorbar(labelled[0:max_iteration],programAcc[i:j],yerr=programAccSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row,col+1].grid(visible=True, which='major', axis='both')
        axs[row,col+1].set_xticks(xticks, labels=xlabels)

        axs[row,col+1].set_facecolor(face_colour)

        # Upper Right
        axs[row+1,col+1].errorbar(labelled[0:max_iteration],programTPR[i:j],yerr=programTPRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+1,col+1].grid(visible=True, which='major', axis='both')
        axs[row+1,col+1].set_xticks(xticks, labels=xlabels)

        axs[row+1,col+1].set_facecolor(face_colour)

        # Lower left
        axs[row+2,col+1].errorbar(labelled[0:max_iteration],programTNR[i:j],yerr=programTNRSE[i:j],label=lab,linestyle=linestyle,marker=marker,color=color)
        axs[row+2,col+1].grid(visible=True, which='major', axis='both')
        axs[row+2,col+1].set_xticks(xticks, labels=xlabels)

        axs[row+2,col+1].set_facecolor(face_colour)

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

    if experiment_sets == 'generated':
        title = 'Generated'
    elif experiment_sets == 'unlabelled':
        title = 'Unlabelled'

    # Creates a custom legend.
    fig.legend(handles=legend_elements,loc='outside right center',fontsize='small',title=f'{title}\nexamples',title_fontsize='small')

    plt.show()

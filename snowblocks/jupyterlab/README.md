
# Table of Contents

1.  [Installation](#org7fab848)
    1.  [Create conda env](#org6d21562)
    2.  [Add addition kernels](#org522cfbe)
        1.  [xeus-python](#org0dc2e57)
        2.  [xeus-cling](#orgce66376)
        3.  [common-lisp-jupyter](#orgb500a73)
    3.  [Add plugins](#orge078012)
        1.  [jupyterlab-templates](#orgf74226e)
        2.  [scratchpad](#orgb317d68)
        3.  [quickopen](#orgaa02b40)
        4.  [dask-labextension](#org3d0da82)
        5.  [debugger](#orge099d9d)



<a id="org7fab848"></a>

# Installation


<a id="org6d21562"></a>

## Create conda env

<div class="sh" id="orgb1d2758">
<p>
conda env create -f environment.yml
</p>

</div>


<a id="org522cfbe"></a>

## Add addition kernels


<a id="org0dc2e57"></a>

### xeus-python

<https://github.com/jupyter-xeus/xeus-python>

<div class="sh" id="orgfc6211f">
<p>
conda install xeus-python -c conda-forge
</p>

</div>


<a id="orgce66376"></a>

### xeus-cling

<https://github.com/jupyter-xeus/xeus-cling>

<div class="sh" id="orgb08533a">
<p>
conda install xeus-cling -c conda-forge
</p>

</div>


<a id="orgb500a73"></a>

### common-lisp-jupyter

<https://github.com/yitzchak/common-lisp-jupyter>

<div class="sh" id="org74afe6f">
<p>
export PATH=$PATH:~/.roswell/bin
ros install common-lisp-jupyter
</p>

</div>


<a id="orge078012"></a>

## Add plugins


<a id="orgf74226e"></a>

### jupyterlab-templates

<https://github.com/timkpaine/jupyterlab_templates>

<div class="sh" id="org09e2f49">
<p>
pip install jupyterlab<sub>templates</sub>
jupyter labextension install jupyterlab<sub>templates</sub>
jupyter serverextension enable &#x2013;py jupyterlab<sub>templates</sub>
</p>

</div>


<a id="orgb317d68"></a>

### scratchpad

<https://github.com/minrk/nbextension-scratchpad>

<div class="sh" id="org6032a02">
<p>
cd ~/opt/
git clone git://github.com/minrk/nbextension-scratchpad
jupyter nbextension install nbextension-scratchpad
jupyter nbextension enable nbextension-scratchpad/main
</p>

</div>


<a id="orgaa02b40"></a>

### quickopen

<https://github.com/parente/jupyterlab-quickopen>

<div class="sh" id="orgdd6d7f7">
<p>
pip install jupyterlab-quickopen
</p>

</div>


<a id="org3d0da82"></a>

### dask-labextension

<https://github.com/dask/dask-labextension>

<div class="sh" id="org75acb76">
<p>
pip install dask-labextension
</p>

</div>


<a id="orge099d9d"></a>

### debugger

<https://github.com/jupyterlab/debugger>

<div class="sh" id="org746295a">
<p>
jupyter labextension install @jupyterlab/debugger
</p>

</div>


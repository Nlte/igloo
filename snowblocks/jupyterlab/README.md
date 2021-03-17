
# Table of Contents

1.  [Installation](#orgc7464c2)
    1.  [Create conda env](#org56137bb)
    2.  [Add addition kernels](#org28a0db4)
        1.  [xeus-python](#org2a8311a)
        2.  [xeus-cling](#org45c9597)
        3.  [common-lisp-jupyter](#org93d700b)
    3.  [Add plugins](#orgd9926d1)
        1.  [jupyterlab-templates](#org88d8408)
        2.  [scratchpad](#org4a4366a)
        3.  [quickopen](#org26b67d8)
        4.  [dask-labextension](#orgca56b10)
        5.  [debugger](#orgdb3b229)
        6.  [jupyterlab-tabular-data-editor](#org0966606)



<a id="orgc7464c2"></a>

# Installation


<a id="org56137bb"></a>

## Create conda env

<div class="sh" id="org7d4e0e7">
<p>
conda env create -f environment.yml
</p>

</div>


<a id="org28a0db4"></a>

## Add addition kernels


<a id="org2a8311a"></a>

### xeus-python

<https://github.com/jupyter-xeus/xeus-python>

<div class="sh" id="org430c6e7">
<p>
conda install xeus-python -c conda-forge
</p>

</div>


<a id="org45c9597"></a>

### xeus-cling

<https://github.com/jupyter-xeus/xeus-cling>

<div class="sh" id="org8afadac">
<p>
conda install xeus-cling -c conda-forge
</p>

</div>


<a id="org93d700b"></a>

### common-lisp-jupyter

<https://github.com/yitzchak/common-lisp-jupyter>

<div class="sh" id="org9b3dfa0">
<p>
export PATH=$PATH:~/.roswell/bin
ros install common-lisp-jupyter
</p>

</div>


<a id="orgd9926d1"></a>

## Add plugins


<a id="org88d8408"></a>

### jupyterlab-templates

<https://github.com/timkpaine/jupyterlab_templates>

    pip install jupyterlab_templates
    jupyter labextension install jupyterlab_templates
    jupyter serverextension enable --py jupyterlab_templates


<a id="org4a4366a"></a>

### scratchpad

<https://github.com/minrk/nbextension-scratchpad>

<div class="sh" id="org50eb2b1">
<p>
cd ~/opt/
git clone git://github.com/minrk/nbextension-scratchpad
jupyter nbextension install nbextension-scratchpad
jupyter nbextension enable nbextension-scratchpad/main
</p>

</div>


<a id="org26b67d8"></a>

### quickopen

<https://github.com/parente/jupyterlab-quickopen>

<div class="sh" id="orgec16ac7">
<p>
pip install jupyterlab-quickopen
</p>

</div>


<a id="orgca56b10"></a>

### dask-labextension

<https://github.com/dask/dask-labextension>

<div class="sh" id="orga15ffaa">
<p>
pip install dask-labextension
</p>

</div>


<a id="orgdb3b229"></a>

### debugger

<https://github.com/jupyterlab/debugger>

<div class="sh" id="org16e62f1">
<p>
jupyter labextension install @jupyterlab/debugger
</p>

</div>


<a id="org0966606"></a>

### jupyterlab-tabular-data-editor

<https://github.com/jupytercalpoly/jupyterlab-tabular-data-editor>

<div class="sh" id="org347ec20">
<p>
jupyter labextension install jupyterlab-tabular-data-editor
</p>

</div>


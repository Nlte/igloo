![ ](img/igloo-logo-banner.jpg)

Based on https://github.com/arcticicestudio/igloo

### Install

```
cd ~
git clone --recursive git@github.com:Nlte/igloo.git
```

### Packages
`./bootstrap -c snowblocks/<package>/`

```
|-- README.md
|-- bootstrap
|-- colors
|   `-- iterm
|       `-- doom.itermcolors
|-- img
|   |-- igloo-logo-banner.jpg
|   |-- igloo-logo-banner.png
|   `-- igloo-logo-banner.svg
|-- lib
|   |-- bash
|   |   |-- script-template.sh
|   |   `-- utils.sh
|   |-- jupyter
|   |   `-- notebooks
|   |       |-- python-analytics-distributed.ipynb
|   |       `-- python-analytics.ipynb
|   |-- makefiles
|   |   `-- python
|   |       |-- Makefile-pipenv
|   |       |-- Makefile-pipenv-simple
|   |       |-- Makefile-poetry
|   |       `-- genreqs-from-pipenv.py
|   |-- pandas
|   |   |-- aggregate-column-names-per-value.ipynb
|   |   |-- concatenate-values.py
|   |   |-- dask-advanced-pivot-table.ipynb
|   |   |-- df-to-excel-in-s3.py
|   |   |-- explode.ipynb
|   |   `-- group-status-based-on-row.py
|   |-- project-templates
|   |   |-- airflow-build
|   |   |   |-- Makefile
|   |   |   `-- README.md
|   |   |-- analytics
|   |   |   |-- README.md
|   |   |   |-- cookiecutter.json
|   |   |   |-- hooks
|   |   |   |   `-- pre_gen_project.py
|   |   |   |-- requirements.txt
|   |   |   |-- tests
|   |   |   |   |-- conftest.py
|   |   |   |   `-- test_creation.py
|   |   |   `-- {{\ cookiecutter.repo_name\ }}
|   |   |       |-- Makefile
|   |   |       |-- README.md
|   |   |       |-- notebooks
|   |   |       |-- requirements.txt
|   |   |       |-- setup.py
|   |   |       |-- src
|   |   |       |   |-- __init__.py
|   |   |       |   |-- data
|   |   |       |   |   |-- __init__.py
|   |   |       |   |   `-- make_dataset.py
|   |   |       |   |-- features
|   |   |       |   |   |-- __init__.py
|   |   |       |   |   `-- build_features.py
|   |   |       |   |-- models
|   |   |       |   |   |-- __init__.py
|   |   |       |   |   |-- predict_model.py
|   |   |       |   |   `-- train_model.py
|   |   |       |   `-- visualization
|   |   |       |       |-- __init__.py
|   |   |       |       `-- visualize.py
|   |   |       |-- test_environment.py
|   |   |       `-- tox.ini
|   |   |-- cpplib
|   |   |   |-- README.md
|   |   |   |-- cookiecutter.json
|   |   |   `-- {{\ cookiecutter.repo_name\ }}
|   |   |       |-- CMakeLists.txt
|   |   |       |-- Makefile
|   |   |       |-- README.md
|   |   |       |-- apps
|   |   |       |   |-- CMakeLists.txt
|   |   |       |   `-- app_main.cpp
|   |   |       |-- docs
|   |   |       |   |-- CMakeLists.txt
|   |   |       |   `-- mainpage.md
|   |   |       |-- include
|   |   |       |   `-- mylib
|   |   |       |       `-- lib.hpp
|   |   |       |-- src
|   |   |       |   |-- CMakeLists.txt
|   |   |       |   `-- lib.cpp
|   |   |       `-- tests
|   |   |           |-- CMakeLists.txt
|   |   |           `-- test_mylib.cpp
|   |   |-- python-lib-minimal
|   |   |   |-- cookiecutter.json
|   |   |   `-- {{\ cookiecutter.package_name\ }}
|   |   |       |-- MANIFEST.in
|   |   |       |-- Makefile
|   |   |       |-- README.md
|   |   |       |-- requirements.txt
|   |   |       |-- requirements_dev.txt
|   |   |       |-- setup.py
|   |   |       `-- {{\ cookiecutter.package_name\ }}
|   |   |           |-- __init__.py
|   |   |           |-- func.py
|   |   |           |-- main.py
|   |   |           `-- tests
|   |   |               |-- __init__.py
|   |   |               |-- test_func.py
|   |   |               `-- test_main.py
|   |   |-- python-lib-minimal-poetry
|   |   |   |-- cookiecutter.json
|   |   |   `-- {{\ cookiecutter.package_name\ }}
|   |   |       |-- Makefile
|   |   |       |-- README.md
|   |   |       |-- pyproject.toml
|   |   |       `-- {{\ cookiecutter.package_name\ }}
|   |   |           |-- __init__.py
|   |   |           |-- func.py
|   |   |           |-- main.py
|   |   |           `-- tests
|   |   |               |-- __init__.py
|   |   |               |-- test_func.py
|   |   |               `-- test_main.py
|   |   |-- python-lib-poetry
|   |   |   |-- README.md
|   |   |   |-- copier.yaml
|   |   |   `-- python-lib-poetry
|   |   |       |-- Makefile.tmpl
|   |   |       |-- README.md.tmpl
|   |   |       |-- [[package_name]]
|   |   |       |   |-- __init__.py
|   |   |       |   |-- cli
|   |   |       |   |   |-- __init__.py
|   |   |       |   |   `-- main.py
|   |   |       |   `-- tests
|   |   |       |       `-- __init__.py
|   |   |       |-- config
|   |   |       |   |-- coverage.ini.tmpl
|   |   |       |   |-- flake8.ini
|   |   |       |   |-- mypy.ini
|   |   |       |   `-- pytest.ini
|   |   |       |-- pyproject.toml.tmpl
|   |   |       `-- scripts
|   |   |           |-- changelog.jinja
|   |   |           |-- changelog.py
|   |   |           `-- setup.sh
|   |   |-- python-scientific
|   |   |   |-- README.md
|   |   |   |-- cookiecutter.json
|   |   |   `-- {{\ cookiecutter.repo_name\ }}
|   |   |       |-- AUTHORS.rst
|   |   |       |-- CONTRIBUTING.rst
|   |   |       |-- LICENSE
|   |   |       |-- MANIFEST.in
|   |   |       |-- README.rst
|   |   |       |-- docs
|   |   |       |   |-- Makefile
|   |   |       |   |-- make.bat
|   |   |       |   `-- source
|   |   |       |       |-- _static
|   |   |       |       |-- conf.py
|   |   |       |       |-- index.rst
|   |   |       |       |-- installation.rst
|   |   |       |       |-- min_versions.rst
|   |   |       |       |-- release-history.rst
|   |   |       |       `-- usage.rst
|   |   |       |-- requirements-dev.txt
|   |   |       |-- requirements.txt
|   |   |       |-- setup.cfg
|   |   |       |-- setup.py
|   |   |       |-- versioneer.py
|   |   |       `-- {{\ cookiecutter.package_dir_name\ }}
|   |   |           |-- __init__.py
|   |   |           |-- _version.py
|   |   |           `-- tests
|   |   |               |-- __init__.py
|   |   |               |-- conftest.py
|   |   |               `-- test_examples.py
|   |   `-- spark-cluster
|   |       |-- Dockerfile
|   |       |-- apps
|   |       |-- data
|   |       |-- docker-compose.yml
|   |       |-- instructions.txt
|   |       `-- start-spark.sh
|   |-- python
|   |   |-- cli-script.py
|   |   `-- fetch-package-version.py
|   `-- sql
|       |-- mysql
|       |   |-- approximate_row_count.sql
|       |   |-- pivot_columns_to_rows.sql
|       |   |-- pk_dist_psize_compute.ipynb
|       |   |-- server_timezone.sql
|       |   `-- table_size.sql
|       `-- psql
|           `-- approximate_row_count.sql
|-- snowblocks
|   |-- alacritty
|   |   |-- alacritty.yml
|   |   `-- snowblock.json
|   |-- bat
|   |   |-- config
|   |   `-- snowblock.json
|   |-- bin
|   |   |-- _pdipython_startup.py
|   |   |-- ig
|   |   |-- ig_aws_ec2_get_instance_by_tag
|   |   |-- ig_aws_ec2_get_privateip_by_tag
|   |   |-- ig_csvtoexcel
|   |   |-- ig_curl_respheaders
|   |   |-- ig_dask_conf.sh
|   |   |-- ig_dask_dashboard
|   |   |-- ig_dask_scheduler
|   |   |-- ig_dask_worker
|   |   |-- ig_decrypt
|   |   |-- ig_docker_kill_all_containers
|   |   |-- ig_docker_mysql_run
|   |   |-- ig_docker_postgres_run
|   |   |-- ig_docker_rm_all
|   |   |-- ig_docker_rm_image_all
|   |   |-- ig_docker_rm_network_all
|   |   |-- ig_encrypt
|   |   |-- ig_exceltocsv
|   |   |-- ig_genshapasswd
|   |   |-- ig_genwordpasswd
|   |   |-- ig_git_initial_commit
|   |   |-- ig_googlechrome
|   |   |-- ig_grep_files
|   |   |-- ig_imgcat
|   |   |-- ig_ip_from_cidr
|   |   |-- ig_jupyter_notebook_open
|   |   |-- ig_jupyter_notebook_template
|   |   |-- ig_jupyter_open
|   |   |-- ig_jupyter_server_list
|   |   |-- ig_jupyter_tmux_session
|   |   |-- ig_k8s_coredns_config
|   |   |-- ig_k8s_coredns_delete
|   |   |-- ig_k8s_coredns_logs
|   |   |-- ig_k8s_coredns_pods
|   |   |-- ig_killallps
|   |   |-- ig_lex_yacc_compile
|   |   |-- ig_macos_screencapture
|   |   |-- ig_man
|   |   |-- ig_openssl_cert_to_text
|   |   |-- ig_pdipython
|   |   |-- ig_python_poetry_local_publish
|   |   |-- ig_pyvenv
|   |   |-- ig_sed_escape
|   |   |-- ig_sed_recursive
|   |   |-- ig_sed_rm_empty_lines
|   |   |-- ig_showtermcolors
|   |   |-- ig_sql_batch_delete
|   |   |-- ig_sql_batch_update
|   |   `-- ig_ssl_getcert
|   |-- broot
|   |   |-- conf.toml
|   |   `-- snowblock.json
|   |-- common-lisp
|   |   |-- README.md
|   |   |-- common-lisp.org
|   |   `-- snowblock.json
|   |-- docker
|   |   |-- config.yml
|   |   `-- snowblock.json
|   |-- doom
|   |   |-- doomd
|   |   |   |-- config.el
|   |   |   |-- custom.el
|   |   |   |-- init.el
|   |   |   |-- packages.el
|   |   |   `-- snippets
|   |   |       |-- org-mode
|   |   |       |   `-- begin_src_dot
|   |   |       `-- python-mode
|   |   |           |-- py_patch_env
|   |   |           |-- py_pdb
|   |   |           `-- sprefectflow
|   |   `-- snowblock.json
|   |-- emacs
|   |   |-- codesamples.el
|   |   |-- early-init.el
|   |   |-- init.el
|   |   |-- snowblock.json
|   |   `-- src
|   |       |-- ig-completion.el
|   |       |-- ig-core-packages.el
|   |       |-- ig-core.el
|   |       |-- ig-editor.el
|   |       |-- ig-evil.el
|   |       |-- ig-git.el
|   |       |-- ig-keybinds.el
|   |       |-- ig-lang-bison.el
|   |       |-- ig-lang-cpp.el
|   |       |-- ig-lang-elisp.el
|   |       |-- ig-lang-markdown.el
|   |       |-- ig-lang-org.el
|   |       |-- ig-lang-python.el
|   |       |-- ig-ledger.el
|   |       |-- ig-lib.el
|   |       |-- ig-lsp.el
|   |       |-- ig-org-capture-templates.el
|   |       |-- ig-project.el
|   |       |-- ig-snippets.el
|   |       |-- ig-terminal.el
|   |       |-- ig-ui.el
|   |       `-- ig-workspaces.el
|   |-- fish
|   |   |-- config.fish
|   |   |-- fish_user_key_bindings.fish
|   |   |-- fishfile
|   |   `-- snowblock.json
|   |-- git
|   |   `-- snowblock.json
|   |-- htop
|   |   |-- htoprc
|   |   `-- snowblock.json
|   |-- iredis
|   |   |-- iredisrc
|   |   `-- snowblock.json
|   |-- jupyterlab
|   |   |-- README.md
|   |   |-- async-ipython-magic
|   |   |-- environment.yml
|   |   |-- ipython_config.py
|   |   |-- ipython_kernel_config.py
|   |   |-- jupyter_notebook_config.py
|   |   |-- jupyterlab.org
|   |   |-- notebook.json
|   |   |-- snowblock.json
|   |   `-- templates
|   |       `-- analytics.ipynb
|   |-- lldb
|   |   |-- cpython_lldb.py
|   |   |-- lldbinit
|   |   |-- lldbinit.py
|   |   `-- snowblock.json
|   |-- mycli
|   |   |-- myclirc
|   |   `-- snowblock.json
|   |-- nvim
|   |   |-- UltiSnips
|   |   |   |-- cpp.snippets
|   |   |   `-- python.snippets
|   |   |-- coc-settings.json
|   |   |-- coc.vim
|   |   |-- init.vim
|   |   |-- install.sh
|   |   |-- nord_additional_syntax.vim
|   |   |-- snowblock.json
|   |   `-- tasks
|   |-- pgcli
|   |   |-- pgclirc
|   |   `-- snowblock.json
|   |-- ripgrep
|   |   |-- config
|   |   `-- snowblock.json
|   |-- sqlite
|   |   |-- liteclirc
|   |   |-- snowblock.json
|   |   `-- sqliterc
|   |-- tmux
|   |   |-- snowblock.json
|   |   `-- tmux.conf
|   `-- wget
|       |-- snowblock.json
|       `-- wgetrc
`-- wallpapers
    |-- wallpaper-nord-1.jpg
    `-- wallpaper-nord-2.jpg
```

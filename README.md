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
.
├── README.md
├── bootstrap
├── colors
│   └── iterm
│       └── doom.itermcolors
├── img
│   ├── igloo-logo-banner.jpg
│   ├── igloo-logo-banner.png
│   └── igloo-logo-banner.svg
├── lib
│   ├── bash
│   │   ├── script-template.sh
│   │   └── utils.sh
│   ├── makefiles
│   │   └── python
│   │       ├── Makefile-pipenv
│   │       ├── Makefile-pipenv-simple
│   │       ├── Makefile-poetry
│   │       └── genreqs-from-pipenv.py
│   ├── pandas
│   │   ├── aggregate-column-names-per-value.ipynb
│   │   ├── concatenate-values.py
│   │   ├── df_to_excel_in_s3.py
│   │   ├── explode.ipynb
│   │   └── group_status_based_on_row.py
│   ├── project-templates
│   │   ├── airflow
│   │   │   ├── Dockerfile
│   │   │   ├── Makefile
│   │   │   ├── README.md
│   │   │   ├── docker-compose.yaml
│   │   │   └── scripts
│   │   │       └── init.sh
│   │   ├── analytics
│   │   │   ├── README.md
│   │   │   ├── cookiecutter.json
│   │   │   ├── hooks
│   │   │   │   └── pre_gen_project.py
│   │   │   ├── requirements.txt
│   │   │   ├── scripts
│   │   │   ├── tests
│   │   │   │   ├── conftest.py
│   │   │   │   └── test_creation.py
│   │   │   └── {{\ cookiecutter.repo_name\ }}
│   │   │       ├── Makefile
│   │   │       ├── README.md
│   │   │       ├── notebooks
│   │   │       ├── requirements.txt
│   │   │       ├── setup.py
│   │   │       ├── src
│   │   │       │   ├── __init__.py
│   │   │       │   ├── data
│   │   │       │   │   ├── __init__.py
│   │   │       │   │   └── make_dataset.py
│   │   │       │   ├── features
│   │   │       │   │   ├── __init__.py
│   │   │       │   │   └── build_features.py
│   │   │       │   ├── models
│   │   │       │   │   ├── __init__.py
│   │   │       │   │   ├── predict_model.py
│   │   │       │   │   └── train_model.py
│   │   │       │   └── visualization
│   │   │       │       ├── __init__.py
│   │   │       │       └── visualize.py
│   │   │       ├── test_environment.py
│   │   │       └── tox.ini
│   │   └── python-lib-poetry
│   │       ├── README.md
│   │       ├── copier.yaml
│   │       └── python-lib-poetry
│   │           ├── Makefile.tmpl
│   │           ├── README.md.tmpl
│   │           ├── [[package_name]]
│   │           │   ├── __init__.py
│   │           │   ├── cli
│   │           │   │   ├── __init__.py
│   │           │   │   └── main.py
│   │           │   └── tests
│   │           │       └── __init__.py
│   │           ├── config
│   │           │   ├── coverage.ini.tmpl
│   │           │   ├── flake8.ini
│   │           │   ├── mypy.ini
│   │           │   └── pytest.ini
│   │           ├── pyproject.toml.tmpl
│   │           └── scripts
│   │               ├── changelog.jinja
│   │               ├── changelog.py
│   │               └── setup.sh
│   ├── python
│   │   └── cli-script.py
│   └── sql
│       ├── mysql
│       │   ├── approximate_row_count.sql
│       │   ├── pivot_columns_to_rows.sql
│       │   ├── pk_dist_psize_compute.ipynb
│       │   └── server_timezone.sql
│       └── psql
│           └── approximate_row_count.sql
├── snowblocks
│   ├── bat
│   │   ├── config
│   │   └── snowblock.json
│   ├── bin
│   │   ├── _pdipython_startup.py
│   │   ├── ig
│   │   ├── ig_aws_ec2_get_instance_by_tag
│   │   ├── ig_aws_ec2_get_privateip_by_tag
│   │   ├── ig_csvtoexcel
│   │   ├── ig_curl_respheaders
│   │   ├── ig_dask_conf.sh
│   │   ├── ig_dask_dashboard
│   │   ├── ig_dask_scheduler
│   │   ├── ig_dask_worker
│   │   ├── ig_decrypt
│   │   ├── ig_docker_mysql_run
│   │   ├── ig_docker_rm_all
│   │   ├── ig_docker_rm_image_all
│   │   ├── ig_encrypt
│   │   ├── ig_exceltocsv
│   │   ├── ig_genshapasswd
│   │   ├── ig_genwordpasswd
│   │   ├── ig_git_initial_commit
│   │   ├── ig_googlechrome
│   │   ├── ig_grep_files
│   │   ├── ig_imgcat
│   │   ├── ig_ip_from_cidr
│   │   ├── ig_jupyter_notebook_open
│   │   ├── ig_jupyter_open
│   │   ├── ig_jupyter_server_list
│   │   ├── ig_jupyter_tmux_session
│   │   ├── ig_k8s_coredns_config
│   │   ├── ig_k8s_coredns_delete
│   │   ├── ig_k8s_coredns_logs
│   │   ├── ig_k8s_coredns_pods
│   │   ├── ig_killallps
│   │   ├── ig_man
│   │   ├── ig_openssl_cert_to_text
│   │   ├── ig_pdipython
│   │   ├── ig_pyvenv
│   │   ├── ig_sed_escape
│   │   ├── ig_sed_recursive
│   │   ├── ig_sed_rm_empty_lines
│   │   ├── ig_showtermcolors
│   │   ├── ig_sql_batch_update
│   │   └── ig_ssl_getcert
│   ├── broot
│   │   ├── conf.toml
│   │   └── snowblock.json
│   ├── common-lisp
│   │   ├── README.md
│   │   ├── common-lisp.org
│   │   └── snowblock.json
│   ├── docker
│   │   ├── config.yml
│   │   └── snowblock.json
│   ├── doom
│   │   ├── doomd
│   │   │   ├── config.el
│   │   │   ├── custom.el
│   │   │   ├── init.el
│   │   │   └── packages.el
│   │   └── snowblock.json
│   ├── emacs
│   │   ├── emacs
│   │   │   ├── core.el
│   │   │   ├── early-init.el
│   │   │   ├── init.el
│   │   │   ├── install
│   │   │   └── lisp
│   │   │       ├── #igloo-ui.el#
│   │   │       ├── igloo-buffer.el
│   │   │       ├── igloo-company.el
│   │   │       ├── igloo-config.el
│   │   │       ├── igloo-csv.el
│   │   │       ├── igloo-dashboard.el
│   │   │       ├── igloo-dashboard.el~
│   │   │       ├── igloo-edit.el
│   │   │       ├── igloo-elisp.el
│   │   │       ├── igloo-env.el
│   │   │       ├── igloo-evil.el
│   │   │       ├── igloo-flycheck.el
│   │   │       ├── igloo-general.el
│   │   │       ├── igloo-git.el
│   │   │       ├── igloo-hydra.el
│   │   │       ├── igloo-ivy.el
│   │   │       ├── igloo-jira.el
│   │   │       ├── igloo-lib.el
│   │   │       ├── igloo-lsp.el
│   │   │       ├── igloo-org.el
│   │   │       ├── igloo-projectile.el
│   │   │       ├── igloo-python.el
│   │   │       ├── igloo-shell.el
│   │   │       ├── igloo-snippet.el
│   │   │       ├── igloo-tab.el
│   │   │       ├── igloo-tab.el~
│   │   │       ├── igloo-theme.el
│   │   │       ├── igloo-tree.el
│   │   │       ├── igloo-ui.el
│   │   │       ├── igloo-ui.el~
│   │   │       ├── igloo-workspace.el
│   │   │       ├── igloo-yaml.el
│   │   │       ├── init-benchmarking.el
│   │   │       ├── ivy-make.el
│   │   │       ├── nano-base-colors.el
│   │   │       ├── nano-faces.el
│   │   │       ├── nano-modeline.el
│   │   │       ├── nano-theme-dark.el
│   │   │       ├── nano-theme-light.el
│   │   │       ├── nano-theme.el
│   │   │       └── nose.el
│   │   └── snowblock.json
│   ├── fish
│   │   ├── config.fish
│   │   ├── fishfile
│   │   └── snowblock.json
│   ├── git
│   │   └── snowblock.json
│   ├── htop
│   │   ├── htoprc
│   │   └── snowblock.json
│   ├── iredis
│   │   ├── iredisrc
│   │   └── snowblock.json
│   ├── jupyterlab
│   │   ├── README.md
│   │   ├── environment.yml
│   │   ├── ipython_config.py
│   │   ├── ipython_kernel_config.py
│   │   ├── jupyter_notebook_config.py
│   │   ├── jupyterlab.org
│   │   ├── snowblock.json
│   │   └── templates
│   │       └── analytics.ipynb
│   ├── lldb
│   │   ├── cpython_lldb.py
│   │   ├── lldbinit
│   │   ├── lldbinit.py
│   │   └── snowblock.json
│   ├── mycli
│   │   ├── myclirc
│   │   └── snowblock.json
│   ├── nvim
│   │   ├── UltiSnips
│   │   │   ├── cpp.snippets
│   │   │   └── python.snippets
│   │   ├── coc-settings.json
│   │   ├── coc.vim
│   │   ├── init.vim
│   │   ├── install.sh
│   │   ├── nord_additional_syntax.vim
│   │   ├── snowblock.json
│   │   └── tasks
│   ├── pgcli
│   │   ├── pgclirc
│   │   └── snowblock.json
│   ├── ripgrep
│   │   ├── config
│   │   └── snowblock.json
│   ├── sqlite
│   │   ├── liteclirc
│   │   ├── snowblock.json
│   │   └── sqliterc
│   ├── tmux
│   │   ├── snowblock.json
│   │   └── tmux.conf
│   └── wget
│       ├── snowblock.json
│       └── wgetrc
```

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
│   │   ├── scrip-template.sh
│   │   └── utils.sh
│   ├── makefiles
│   │   └── Makefile-python-pipenv
│   ├── pandas
│   │   ├── aggregate-column-names-per-value.ipynb
│   │   └── explode.ipynb
│   ├── project-templates
│   │   └── analytics
│   └── sql
│       └── mysql
│           ├── mysql-server-timezone.sql
│           └── pivot_columns_to_rows.sql
├── snowblocks
│   ├── bat
│   │   ├── config
│   │   └── snowblock.json
│   ├── bin
│   │   ├── _pdipython_startup.py
│   │   ├── ig_aws_ec2_get_instance_by_tag
│   │   ├── ig_aws_ec2_get_privateip_by_tag
│   │   ├── ig_csvtoexcel
│   │   ├── ig_curl_respheaders
│   │   ├── ig_decrypt
│   │   ├── ig_docker_rm_all
│   │   ├── ig_docker_rm_image_all
│   │   ├── ig_encrypt
│   │   ├── ig_exceltocsv
│   │   ├── ig_genshapasswd
│   │   ├── ig_genwordpasswd
│   │   ├── ig_googlechrome
│   │   ├── ig_grep_files
│   │   ├── ig_imgcat
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
│   │   ├── ig_pdipython
│   │   ├── ig_pyvenv
│   │   ├── ig_sed_recursive
│   │   ├── ig_showtermcolors
│   │   └── ig_ssl_getcert
│   ├── broot
│   │   ├── conf.toml
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
│   │   │   ├── init.el
│   │   │   └── lisp
│   │   │       ├── igloo-buffer.el
│   │   │       ├── igloo-cache.el
│   │   │       ├── igloo-company.el
│   │   │       ├── igloo-config.el
│   │   │       ├── igloo-csv.el
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
│   │   │       ├── igloo-theme.el
│   │   │       ├── igloo-ui.el
│   │   │       ├── igloo-workspace.el
│   │   │       ├── igloo-yaml.el
│   │   │       ├── init-benchmarking.el
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
└── wallpapers
    ├── wallpaper-nord-1.jpg
    └── wallpaper-nord-2.jpg
```

# helper for _fzf_search_git_status
# arg should be a line from git status --short, e.g.
# MM functions/_fzf_preview_changed_file.fish
#  D README.md
# R  LICENSE -> "New License"
function _ig_fzf_preview_changed_file --argument-names path_status --description "Show the git diff of the given file."
    # remove quotes because they'll be interpreted literally by git diff
    # no need to requote when referencing $path because fish does not perform word splitting
    # https://fishshell.com/docs/current/fish_for_bash_users.html
    set -f path (string unescape (string sub --start 4 $path_status))
    # first letter of short format shows index, second letter shows working tree
    # https://git-scm.com/docs/git-status/2.35.0#_short_format
    set -f index_status (string sub --length 1 $path_status)
    set -f working_tree_status (string sub --start 2 --length 1 $path_status)

    set -f diff_opts --color=always

    if test $index_status = '?'
        _fzf_report_diff_type Untracked
        _fzf_preview_file $path
    else if contains {$index_status}$working_tree_status DD AU UD UA DU AA UU
        # Unmerged statuses taken directly from git status help's short format table
        # Unmerged statuses are mutually exclusive with other statuses, so if we see
        # these, then safe to assume the path is unmerged
        _fzf_report_diff_type Unmerged
        git diff $diff_opts -- $path
    else
        if test $index_status != ' '
            _fzf_report_diff_type Staged

            # renames are only detected in the index, never working tree, so only need to test for it here
            # https://stackoverflow.com/questions/73954214
            if test $index_status = R
                # diff the post-rename path with the original path, otherwise the diff will show the entire file as being added
                set -f orig_and_new_path (string split --max 1 -- ' -> ' $path)
                git diff --staged $diff_opts -- $orig_and_new_path[1] $orig_and_new_path[2]
                # path currently has the form of "original -> current", so we need to correct it before it's used below
                set path $orig_and_new_path[2]
            else
                git diff --staged $diff_opts -- $path
            end
        end

        if test $working_tree_status != ' '
            _fzf_report_diff_type Unstaged
            git diff $diff_opts -- $path
        end
    end
end

# helper for _fzf_preview_changed_file
# prints out something like
#  Staged
function _ig_fzf_report_diff_type --argument-names diff_type --description "Print a distinct colored header meant to preface a git patch."
    # number of "-" to draw is the length of the string to box + 2 for padding
    set -f repeat_count (math 2 + (string length $diff_type))
    set -f line (string repeat --count $repeat_count ─)
    set -f top_border |$line|
    set -f btm_border |$line|

    set_color yellow
    echo $top_border
    echo " $diff_type "
    echo $btm_border
    set_color normal
end

function _ig_fzf_search_directory --description "Search the current directory. Replace the current token with the selected file paths."
    # Directly use fd binary to avoid output buffering delay caused by a fd alias, if any.
    # Debian-based distros install fd as fdfind and the fd package is something else, so
    # check for fdfind first. Fall back to "fd" for a clear error message.
    set -f fd_cmd (command -v fdfind || command -v fd  || echo "fd")
    set -f --append fd_cmd --color=always $fzf_fd_opts

    # $fzf_dir_opts is the deprecated version of $fzf_directory_opts
    set -f fzf_arguments --multi --ansi $fzf_dir_opts $fzf_directory_opts
    set -f token (commandline --current-token)
    # expand any variables or leading tilde (~) in the token
    set -f expanded_token (eval echo -- $token)
    # unescape token because it's already quoted so backslashes will mess up the path
    set -f unescaped_exp_token (string unescape -- $expanded_token)

    # If the current token is a directory and has a trailing slash,
    # then use it as fd's base directory.
    if string match --quiet -- "*/" $unescaped_exp_token && test -d "$unescaped_exp_token"
        set --append fd_cmd --base-directory=$unescaped_exp_token
        # use the directory name as fzf's prompt to indicate the search is limited to that directory
        set --prepend fzf_arguments --prompt="Search Directory $unescaped_exp_token> " --preview="_fzf_preview_file $expanded_token{}"
        set -f file_paths_selected $unescaped_exp_token($fd_cmd 2>/dev/null | fzf $fzf_arguments)
    else
        set --prepend fzf_arguments --prompt="Search Directory> " --query="$unescaped_exp_token" --preview='_fzf_preview_file {}'
        set -f file_paths_selected ($fd_cmd 2>/dev/null | fzf $fzf_arguments)
    end


    if test $status -eq 0
        commandline --current-token --replace -- (string escape -- $file_paths_selected | string join ' ')
    end

    commandline --function repaint
end


function _ig_fzf_search_git_log --description "Search the output of git log and preview commits. Replace the current token with the selected commit hash."
    if not git rev-parse --git-dir >/dev/null 2>&1
        echo '_fzf_search_git_log: Not in a git repository.' >&2
    else
        if not set --query fzf_git_log_format
            # %h gives you the abbreviated commit hash, which is useful for saving screen space, but we will have to expand it later below
            set -f fzf_git_log_format '%C(bold blue)%h%C(reset) - %C(cyan)%ad%C(reset) %C(yellow)%d%C(reset) %C(normal)%s%C(reset)  %C(dim normal)[%an]%C(reset)'
        end

        set -f preview_cmd 'git show --color=always --stat --patch {1}'
        if set --query fzf_diff_highlighter
            set preview_cmd "$preview_cmd | $fzf_diff_highlighter"
        end

        set -f selected_log_lines (
            git log --no-show-signature --color=always --format=format:$fzf_git_log_format --date=short | \
            fzf --ansi \
                --multi \
                --prompt="Git Log> " \
                --preview=$preview_cmd \
                --query=(commandline --current-token) \
                $fzf_git_log_opts
        )
        if test $status -eq 0
            for line in $selected_log_lines
                set -f abbreviated_commit_hash (string split --field 1 " " $line)
                set -f full_commit_hash (git rev-parse $abbreviated_commit_hash)
                set -f --append commit_hashes $full_commit_hash
            end
            commandline --current-token --replace (string join ' ' $commit_hashes)
        end
    end

    commandline --function repaint
end


function _ig_fzf_search_git_status --description "Search the output of git status. Replace the current token with the selected file paths."
    if not git rev-parse --git-dir >/dev/null 2>&1
        echo '_fzf_search_git_status: Not in a git repository.' >&2
    else
        set -f preview_cmd '_fzf_preview_changed_file {}'
        if set --query fzf_diff_highlighter
            set preview_cmd "$preview_cmd | $fzf_diff_highlighter"
        end

        set -f selected_paths (
            # Pass configuration color.status=always to force status to use colors even though output is sent to a pipe
            git -c color.status=always status --short |
            fzf --ansi \
                --multi \
                --prompt="Git Status> " \
                --query=(commandline --current-token) \
                --preview=$preview_cmd \
                --nth="2.." \
                $fzf_git_status_opts
        )
        if test $status -eq 0
            # git status --short automatically escapes the paths of most files for us so not going to bother trying to handle
            # the few edges cases of weird file names that should be extremely rare (e.g. "this;needs;escaping")
            set -f cleaned_paths

            for path in $selected_paths
                if test (string sub --length 1 $path) = R
                    # path has been renamed and looks like "R LICENSE -> LICENSE.md"
                    # extract the path to use from after the arrow
                    set --append cleaned_paths (string split -- "-> " $path)[-1]
                else
                    set --append cleaned_paths (string sub --start=4 $path)
                end
            end

            commandline --current-token --replace -- (string join ' ' $cleaned_paths)
        end
    end

    commandline --function repaint
end


function _ig_fzf_search_history --description "Search command history. Replace the command line with the selected command."
    # history merge incorporates history changes from other fish sessions
    # it errors out if called in private mode
    if test -z "$fish_private_mode"
        builtin history merge
    end

    if not set --query fzf_history_time_format
        # Reference https://devhints.io/strftime to understand strftime format symbols
        set -f fzf_history_time_format "%m-%d %H:%M:%S"
    end

    # Delinate commands throughout pipeline using null rather than newlines because commands can be multi-line
    set -f commands_selected (
        builtin history --null --show-time="$fzf_history_time_format │ " |
        fzf --read0 \
            --print0 \
            --multi \
            --prompt="History> " \
            --query=(commandline) \
            --preview="echo -- {} | string replace --regex '^.*? │ ' '' | fish_indent --ansi" \
            --preview-window="bottom:3:wrap" \
            $fzf_history_opts |
        string split0 |
        # remove timestamps from commands selected
        string replace --regex '^.*? │ ' ''
    )

    if test $status -eq 0
        commandline --replace -- $commands_selected
    end

    commandline --function repaint
end


function _ig_fzf_search_processes --description "Search all running processes. Replace the current token with the pid of the selected process."
    # Directly use ps command because it is often aliased to a different command entirely
    # or with options that dirty the search results and preview output
    set -f ps_cmd (command -v ps || echo "ps")
    # use all caps to be consistent with ps default format
    # snake_case because ps doesn't seem to allow spaces in the field names
    set -f ps_preview_fmt (string join ',' 'pid' 'ppid=PARENT' 'user' '%cpu' 'rss=RSS_IN_KB' 'start=START_TIME' 'command')
    set -f processes_selected (
        $ps_cmd -A -opid,command | \
        fzf --multi \
                    --prompt="Processes> " \
                    --query (commandline --current-token) \
                    --ansi \
                    # first line outputted by ps is a header, so we need to mark it as so
                    --header-lines=1 \
                    # ps uses exit code 1 if the process was not found, in which case show an message explaining so
                    --preview="$ps_cmd -o '$ps_preview_fmt' -p {1} || echo 'Cannot preview {1} because it exited.'" \
                    --preview-window="bottom:4:wrap" \
                    $fzf_processes_opts
    )

    if test $status -eq 0
        for process in $processes_selected
            set -f --append pids_selected (string split --no-empty --field=1 -- " " $process)
        end

        # string join to replace the newlines outputted by string split with spaces
        commandline --current-token --replace -- (string join ' ' $pids_selected)
    end

    commandline --function repaint
end


function _ig_fzf_search_all_commands --description "Search all commands"
	set -f command_selected (
		begin; apropos -s 1 '' && ls $HOME/igloo/snowblocks/bin/ig_* | rev | cut -d'/' -f 1 | rev; end |
		fzf
	)

	if test $status -eq 0
		commandline --current-token --replace -- (string split -f1 ' ' $command_selected)
	end

	commandline --function repaint
end


function _ig_fzf_search_ig_commands --description "Search igloo commands"
	set -f command_selected (
		begin; ls $HOME/igloo/snowblocks/bin/ig_* | rev | cut -d'/' -f 1 | rev; end |
		fzf
	)

	if test $status -eq 0
		commandline --current-token --replace -- (string split -f1 ' ' $command_selected)
	end

	commandline --function repaint
end


function rr --description "Run command with previous command output"
    set prev (history | head -n 1)
    set prev_output (eval $prev)
    set cmd $argv[1]
    echo "Running '$cmd $prev_output'"
    eval "$cmd $prev_output"
end


function qs --description "Ripgrep++"
    if test (count $argv) -eq 0
        echo "usage: qs <search term>"
        return
    end
    set search $argv[1]
    set f $(rg --files-with-matches --no-messages "$search" | fzf -0 -m --preview-window=up:50% --preview "bat --color=always {} | rg -N --color always --colors 'match:bg:yellow' --ignore-case --context 12 '$search' || bat --color=always {}")
    if test -n "$f"
        echo $(realpath "$f") > ~/.qs_last_file
        echo "$f"
    end
end


function qse --description "Edit qs selection"
    hx $(cat ~/.qs_last_file)
end


function venv --description "Activate python virtualenv"
	set selected_venv $(ls ~/pyenvs/ | fzf)
    	if test -n "$selected_venv"
		source ~/pyenvs/$selected_venv/bin/activate.fish
	end
end


function spd --description "Go to project directory"
	set selected $(ls ~/projects/ | fzf)
    	if test -n "$selected"
            cd ~/projects/$selected
    	end
end

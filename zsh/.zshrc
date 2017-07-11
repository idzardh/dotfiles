## Initialize colors
autoload -U colors
colors

# allow for functions in the prompt
setopt PROMPT_SUBST

# auto load zsh functions
fpath=(~/dotfiles/zsh $fpath)
autoload -U ~/dotfiles/zsh/*(:t)

# enable auto execution
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

# append git functions needed for prompt
preexec_functions+='preexec_update_git_vars'
precmd_functions+='precmd_update_git_vars'
chpwd_functions+='chpwd_update_git_vars'

# set prompt
PROMPT='[%F{blue}%n%f:%F{yellow}%2/%f$(prompt_git_info)%{${fg[default]}%}]%# '
RPROMPT='[%F{red}%?%f]'

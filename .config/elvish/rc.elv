use re
use readline-binding
use path
use str
use math

eval (starship init elvish)

use epm
use ./env

epm:install &silent-if-installed ^
            github.com/zzamboni/elvish-completions ^
            github.com/xiaq/edit.elv

use github.com/zzamboni/elvish-completions/builtins
use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/dd
use github.com/zzamboni/elvish-completions/git
use github.com/zzamboni/elvish-completions/ssh

use github.com/xiaq/edit.elv/smart-matcher
smart-matcher:apply

set edit:insert:binding[Alt-Backspace] = $edit:kill-small-word-left~
set edit:insert:binding[Alt-d] = $edit:kill-small-word-right~
set edit:insert:binding[Alt-m] = $edit:-instant:start~
set edit:insert:binding[Alt-b] = $edit:move-dot-left-alnum-word~
set edit:insert:binding[Alt-f] = $edit:move-dot-right-alnum-word~
set edit:max-height = 20

use ./alias
use ./venv

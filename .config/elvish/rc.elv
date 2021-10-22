use re
use readline-binding
use path
use str
use math

eval (starship init elvish)

use epm

epm:install &silent-if-installed ^
            github.com/zzamboni/elvish-completions

use github.com/zzamboni/elvish-completions/builtins
use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/dd
use github.com/zzamboni/elvish-completions/git
use github.com/zzamboni/elvish-completions/ssh

edit:insert:binding[Alt-Backspace] = $edit:kill-small-word-left~
edit:insert:binding[Alt-d] = $edit:kill-small-word-right~
edit:insert:binding[Alt-m] = $edit:-instant:start~
edit:max-height = 20

use ./alias

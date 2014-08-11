# orgmode gtd (Get Things Done) pack

An [emacs-live](https://github.com/overtone/emacs-live) pack containing my cutomsiations for using GTD.
Stolen from numerous GTD org-modes on the web and modified for my work flow. Includes org-pomodoro

## Installing and loading the pack

First clone or add submodule org-gtd-pack into a folder (for example `~/.live-packs/gas-gtd-pack`).

Then create a `~/.emacs-live.el` file and use the function `live-add-packs` to load the gas-gtd-pack:

    (live-add-packs '(~/.live-packs/gas-gtd-pack))

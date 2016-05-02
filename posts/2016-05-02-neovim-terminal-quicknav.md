---
title: Quick navigation with Neovim terminals
---

I've recently started using Neovim as a substitute for tmux on OSX. (On Linux I
just use Xmonad to lay out terminals, but I've got to say that the integrated
experience that comes from using terminals within vim is _really_ nice.) The
new terminal buffers are pretty impressive, but one thing they lack out of the
box is decent navigation.

Neovim terminal theory
----------------------

The terminal emulator support in Neovim works in the following way. First, a
new kind of buffer is introduced, called a terminal buffer. We can
programmatically determine whether the current buffer is a terminal buffer by
comparing the value of the `buftype` setting with the string `'terminal'`.

```
if &buftype ==# 'terminal'
    echo 'The buffer is a terminal.'
endif
```

Terminal buffers are not modifiable, so commands that delete text won't work.

Second, a new kind of input mode is introduced, called terminal mode. Terminal
mode is *essentially* insert mode, but there are some differences.

 1. Instead of `--INSERT--`, the input mode indicator reads `--TERMINAL--`.
    This difference doesn't *really* matter, but it's a difference nonetheless.
 2. A whole new family of mapping commands is added; they start with the letter
    `t`, e.g. `tnoremap`.
 3. By default, the way to exit terminal mode is only with the sequence
    `<C-\><C-n>`. (This sequence can in fact be used to go to normal mode from
    any mode.)

Getting decent navigation
-------------------------

First, since in order to change windows in vim I have `<C-j>` mapped to
`<C-w><C-j>`, it would be nice if pressing `<C-j>` did the same in terminal
windows.

```
tnoremap <C-h> <C-\><C-n><C-h>
tnoremap <C-j> <C-\><C-n><C-j>
tnoremap <C-k> <C-\><C-n><C-k>
tnoremap <C-l> <C-\><C-n><C-l>
```

Already this is pretty good, but an invariant I had grown accustomed to is now
broken: `<C-h>` followed by `<C-l>` is no longer a no-op! Enter terminal mode
in a terminal buffer, and press `<C-h>` followed by `<C-l>`; we've returned to
the terminal buffer we were in (assuming there are no horizontal splits), but
we're now in normal mode.

A simple fix is to make vim automatically enter terminal mode when entering a
window that's viewing a terminal buffer. We can use an autocommand for this.

```
autocmd WinEnter *
 \ if &buftype ==# 'terminal'
 \  startinsert
 \ endif
```

This solution is somewhat dissatisfying when moving between tabs though.
Suppose you move to the next tab, and the window that has focus in that tab is
viewing a terminal buffer. Then this autocommand will cause vim to enter
terminal mode in that window. This means that you need to exit terminal mode
before being able to change tabs again. If you know a solution to this, please
let me know.

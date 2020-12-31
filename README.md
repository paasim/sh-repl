# sh-repl

[![build status](https://github.com/paasim/sh-repl/workflows/check/badge.svg)](https://github.com/paasim/sh-repl/actions)

A simple tool for printing and evaluating shell-oneliners. Might be useful for rendering shell scripts to markdown (with the command printed before the output).


## Install
Can be installed with [stack] by running

    stack install sh-repl

## Usage
Reads the input from `stdin` and returns to output to `stdout`. If multiple lines are provided, each is evaluated and printed separately. Allows the user to optionally specify a string for the prompt and maximum number of output lines to be printed (the defaults being `$` and infinite respectively).

```bash
echo "ls | grep yaml" | sh-repl-exe
$ ls | grep yaml
package.yaml
stack.yaml
stack.yaml.lock

echo "ls | grep yaml" | sh-repl-exe --prompt ">" --max-lines 2
> ls | grep yaml
package.yaml
stack.yaml
...
```


## Caveats
Currently relatively simple, is able to recognize `|`, `;` and `#` (comments), but other "special characters" in shells such as `\` is not supported currently. In other words, the following will not be evaluated correctly:

```bash
cmd \
  --arg1 value \
  --arg2 value
```

Also `sh-repl` disregards exit codes and instead simply prints `stdout` if there is some output and `stderr` otherwise. One reason for this is that `grep` returns a nonzero exit code when the expression does not match any lines and for some use cases this makes sense.


[stack]: https://github.com/commercialhaskell/stack


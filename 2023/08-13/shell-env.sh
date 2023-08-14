#!/usr/bin/bash

# Bash has some weird behaviors that when you use it as a scripting language they can shoot you on the foot. Namely

# 1. If a command returns non zero exit code the bash will not exit immediately instead it will execute the next line
#    e.g
#    ```bash
#    rm -rf x
#    cd x
#    echo "hi"
#    ```
#    This will print hi. 
#    This happens because bash was built as a repl, but as a script you want it to stop if an error occurs.
#
# `set -e`: exit immediately if a command exits with non zero status.

# 2. If you try to access an undefined variable `X` it will falls back as an empty string. As a script you want it to be 
#    and error
# 
#   `set -u`: accessing an undefined variable is an error instead of treating it as empty string

# 3. When you have a pipes that combines multiple commands, a command fails in the middle of the pipe will not necessarily 
#    fail the whole pipe. What you want is whenever a comamnd fails the pipe fails as well and return the same status code
#    of the failed command.
#
#    `set -o pipefail`



set -euo pipefail


echo "Do whatever here."

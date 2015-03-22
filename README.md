# DUOLOGUE

High-level interaction library for Common Lisp

## Functions
### choose (msg options &key if-wrong-option default (print-options t) (separator ~%) complete (color \*prompt-color\*) (error-color \*prompt-error-color\*))
Asks the user to choose one of the given options.

- **msg**: (string) The prompt message.
- **options**: (list) The list of options the user can choose from.
- **if-wrong-option**: (function) When present, this function is run if the user enters a wrong option. Default: nil.
- **default**: The default value. The default value is selected if the user just hits the ENTER key. Default: nil.
- **print-options**: (boolean) Print the options on the screen. Default: T.
- **separator**: (string) Separation string to use when printing the options. Default: '~%'
- **complete**: If T, then readline completion is enabled. Default: nil.
- **color**: Color to use at prompt. Default: *prompt-color*
- **error-color**: Color to use when error ocurrs. Default: *prompt-error-color*


**Tags**: menu, choose


### say (datum &rest args)
Prints a message on the screen.

- **datum**: (string) A format like string.
- **args**: Format arguments or :color, :newline options
- **color**: An ansi-text color. One of ansi-colors (.i.e :red, :green, :yellow)
- **newline**: If t, forces a newline after printing


A newline is printed iff either newline parameter is T or datum doesn't end with a space. That is, if datum ends in a space, then no newline is printed.

   Example: 
   ```
(say "Hello ~A" "John" :color :blue)
```
**Categories**: printing
**Tags**: printing


## Macros
## Generic-Functions
## Slot-Accessors
## Variables
## Classs
## Conditions
## Constants

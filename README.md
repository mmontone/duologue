# DUOLOGUE

High-level interaction library for Common Lisp

## Functions
### ask (&optional (msg Yes or no: ) &key (default nil default-p) if-wrong-answer (color \*prompt-color\*) (error-color \*prompt-error-color\*))
Ask for yes or no.

- **msg**: (string) The prompt to use. Default: 'Yes or no: '.
- **default**: Default value. It gets selected if the user enters the empty string. Default: nil.
- **if-wrong-answer**: (function) Function to execute if a wrong answer is given.
- **color**: Prompt color.
- **error-color**: Prompt error color.




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


Example: 

  ```
(choose "Choose: " (list "foo" "bar" "baz") :default "baz")
```
**Tags**: menu, choose


### prompt (&optional msg &key default (required-p t) validator if-invalid (color \*prompt-color\*) (error-color \*prompt-error-color\*))
Prompt for a string.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **validator**: (function) A function to use to validate the input. Should return T if the input is valid, or NIL otherwise.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.




### say (datum &rest args)
Prints a message on the screen.

- **datum**: (string) A format like string.
- **args**: Format arguments or :color, :newline options
- **color**: (keyword) An ansi-text color. One of ansi-colors (.i.e :red, :green, :yellow)
- **newline**: (boolean) If t, forces a newline after printing


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
### \*prompt-color\*
The default prompt color.

### \*prompt-error-color\*
The default error color

## Classs
## Conditions
## Constants

# DUOLOGUE

High-level interaction library for Common Lisp

## Functions
### ask

```lisp
(&optional (msg "Yes or no: ") &key (default nil default-p) if-wrong-answer
 (color *prompt-color*) (error-color *prompt-error-color*))
```

Ask for yes or no.

- **msg**: (string) The prompt to use. Default: 'Yes or no: '.
- **default**: Default value. It gets selected if the user enters the empty string. Default: nil.
- **if-wrong-answer**: (function) Function to execute if a wrong answer is given.
- **color**: Prompt color.
- **error-color**: Prompt error color.




### choose

```lisp
(msg options &key if-wrong-option default (print-options t) (separator "~%")
 complete (color *prompt-color*) (error-color *prompt-error-color*))
```

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

  ```lisp
(choose "Choose: " (list "foo" "bar" "baz") :default "baz")
```
**Tags**: menu, choose


### choose-many

```lisp
(msg options &key if-wrong-option default (print-options t) (separator "~%")
 complete (test #'eql) (color *prompt-color*)
 (error-color *prompt-error-color*))
```

Asks the user to choose many of the given options.

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

  ```lisp
(choose-many "Choose: " (list "foo" "bar" "baz") :default "baz")
```
**Tags**: menu, choose


### parse-prompt

```lisp
(parser &optional msg &key default (required-p t) validator if-invalid
 (color *prompt-color*) (error-color *prompt-error-color*))
```

Like prompt, but parses its input.

- **parser**: A function that parses the input string. Should return NIL if it cannot parse.
- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **validator**: (function) A function to use to validate the input. Should return T if the input is valid, or NIL otherwise.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.


**Returns**: The parsed value or the default value, depending on what the user entered



### prompt

```lisp
(&optional msg &key default (required-p t) validator if-invalid
 (color *prompt-color*) (error-color *prompt-error-color*))
```

Prompt for a string.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **validator**: (function) A function to use to validate the input. Should return T if the input is valid, or NIL otherwise.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.




### prompt-datetime

```lisp
(&optional msg &key default (required-p t) if-invalid (color *prompt-color*)
 (error-color *prompt-error-color*))
```

Prompts for a timestamp.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.


**Returns**: the parsed local-time

The input is parsed with chronicity library and transformed to a local-time. 
   The input is validated and the process does not stop until the user enters a valid timestamp address.

### prompt-email

```lisp
(&optional msg &key default (required-p t) if-invalid (color *prompt-color*)
 (error-color *prompt-error-color*))
```

Prompts for an email.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.


**Returns**: the entered email

The email is validated and the process does not stop until the user enters a valid email address.

### prompt-integer

```lisp
(&optional msg &key default (required-p t) if-invalid (color *prompt-color*)
 (error-color *prompt-error-color*))
```

Prompts for an integer.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.


**Returns**: the entered number



### prompt-pathname

```lisp
(&optional msg &key default (required-p t) if-invalid (color *prompt-color*)
 (error-color *prompt-error-color*) probe if-exists (if-does-not-exist :error)
 absolute-p file-type directory-p)
```

Prompts for a pathname.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color.
- probe. If T, checks that the file exists on the filesystem.
- **if-exists**: Function to call if the probe is successful.
- **if-does-not-exist**: (keyword) One of:
* :error : Tries again until the pathname can be accessed.
* :warn : Warns the user the pathname could not be accessed and asks for continuing.
* :warn-and-continue: Warns the user the pathname could not be accessed and continues.
* :warn-and-ask-again: Warns the user the pathname could not be accessed and asks for another pathname.





### prompt-url

```lisp
(&optional msg &key default (required-p t) if-invalid (color *prompt-color*)
 (error-color *prompt-error-color*) probe if-exists (if-does-not-exist :error))
```

Prompts for an url.

- **msg**: The prompt.
- **default**: Default value. This is returned if the user enters the empty string. Default: nil.
- **required-p**: (boolean) If T, then the empty string is not allowed as a valid input, and the user is asked again for input. Default: t.
- **if-invalid**: (function) Function to execute if the validator fails.
- **color**: Prompt color
- **error-color**: Prompt error color
- **probe**: (boolean) If T, then url is accessed and verified.
- **if-exists**: (function) A function to call if the url exists (can be accessed).
- **if-does-not-exist**: (keyword) One of:
* :error : Tries again until the url can be accessed.
* :warn : Warns the user the url could not be accessed and asks for continuing.
* :warn-and-continue: Warns the user the url could not be accessed and continues.
* :warn-and-ask-again: Warns the user the url could not be accessed and asks for another url.



**Returns**: the entered url



### say

```lisp
(datum &rest args)
```

Prints a message on the screen.

- **datum**: (string) A format like string.
- **args**: Format arguments or :color, :newline options
- **color**: (keyword) An ansi-text color. One of ansi-colors (.i.e :red, :green, :yellow)
- **newline**: (boolean) If t, forces a newline after printing


A newline is printed iff either newline parameter is T or datum doesn't end with a space. That is, if datum ends in a space, then no newline is printed.

   Example:
 
   ```lisp
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

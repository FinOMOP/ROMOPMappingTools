# LogTibble

Class for managing log messages as a tibble.

## Active bindings

- `logTibble`:

  get as tibble

## Methods

### Public methods

- [`LogTibble$new()`](#method-LogTibble-new)

- [`LogTibble$addLog()`](#method-LogTibble-addLog)

- [`LogTibble$INFO()`](#method-LogTibble-INFO)

- [`LogTibble$WARNING()`](#method-LogTibble-WARNING)

- [`LogTibble$ERROR()`](#method-LogTibble-ERROR)

- [`LogTibble$SUCCESS()`](#method-LogTibble-SUCCESS)

- [`LogTibble$FATAL()`](#method-LogTibble-FATAL)

- [`LogTibble$print()`](#method-LogTibble-print)

- [`LogTibble$clone()`](#method-LogTibble-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a new LogTibble object. addLog

#### Usage

    LogTibble$new()

------------------------------------------------------------------------

### Method `addLog()`

Adds a log message to the log tibble.

#### Usage

    LogTibble$addLog(type, step, message, ...)

#### Arguments

- `type`:

  Type of log message ("INFO", "WARNING", "ERROR", "SUCCESS")

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting INFO

------------------------------------------------------------------------

### Method `INFO()`

Adds an informational log message to the log tibble.

#### Usage

    LogTibble$INFO(step, message, ...)

#### Arguments

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting WARNING

------------------------------------------------------------------------

### Method `WARNING()`

Adds a warning log message to the log tibble.

#### Usage

    LogTibble$WARNING(step, message, ...)

#### Arguments

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting ERROR

------------------------------------------------------------------------

### Method `ERROR()`

Adds an error log message to the log tibble.

#### Usage

    LogTibble$ERROR(step, message, ...)

#### Arguments

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting SUCCESS

------------------------------------------------------------------------

### Method `SUCCESS()`

Adds an error log message to the log tibble.

#### Usage

    LogTibble$SUCCESS(step, message, ...)

#### Arguments

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting FATAL

------------------------------------------------------------------------

### Method `FATAL()`

Adds a fatal log message to the log tibble.

#### Usage

    LogTibble$FATAL(step, message, ...)

#### Arguments

- `step`:

  Step or description associated with the log message

- `message`:

  Log message content

- `...`:

  Additional parameters for message formatting print

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

prints log.

#### Usage

    LogTibble$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogTibble$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

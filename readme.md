# Change the date of a file by an offset (macOS)

## How to use

With this little bash command you can change the modification or creation date of multiple files. Only tested on macOS.

`change-file-datetime [OPTION] OFFSET FILE...`

OPTION are:

- -m offset the modification date (default)
- -c offset the creation date

OFFSET is the offset that should be applied in seconds. To subtract just use negative numbers.

FILE are one or more files where the date should be changed.

EXAMPLE:
`./change-file-datetime -m 3600 file.txt`
In the top case this would add 1h to the modification date.

`./change-file-datetime -c -1800 *.txt`
Subtract 30min of the creation date from all txt files.

## Known bugs

Time zone is set to +0100. This will cause problems in other time zones and with daylight saving. Sorry.

## Develop

`ghci change-file-datetime.hs` to load the program

`> :main -m 3600 file.txt` to run the main function

`> :q` to quit
`> :l` to load a program
`> :r` to reload a program
`> :i` information about a operator

## Compile

`ghc change-file-datetime.hs`

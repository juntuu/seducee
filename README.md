# seducee

A language server for sed. This is a complete hack and very incomplete.


## About the name

I had three requirements for the name:
- It must include `sed`.
- It must be a valid sed program, e.g. `echo duck | sed seducee`.
- It must make sense, i.e. not be just nonsense words.


## Implemented features (MVP)

- Full document formatting
- Renaming labels
- Find references (for labels)
- Goto definition (for labels)
- Highlight the command under cursor
- Hover hints for some functions

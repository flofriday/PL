# Calculator

## ToDo Operations

✔️ done and manually tested
🚧 partially done, partially/not tested
📝 todo

| Operation                                       | Implemented |                                          Note                                           |
|-------------------------------------------------|:-----------:|:---------------------------------------------------------------------------------------:|
| Digit ’0’ to ’9’                                |     ✔️      |                                                                                         |
| Dot ’.’                                         |     ✔️      |                                                                                         |
| ’(’                                             |     ✔️      |                                                                                         |
| ’)’                                             |     ✔️      |                                                                                         |
| Lowercase letter ’a’ to ’z’                     |     ✔️      |                                                                                         |
| Uppercase letter ’A’ to ’Z’                     |     ✔️      |                                                                                         |
| Comparison operation ’=’, ’<’, or ’>’           |     🚧      | does PartialEq from Value implement the epsilone behaviour? maybe some better unittests |
| Arithmetic operation ’+’, ’-’, ’*’, ’/’, or ’%’ |     🚧      |                  associative operation weird behaviour, check comment                   |
| Logic operation                                 |     📝      |                                                                                         |
| Null-Check ’_’                                  |     🚧      |                                       not tested                                        |
| Negation ’~’                                    |     ✔️      |                                                                                         |
| Integer conversion ’?’                          |     ✔️      |                                                                                         |
| Copy ’!’                                        |     ✔️      |                                                                                         |
| Delete ’$’                                      |     📝      |                                                                                         |
| Apply immediately ’@’                           |     📝      |                                                                                         |
| Apply later ’\’’                                |     📝      |                                                                                         |
| Stack size ’#’                                  |     📝      |                                                                                         |
| Read input ’\’’                                 |     📝      |                                                                                         |
| Write output ’"’                                |     🚧      |            This is kinda hard to do without passing the stream to the class             |
| Invalid Char                                    |     📝      |                                                                                         |
| Tests inside registers                          |     📝      |                                                                                         |

Issues
- imprecision of floating point arithmetic bei conversions

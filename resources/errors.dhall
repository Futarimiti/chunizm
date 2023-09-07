{ illegalArgsError = \(arg : Text) -> "Illegal args: ${arg}" 
, arityMismatchError = \(exp : Natural)
                    -> \(act : Natural)
                    -> "Arity mismatch: expected ${Natural/show exp}, got ${Natural/show act}"
, malformedCmdError = "Parse error: malformed command/args"
}

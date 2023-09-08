import re

# Define token types
TOKEN_TYPES = {
    "INTEGER": r"\d+",
    "LPAREN": r"\(",
    "RPAREN": r"\)",
    "IDENTIFIER": r"[a-zA-Z_][a-zA-Z0-9_]*",
    "OPERATOR": r"[+\-*/]",
    "KEYWORD": r"cond|if|else",
    "WHITESPACE": r"\s+",
}

# Combine token regex patterns into a single pattern
TOKEN_PATTERN = "|".join(
    f"(?P<{type}>{pattern})" for type, pattern in TOKEN_TYPES.items()
)


# Tokenize the input source code
def lex(source_code):
    tokens = []
    for match in re.finditer(TOKEN_PATTERN, source_code):
        for token_type, token_value in match.groupdict().items():
            if token_value is not None and token_type != "WHITESPACE":
                tokens.append((token_type, token_value))
                break
    return tokens


# Example usage:
if __name__ == "__main__":
    source_code = "(+ 2 (* 3 4))"
    tokens = lex(source_code)
    for token in tokens:
        print(token)

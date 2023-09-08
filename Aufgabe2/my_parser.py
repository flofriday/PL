from my_lexer import lex


class ASTNode:
    def __str__(self):
        return "ASTNode"

class NumberNode(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"NumberNode({self.value})"

class ListNode(ASTNode):
    def __init__(self, elements):
        self.elements = elements

    def __str__(self):
        elements_str = ", ".join(map(str, self.elements))
        return f"ListNode([{elements_str}])"

class IdentifierNode(ASTNode):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return f"IdentifierNode({self.name})"

class FunctionNode(ASTNode):
    def __init__(self, parameters, body):
        self.parameters = parameters
        self.body = body

    def __str__(self):
        return f"FunctionNode({self.parameters}, {self.body})"


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_token = None

    def error(self, message):
        raise Exception(message)

    def advance(self):
        if self.tokens:
            self.current_token = self.tokens.pop(0)
        else:
            self.current_token = None

    def parse(self):
        self.advance()
        return self.parse_expression()

    def parse_expression(self):
        if self.current_token[0] == "INTEGER":
            return self.parse_integer()
        elif self.current_token[0] == "LPAREN":
            return self.parse_list()
        elif self.current_token[0] == "IDENTIFIER":
            return self.parse_identifier()
        elif self.current_token[0] == "OPERATOR":
            return self.parse_operator()
        else:
            self.error(f"Unexpected token: {self.current_token[1]}")

    def parse_integer(self):
        value = int(self.current_token[1])
        self.advance()
        return NumberNode(value)

    def parse_list(self):
        self.advance()  # Consume '('
        elements = []
        while self.current_token[0] != "RPAREN":
            elements.append(self.parse_expression())
        self.advance()  # Consume ')'
        return ListNode(elements)

    def parse_identifier(self):
        name = self.current_token[1]
        self.advance()
        return IdentifierNode(name)

    # Add a new method to parse operators
    def parse_operator(self):
        operator = self.current_token[1]
        self.advance()
        left_operand = self.parse_expression()
        right_operand = self.parse_expression()
        return ListNode([IdentifierNode(operator), left_operand, right_operand])


# Example usage:
if __name__ == "__main__":
    source_code = "(+ 2 (* 3 4))"
    tokens = lex(source_code)
    parser = Parser(tokens)
    ast = parser.parse()
    print(ast)

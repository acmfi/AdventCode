Definitions.

IDS = [a-z]+
WHITESPACE = [\s\t\n\r]

Rules.

{WHITESPACE}+ : skip_token.
{IDS}        : {token, TokenChars}.

Erlang code.
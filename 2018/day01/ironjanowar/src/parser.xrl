Definitions.

NUMS = [+|-][0-9]+
WHITESPACE = [\s\t\n\r]

Rules.

{WHITESPACE}+ : skip_token.
{NUMS}        : {token, list_to_integer(TokenChars)}.

Erlang code.
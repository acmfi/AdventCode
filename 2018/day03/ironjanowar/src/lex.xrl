Definitions.

WHITESPACE = [\s\t\n\r]
IDS = #[0-9]+
SKIP = [@|\:]
POSITION = [0-9]+,[0-9]+
SIZE = [0-9]+x[0-9]+

Rules.

{WHITESPACE}+ : skip_token.
{SKIP}        : skip_token.
{IDS}         : {token, {id, TokenChars}}.
{POSITION}    : {token, {position, TokenChars}}.
{SIZE}        : {token, {size, TokenChars}}.

Erlang code.
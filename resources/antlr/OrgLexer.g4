lexer grammar OrgLexer;

NEWLINE: '\r'? '\n';
STAR: '*';
LBRACK: '[';
RBRACK: ']';
HASH: '#';
END_DRAWER: ':END:';
COLON: ':';
DASH: '-';
PLUS: '+';
BAR: '|';
UNDERSCORE: '_';
SPACE: [ \t];
COMMENT: 'COMMENT';
UPPER: [A-Z];
LOWER: [a-z];
TEXT_CHAR: ~[\r\n];

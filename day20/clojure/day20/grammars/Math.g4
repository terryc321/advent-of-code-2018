

// Grammar for a simple calculator

grammar Day20;

// Rule for an expression
expr: expr op=('*'|'/') expr  // Multiplication and division
    | expr op=('+'|'-') expr  // Addition and subtraction
    | INT                     // Integer
    | '(' expr ')'            // Parentheses
    | ID                      // Variable identifier
    ;

// Token definitions
INT: [0-9]+;
ID: [a-zA-Z]+;

// Operators
OP: '*' | '/' | '+' | '-';

// Ignore whitespace
WS: [ \t\r\n]+ -> skip;

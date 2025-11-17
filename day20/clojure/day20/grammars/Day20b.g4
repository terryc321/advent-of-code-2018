

// Grammar for a simple calculator

grammar Day20b;

// Rule for an expression
expr: expr '|' expr * // Alternation
    | '(' expr ')'       // Parentheses
    | expr expr 
    | ID                 // Variable identifier
    ;

// Token definitions
ID: 'N' | 'S' | 'E' | 'W';

// Operators
OP: '*' | '/' | '+' | '-';

// Ignore whitespace
WS: [ \t\r\n]+ -> skip;
